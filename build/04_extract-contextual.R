library(tidyverse)
library(data.table)
library(haven)
library(glue)
library(fastLink)
library(stringdist)

#' take a caseID - candidate key and melt to a df keyed on caseID _and_ candidate
#' 
#' @param tbl the wide dataset
#' @param measure_regex the office-party columns to  melt
#' 
#' @return a dataset with usually 2 x nrow(tbl) rows, or 3 if we record 3 options for office
melt_cand <- function(tbl, measure_regex, ids = carry_vars) {
  melt(as.data.table(tbl),
       id.vars = ids,
       measure.vars = patterns(measure_regex),
       variable.name = "cand",
       value.name = c("name", "party"),
       variable.factor = FALSE) %>%
    filter(cand != "") %>%
    mutate(cand = as.integer(cand)) %>%
    mutate(name_caps = str_to_upper(str_replace(name, ", (MD|M\\.D\\.|Jr\\.|Sr\\.)$", "")),
           namelast = word(name_caps, -1),
           namefirstm = word(name_caps, start = 1, end = -2)) %>%
    tbl_df()
}

#' unique incumbent match
#' 
#' If congsession + districts are not unique identifiers (like senators or midway retirements)
#' then merge in last name
#' 
#' @param tbl the dataset of respondents
#' @param key a dataset of incumbents (NOMINATE)
#' @param var the variable in tbl to look at, which amounts to the office
#' 
match_MC <- function(tbl, key, var, carry_vars) {
  
  # variables that define a constituency 
  # mcs that are unique and not unique wrt district
  if (var %in% c("sen1", "sen2")) mc_counts <- key %>% group_by(congress, chamber, st) %>% tally()
  if (var == "hou") mc_counts <- key %>% group_by(congress, chamber, st, dist) %>% tally()
  
  key_uniq <- semi_join(key, filter(mc_counts, n == 1))
  key_notu <- semi_join(key, filter(mc_counts, n != 1))
  
  # vars to match on   
  if (var %in% c("sen1", "sen2")) match_vars <- c("cong_inc" = "congress", "st")
  if (var == "hou") match_vars <- c("cong_inc" = "congress", "st", "cdid" = "dist")
  
  # match on district
  uniq_matched1 <- inner_join(tbl, key_uniq, by = match_vars)
  persn_unmatch <- anti_join(tbl, key_uniq, by = match_vars)
  
  # for second round, extrat last name
  persn_remain <- persn_unmatch %>% 
    mutate(namelast = gsub(", (MD|M\\.D\\.|Jr\\.|Sr\\.)$", "", .data[[glue("{var}_inc")]]),
           namelast = word(namelast, -1),
           namelast = toupper(namelast))
  
  # vars to match on round2
  if (var %in% c("sen1", "sen2")) match_vars <- c("cong_inc" = "congress", "st", "namelast")
  if (var == "hou") match_vars <- c("cong_inc" = "congress", "st", "cdid" = "dist", "namelast")
  
  # coerce key unique to lastname (check FEC to dedupe)
  key_notu_dedup <- distinct(key_notu, congress, st, cdid, namelast, .keep_all = TRUE)
  
  uniq_matched2 <- inner_join(persn_remain, key_notu_dedup, by = match_vars)
  persn_unmatch2 <- anti_join(persn_remain, key_notu_dedup, by = match_vars)
  
  # check match rows
  stopifnot(nrow(persn_unmatch2) + nrow(uniq_matched2) + nrow(uniq_matched1) == nrow(tbl))
  
  bind_rows(uniq_matched1, uniq_matched2, persn_unmatch2) %>% 
    arrange(year, caseID) %>% 
    select(!!carry_vars, icpsr, fec)
}


#' match cces district + name + party with FEC candidates
#' @param res a long dataset with a CCES identifiers + name and party. name should be all caps and have arguments namelast, namemf
#' @param fec a FEC database to search
#' @param stringdist_thresh the maximum distance for which a pair is a match. 
#' We use JW distance for lastname and firstm, so total distance ranges 0 - 2
#' @return A df with same number of rows as `res`, with FEC info appended.
match_fec <- function(res, fec, stringdist_thresh = 0.2) {
  
  type <- unique(fec$office_sought)
  
  fec_rename <- fec %>% 
    rename(name_fec = name, namefirst_fec = namefirst) %>% # to avoid name conflict
    rename(cdid_up = dist, year = cycle)
  
  # coerce FEC unique wrt district-party-lastname (most duplicates is actually the same person with different accounts)
  fec_counts <- fec_rename %>%
    group_by(year, st, cdid_up, party, namelast) %>% 
    summarize(n = n())
  
  key_uniq <- semi_join(fec_rename, filter(fec_counts, n == 1), by = c("year", "party", "st", "cdid_up", "namelast"))
  key_notu <- semi_join(fec_rename, filter(fec_counts, n != 1), by = c("year", "party", "st", "cdid_up", "namelast"))
  
  if (type == "federal:house") 
    matchvars <- c("year", "st", "cdid_up", "party", "namelast")
  if (type %in% c("state:governor", "federal:senate")) 
    matchvars <- c("year", "st", "party", "namelast")
  
  # separate out candidate keys with no candidate
  res_nocand <- filter(res, is.na(name))
  res_somecand <- filter(res, !is.na(name))
  
  # exact match on lastname
  rf_exact <- inner_join(res_somecand, key_uniq, by =  matchvars)
  r_exact_unmatched <- anti_join(res_somecand, key_uniq, by = matchvars)
  f_exact_unmatched <- anti_join(key_uniq, res_somecand, by = matchvars)
  
  
  n_with_info <- nrow(res_somecand)
  n_matched <- nrow(filter(rf_exact, !is.na(fec)))
  cat(glue("out of {n_with_info} rows with CCES candidate info, we merged
           {n_matched} ({round(100*n_matched/n_with_info)} percent) to a FEC key. "), "\n")
  
  
  
  # stringdistance match within district
  cells <- group_by_(r_exact_unmatched, .dots = setdiff(matchvars, "namelast")) %>% 
    summarize(n = n()) %>% ungroup()
  f_consider <- bind_rows(key_notu, f_exact_unmatched)
  
  r_stringdist <- foreach(index = 1:nrow(cells), .combine = "bind_rows") %do% {
    if (index == 1) cat(" starting string distance matching\n")
    if (index %% 100 == 0) cat(glue(" ... {index} out of {nrow(cells)} districts completed"), "\n")
    
    stringdist_left_join(i = index, 
                         type0 = type, 
                         cdata = cells, 
                         rdata = r_exact_unmatched, 
                         fdata = f_consider, 
                         thresh = stringdist_thresh)
    
  }
  
  n_try_string <- nrow(r_exact_unmatched)
  n_string_matched <- nrow(filter(r_stringdist, !is.na(fec)))
  cat(glue("out of {n_try_string} rows that didn't match on first try, we merged
           {n_string_matched} ({round(100*n_string_matched/n_try_string)} percent) to a FEC key. "), "\n")
  
  
  stopifnot(nrow(rf_exact) + nrow(r_stringdist) + nrow(res_nocand) == nrow(res)) ## check no dupes
  
  bind_rows(rf_exact, r_stringdist, res_nocand)
}

#' fuzzy merge
#' @param i the index of cdata to look for 
#' @param type0 the office
#' @param cdata the cell data that is keyed to district-party
#' @param rdata responden-side data
#' @param fdata fec level data
#' @param thresh the string distance threshold
#' 
#' @return a dataset with cdata$n[i] rows with FEC data merged if applicable
#' 
stringdist_left_join <- function(i, type0, cdata, rdata, fdata, thresh) {
  
  r_consider_i <- filter(rdata, year == cdata$year[i], st == cdata$st[i], party == cdata$party[i])
  f_consider_i <- filter(fdata, year == cdata$year[i], st == cdata$st[i], party == cdata$party[i])
  
  
  # further subset by district
  if (type0 == "federal:house") {
    r_consider_i <- filter(r_consider_i, cdid_up == cdata$cdid_up[i])
    f_consider_i <- filter(f_consider_i, cdid_up == cdata$cdid_up[i])
  } 
  stopifnot(cdata$n[i] == nrow(r_consider_i))
  if (nrow(f_consider_i) == 0) return(r_consider_i) # if there is no one FEC to consider, return
  
  # get to the candidate level, not the respondennt level
  r_i_uniq <- distinct(r_consider_i, year, st, cdid_up, party, namelast, namefirstm)
  set.seed(02138)
  f_i_shuffled <- sample_frac(f_consider_i)
  
  match_last  <- stringdistmatrix(r_i_uniq$namelast, f_i_shuffled$namelast, method = "jw")
  match_first <- stringdistmatrix(r_i_uniq$namefirstm, f_i_shuffled$namefirstm, method = "jw")
  
  # for each row in r_i 
  match_result <- foreach(i_r = 1:nrow(r_i_uniq), .combine = "bind_rows") %do% {
    dists <- match_last[i_r, ] + match_first[i_r, ]
    sort_dists <- order(dists)
    is_match <- dists[sort_dists[1]] < thresh
    
    
    if (!is_match) r_result <- slice(r_i_uniq, i_r) # if we call it a no matchreturn without anything
    if (is_match) {
      r_result <- bind_cols(slice(r_i_uniq, i_r),
                            slice(f_i_shuffled, sort_dists[1]) %>% select(-year, -st, -cdid_up, -party, -namelast))
    }
    r_result
  }
  
  left_join(r_consider_i, match_result, by = c(matchvars, "namelast", "namefirstm"))
}


# Variable Key ---

# each row is a variable for the starndardized data, each column is for the cces year0
master <- 
  tribble(~name,      ~`2006`, ~`2007`,     ~`2008`, ~`2009`, ~`2010`, ~`2011`, ~`2012`,            ~`2013`,            ~`2014`,            ~`2015`,            ~`2016`,
          "gov_inc",  "v5019", "govname",    "V508",  "v608",  "V529",  "V529", "CurrentGovName",   "CurrentGovName",   "CurrentGovName",   "CurrentGovName",   "CurrentGovName",
          "hou_inc",  "v5013", "repname",    "V527",  "v627",  "V501",  "V501", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName",
          "sen1_inc", "v5015", "sen1name",   "V551",  "v651",  "V513",  "V513", "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",
          "sen2_inc", "v5017", "sen2name",   "V552",  "v652",  "V521",  "V521", "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",
          "gov_can1", "v5009", NA,           "V501",  NA,      "V564",  NA,     "GovCand1Name",     NA,                 "GovCand1Name",     NA,                 "GovCand1Name",
          "gov_pty1", "v5010", NA,            NA,     NA,      NA,      NA,     "GovCand1Party",    NA,                 "GovCand1Party",    NA,                 "GovCand1Party",
          "gov_can2", "v5011", NA,           "V502",  NA,      "V567",  NA,     "GovCand2Name",     NA,                 "GovCand2Name",     NA,                 "GovCand2Name",
          "gov_pty2", "v5012", NA,            NA,     NA,      NA,      NA,     "GovCand2Party",    NA,                 "GovCand2Party",    NA,                 "GovCand2Party",
          "hou_can1", "v5001", NA,           "V518",  NA,      "V533",  NA,     "HouseCand1Name",   NA,                 "HouseCand1Name",   NA,                 "HouseCand1Name",
          "hou_pty1", "v5002", NA,            NA,     NA,      NA,      NA,     "HouseCand1Party",  NA,                 "HouseCand1Party",  NA,                 "HouseCand1Party",
          "hou_can2", "v5003", NA,           "V519",  NA,      "V536",  NA,     "HouseCand2Name",   NA,                 "HouseCand2Name",   NA,                 "HouseCand2Name",
          "hou_pty2", "v5004", NA,            NA,     NA,      NA,      NA,     "HouseCand2Party",  NA,                 "HouseCand2Party",  NA,                 "HouseCand2Party",
          "sen_can1", "v5005", NA,           "V553",  NA,      "V548",  NA,     "SenCand1Name",     NA,                 "SenCand1Name",     NA,                 "SenCand1Name",
          "sen_pty1", "v5006", NA,            NA,     NA,      NA,      NA,     "SenCand1Party",    NA,                 "SenCand1Party",    NA,                 "SenCand1Party",
          "sen_can2", "v5007", NA,           "V555",  NA,      "V551",  NA,     "SenCand2Name",     NA,                 "SenCand2Name",     NA,                 "SenCand2Name",
          "sen_pty2", "v5008", NA,            NA,     NA,      NA,      NA,     "SenCand2Party",    NA,                 "SenCand2Party",    NA,                 "SenCand2Party"
  )


# 2008, 2009, 2010, 2011 takes D and R so no party column. but note there is an "other party candidate for 2008, 2010

# Data ----
load("data/output/01_responses/common_all.RData")
feckey <- readRDS("data/output/03_contextual/fec_fmt.Rds")
inc_H <- readRDS("data/output/03_contextual/incumbents_H.Rds")
inc_S <- readRDS("data/output/03_contextual/incumbents_S.Rds")
statecode <- read_csv("data/source/statecode.csv")


cclist <- list(`2006` = cc06, 
               `2007` = cc07, 
               `2008` = cc08, 
               `2009` = cc09, 
               `2010` = cc10, 
               `2011` = cc11, 
               `2012` = cc12, 
               `2013` = cc13,
               `2014` = cc14,
               `2015` = cc15, 
               `2016` = cc16)


# Rename variables ----

for (yr in 2006:2016) {
  for (var in master$name) {
    
    # lookup this var
    rename_from <- filter(master, name == var) %>% 
      pull(!!as.character(yr))
    
    # if it shouldn't exist, ensure it doesn't exist
    if (is.na(rename_from)) {
      cclist[[as.character(yr)]] <- cclist[[as.character(yr)]] %>% 
        mutate(!!var := NULL)
    }
    
    # if it should exist, rename
    if (!is.na(rename_from)) {
      cclist[[as.character(yr)]] <- cclist[[as.character(yr)]] %>% 
        rename(!!var := !!rename_from)
    }
  }
}


# bind ------
carry_vars <- c("year", "caseID", "state", "st", "cdid", "cdid_up")



clean_out <- function(tbl) {
  tbl %>% 
    mutate_if(is.character, function(x) replace(x, x == "__NA__" | x == "", NA)) %>% # make NA if empty or "_NA_" 
    select(!!c(carry_vars, intersect(master$name, colnames(tbl)))) %>% 
    mutate_if(is.labelled, function(x) as.character(as_factor(x)))
}

# bind
dfcc <- map_dfr(cclist, clean_out)

# hou_can1 = , # 2010 vote, D/R
# gov_inc = CurrentGovName) # NJ and VA Gov
# gov_inc = CurrentGovName, # KY, LA, MS Gov


# derive more vars -------
df <- dfcc %>% 
  mutate(cong_inc = as.integer(ceiling((year - 1788)/2)),
         cong_up = cong_inc + 1L,
         cdid = replace(cdid, cdid == 0, 1L), # At-LARGE is 1
         cdid_up = replace(cdid_up, cdid_up == 0, 1L))


# add D/R if in 2008, 2010 ----- 

assign_08_10_pty <- function(vec, yrvec, candvec, pty) {
  replace(vec, yrvec %in% c(2008:2011) & !is.na(candvec), pty)
}

df <- df %>% 
  mutate(gov_pty1 = assign_08_10_pty(gov_pty1, year, gov_can1, "D"),
         hou_pty1 = assign_08_10_pty(hou_pty1, year, hou_can1, "D"),
         sen_pty1 = assign_08_10_pty(sen_pty1, year, sen_can1, "D"),
         gov_pty2 = assign_08_10_pty(gov_pty2, year, gov_can2, "R"),
         hou_pty2 = assign_08_10_pty(hou_pty2, year, hou_can2, "R"),
         sen_pty2 = assign_08_10_pty(sen_pty2, year, sen_can2, "R")
         )

# standardize to D/R -----
df <- df %>%
  mutate_at(vars(matches("_pty")), function(vec) gsub("Democrat.*", "D", gsub("Repub.*", "R", vec)))


# long cand-party df -----
hc_key <- melt_cand(df, c("hou_can", "hou_pty"), carry_vars)
sc_key <- melt_cand(df, c("sen_can", "sen_pty"), carry_vars)
gc_key <- melt_cand(df, c("gov_can", "gov_pty"), carry_vars)


  
# Lautenberg 2016 NJ sen
  
  
# create key of candidates -------
fec_hou <- filter(feckey, office_sought == "federal:house", cycle %in% 2006:2016)
fec_sen <- filter(feckey, office_sought == "federal:senate", cycle %in% 2006:2016)
fec_gov <- filter(feckey, office_sought == "state:governor", cycle %in% 2006:2016)




# do the match
hc_fec_match <- match_fec(hc_key, fec_hou)
sc_fec_match <- match_fec(sc_key, fec_sen)
gc_fec_match <- match_fec(gc_key, fec_gov)
  
  



# create key of incumbents----

# Incumbents, by CCES variable (not by respondent -- so key sen1 and sen2 separate)
hi_mc_match  <- match_MC(df, inc_H, "hou", carry_vars)
s1i_mc_match <- match_MC(df, inc_S, "sen1", carry_vars)
s2i_mc_match <- match_MC(df, inc_S, "sen2", carry_vars)




save(hi_mc_match, s1i_mc_match, s2i_mc_match, file = "data/output/01_responses/incumbents_key.RData")
save(hc_fec_match, sc_fec_match, gc_fec_match, file = "data/output/01_responses/candidates_key.RData")
saveRDS(df, "data/output/01_responses/repsondent_contextual.Rds")

