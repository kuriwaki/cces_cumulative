library(tidyverse)
library(data.table)
library(haven)
library(glue)
library(stringdist)
library(foreach)

#' take a case_id - candidate key and melt to a df keyed on case_id _and_ 
#' candidate
#' 
#' @param tbl the wide dataset
#' @param measure_regex the office-party columns to  melt
#' @param ids thethe identifying variables to keep
#' @param remove_regex regex to remove from the names -- suffix that would hinder 
#' the last name extraction
#' 
#' @return a dataset with usually 2 x nrow(tbl) rows, or 3 if we record 3 options for office
melt_cand <- function(tbl, measure_regex, ids = carry_vars, 
           remove_regex = suffixes) {
  office <- unique(str_extract(measure_regex, "(rep|sen|gov)"))
  
  tbl %>% 
    select(!!ids, matches(measure_regex[1]), matches(measure_regex[2])) %>% 
    pivot_longer(
      cols = -c(ids),
      names_to = c(".value", "cand"),
      names_pattern = glue("{office}_(pty|can)([1-9])")
    ) %>% 
    rename(name = can, party = pty) %>%
    filter(cand != "") %>%
    mutate(cand = as.integer(cand)) %>%
    mutate(name_caps = str_to_upper(gsub(remove_regex , "", name)),
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
match_MC <- function(tbl, key, var, ids = carry_vars, remove_regex = suffixes) {
  
  # variables that define a constituency 
  # mcs that are unique and not unique wrt district
  if (var %in% c("sen1", "sen2")) {
    mc_counts <- key %>% group_by(congress, chamber, st) %>% tally()
    tbl <- tbl %>% filter(st != "DC")
  }
  if (var == "rep") mc_counts <- key %>% group_by(congress, chamber, st, dist) %>% tally()
  
  key_uniq <- semi_join(key, filter(mc_counts, n == 1))
  key_notu <- semi_join(key, filter(mc_counts, n != 1))
  
  # vars to match on   
  if (var %in% c("sen1", "sen2")) match_vars <- c("cong" = "congress", "st")
  if (var == "rep") match_vars <- c("cong" = "congress", "st", "dist")
  
  # match on district
  uniq_matched1 <- inner_join(tbl, key_uniq, by = match_vars)
  persn_unmatch <- anti_join(tbl, key_uniq, by = match_vars)
  
  cat(glue("Out of {nrow(tbl)} incumbent-rows,",
           "{nrow(uniq_matched1)} ({round(100*nrow(uniq_matched1) / nrow(tbl))}",
           "percent) matched uniquely by district"), "\n")
  
  # for second round, extrat last name
  persn_remain <- persn_unmatch %>% 
    mutate(namelast = gsub(remove_regex, "", .data[[glue("{var}_inc")]]),
           namelast = str_remove(namelast, "\\s\\((republican|democrat|independent)\\)"), 
           namelast = word(namelast, -1), # find lastname
           namelast = toupper(namelast)) %>% 
    mutate(namelast = replace(namelast, st == "NV" & namelast == "MASTO", "CORTEZ MASTO"),
           namelast = replace(namelast, st == "MD" & namelast == "HOLLEN", "VAN HOLLEN"),
           namelast = replace(namelast, st == "TX" & namelast == "HUTCHINSON", "HUTCHISON"),
           namelast = replace(namelast, st == "NJ" & namelast == "MENÉNDEZ", "MENENDEZ"),
           namelast = replace(namelast, st == "NY" & namelast == "VELAZQUEZ", "VELÁZQUEZ"))
  
  # vars to match on round2
  if (var %in% c("sen1", "sen2")) match_vars <- c("cong" = "congress", "st", "namelast")
  if (var == "rep") match_vars <- c("cong" = "congress", "st", "dist" = "dist", "namelast")
  
  # coerce key unique to lastname (check FEC to dedupe)
  key_notu_dedup <- distinct(key_notu, congress, st, dist, namelast, .keep_all = TRUE)
  
  uniq_matched2 <- inner_join(persn_remain, key_notu_dedup, by = match_vars)
  persn_unmatch2 <- anti_join(persn_remain, key_notu_dedup, by = match_vars)
  
  
  cat(glue("Out of {nrow(persn_remain)} incumbent-rows that didn't match on first try,",
           "{nrow(uniq_matched2)} matched uniquely by district-lastname", 
           "(match rate up to ",
           "{round(100*(nrow(uniq_matched1) + nrow(uniq_matched2)) / nrow(tbl))} percent)"), 
      "\n")
  
  # check match rows
  stopifnot(nrow(persn_unmatch2) + nrow(uniq_matched2) + nrow(uniq_matched1) == nrow(tbl))
  
  
  namevar <- glue("{var}_inc")
  ptyvar <- glue("{var}_ipt")
  
  
  bind_rows(uniq_matched1, uniq_matched2, persn_unmatch2) %>% 
    mutate(!!paste0(var, "_current") := concatenate_current(
      namevec = .data[[namevar]], 
      partyvec = .data[[ptyvar]])) %>%
    arrange(year, case_id) %>% 
    select(!!ids,
           matches("_current"),
           icpsr, 
           fec)
}

#' Concatenate name and party to a formatted label.
#' if partyvec is missing, don't use parentheses
concatenate_current <- function(namevec, partyvec) {
  str_c(namevec, 
        ifelse(!is.na(partyvec), " (", ""),
        ifelse(!is.na(partyvec), partyvec, ""),
        ifelse(!is.na(partyvec), ")", ""),
        sep = "")
}


#' match cces district + name + party with FEC candidates
#' @param res a long dataset with a CCES identifiers + name and party. name should 
#' be all caps and have arguments namelast, namemf
#' @param fec a FEC database to search
#' @param stringdist_thresh the maximum distance for which a pair is a match. 
#' We use JW distance for lastname and firstm, so total distance ranges 0 - 2
#' @return A df with same number of rows as `res`, with FEC info appended.
match_fec <- function(res, fec, stringdist_thresh = 0.25) {
  
  type <- unique(fec$office_sought)
  
  fec_rename <- fec %>% 
    rename(name_fec = name, namefirst_fec = namefirst) %>% # to avoid name conflict
    rename(dist_up = dist, year = cycle)
  
  # coerce FEC unique wrt district-party-lastname (most duplicates is actually 
  # the same person with different accounts)
  fec_counts <- fec_rename %>%
    group_by(year, st, dist_up, party, namelast) %>% 
    summarize(n = n())
  
  key_uniq <- semi_join(
    fec_rename, 
    filter(fec_counts, n == 1), by = c("year", "party", "st", "dist_up", "namelast"))
  key_notu <- semi_join(
    fec_rename, 
    filter(fec_counts, n != 1), by = c("year", "party", "st", "dist_up", "namelast"))
  
  if (type == "federal:house") 
    matchvars <- c("year", "st", "dist_up", "party", "namelast")
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
  gvars <- setdiff(matchvars, "namelast")
  cells <- count_(r_exact_unmatched, gvars) # {!!!} doesn't work, not sure why
  f_consider <- bind_rows(key_notu, f_exact_unmatched)
  
  r_stringdist <- foreach(index = 1:nrow(cells), .combine = "bind_rows") %do% {
    if (index == 1) cat(" starting string distance matching\n")
    if (index %% 100 == 0) 
      cat(glue(" ... {index} out of {nrow(cells)} districts completed"), "\n")
    
    stringdist_left_join(i = index, 
                         type0 = type, 
                         cdata = cells, 
                         rdata = r_exact_unmatched, 
                         fdata = f_consider, 
                         matchvar = matchvars,
                         thresh = stringdist_thresh)
    
  }
  
  n_try_string <- nrow(r_exact_unmatched)
  n_string_matched <- nrow(filter(r_stringdist, !is.na(fec)))
  cat(glue("out of {n_try_string} rows that didn't match on first try,",
           "we merged {n_string_matched} (match rate up to ", 
           "{round(100*(n_string_matched + n_matched)/n_with_info)} ",
           "percent) to a FEC key. "), "\n")
  
  
  stopifnot(nrow(rf_exact) + nrow(r_stringdist) + nrow(res_nocand) == nrow(res)) ## check no dupes
  
  bind_rows(rf_exact, r_stringdist, res_nocand)
}

#' fuzzy merge
#' @param i the index of cdata to look for 
#' @param type0 the office
#' @param cdata the cell data that is keyed to district-party
#' @param rdata responden-side data
#' @param fdata fec level data
#' @param matchvar vector to key on in final match
#' @param thresh the string distance threshold
#' 
#' @return a dataset with cdata$n[i] rows with FEC data merged if applicable
#' 
stringdist_left_join <- function(i, type0, cdata, rdata, fdata, matchvar, thresh) {
  
  pty_i <- cdata$party[i]
  
  # If not coded party, expand search in FEC to all others
  if (!is.na(pty_i) & !pty_i %in% c("D", "R", "L", "I")) {
    r_consider_i <- filter(rdata, year == cdata$year[i], st == cdata$st[i], party == pty_i)
    f_consider_i <- filter(fdata, year == cdata$year[i], st == cdata$st[i], party == "NA/Other")
  } 
  
  # If no party, use the appropriate rows (b/c R wont accept party == NA). And consider everyone in district
  if (is.na(pty_i)) {
    r_consider_i <- filter(rdata, year == cdata$year[i], st == cdata$st[i], is.na(party))
    f_consider_i <- filter(fdata, year == cdata$year[i], st == cdata$st[i])
  }
  
  # Usually there's no trouble
  if (!is.na(pty_i) & pty_i %in% c("D", "R", "L", "I")) {
    r_consider_i <- filter(rdata, year == cdata$year[i], st == cdata$st[i], party == pty_i)
    f_consider_i <- filter(fdata, year == cdata$year[i], st == cdata$st[i], party == pty_i)
  }
  
  # further subset by district
  if (type0 == "federal:house") {
    r_consider_i <- filter(r_consider_i, dist_up == cdata$dist_up[i])
    f_consider_i <- filter(f_consider_i, dist_up == cdata$dist_up[i])
  } 
  stopifnot(cdata$n[i] == nrow(r_consider_i))
  if (nrow(f_consider_i) == 0) return(r_consider_i) # if there is no one FEC to consider, return
  
  # get to the candidate level, not the respondent level
  r_i_uniq <- distinct(r_consider_i, year, st, dist_up, party, namelast, namefirstm)
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
                            slice(f_i_shuffled, sort_dists[1]) %>% 
                              select(-year, -st, -dist_up, -party, -namelast, -namefirstm))
    }
    r_result
  }
  
  left_join(r_consider_i, 
            match_result, 
            by = c("year", "st", "dist_up", "party", "namelast", "namefirstm"))
}

#' Remove NAs, change labelled (from the dta) to factors (better for R)
#' @param tbl A dataset of respodents
clean_out <- function(tbl, cvars = carry_vars, m = master) {
  tbl %>% 
    # make NA if empty or "_NA_" 
    mutate_if(is.character, function(x) replace(x, x == "__NA__" | x == "", NA)) %>% 
    # the carry_vars specified and any vars in master
    select(!!c(cvars, intersect(m$name, colnames(tbl)))) %>% 
    mutate_if(is.labelled, function(x) as.character(as_factor(x)))
}


#' Standardize party label especially collapse duplicates of common spellings.
#' @param vec A string vector
std_ptylabel <- function(vec) {
  dplyr::recode(vec,
                `Republican` = "R",
                `Democratic` = "D", 
                `R` = "R",                      
                `D` = "D",
                `Democrat` = "D",
                `Libertarian` = "L",
                `Independent` = "I",
                `Green` = "G",
                `Green Party` = "G",
                `Constitution` = "Constitution",
                `Constitutional` = "Constitution",
                `Conservative` = "Conservative",
                `Conservative Party` = "Conservative")
}

# Variable Key ------


# 2008, 2009, 2010, 2011 takes D and R so no party column. but note there is an 
# "other party candidate for 2008, 2010

# Data ------
load("data/output/01_responses/common_all.RData")
inc_H <- readRDS("data/output/03_contextual/incumbents_H.Rds")
inc_S <- readRDS("data/output/03_contextual/incumbents_S.Rds")
statecode <- read_csv("data/source/statecode.csv")

# parameters
# remove generations, MDs, Jr/Srs.
suffixes <- "(,?\\sIV|,?\\sI{1,3}|,?\\sM\\.?D\\.?|,?\\sJr\\.|,?\\sSr\\.)$" 
# carry these along as id vectors 
carry_vars <- c("year", "case_id", "state", "st", "dist", "dist_up", "cong", "cong_up") 

cclist <- list(`2006` = cc06, 
               # `2006m` = mit06_add,
               `2007` = cc07, 
               `2008` = cc08, 
               `2008h` = hu08, 
               `2009` = cc09, 
               `2009r` = hu09,
               `2010` = cc10, 
               `2011` = cc11, 
               `2012` = cc12, 
               `2012p` = panel12,
               `2013` = cc13,
               `2014` = cc14,
               `2015` = cc15, 
               `2016` = cc16,
               `2017` = cc17,
               `2018` = cc18)
               # `2018a` = hua18,
               # `2018b` = hub18)s


# Rename variables ----
master <- readRDS("data/output/02_questions/variable_std_key.Rds")
master$`2008h` <- master$`2008`
master$`2009r` <- master$`2009`
master$`2012p` <- master$`2012`
master$`2018a` <- master$`2018`
master$`2018b` <- master$`2018`

for (yr in c(2006:2018,  "2008h", "2009r","2012p")) { # "2006m",
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

dfcc <- map_dfr(cclist, clean_out, carry_vars, master)

# hou_can1 = , # 2010 vote, D/R
# gov_inc = CurrentGovName) # NJ and VA Gov
# gov_inc = CurrentGovName, # KY, LA, MS Gov



# standardize party label add (without checking) D/R if in 2008, 2010 ----- 

assign_08_10_pty <- function(vec, yrvec, candvec, pty) {
  replace(vec, yrvec %in% c(2008:2011) & !is.na(candvec), pty)
}

df_current <- dfcc %>% 
  mutate(gov_pty1 = assign_08_10_pty(gov_pty1, year, gov_can1, "D"),
         rep_pty1 = assign_08_10_pty(rep_pty1, year, rep_can1, "D"),
         sen_pty1 = assign_08_10_pty(sen_pty1, year, sen_can1, "D"),
         gov_pty2 = assign_08_10_pty(gov_pty2, year, gov_can2, "R"),
         rep_pty2 = assign_08_10_pty(rep_pty2, year, rep_can2, "R"),
         sen_pty2 = assign_08_10_pty(sen_pty2, year, sen_can2, "R")
         ) %>%
  mutate_at(vars(matches("(_pty|_ipt)")), std_ptylabel)


# wide to long cand-party df -----
rc_key <- melt_cand(df_current, c("rep_can", "rep_pty"), carry_vars)
sc_key <- melt_cand(df_current, c("sen_can", "sen_pty"), carry_vars)
gc_key <- melt_cand(df_current, c("gov_can", "gov_pty"), carry_vars)


# create key of incumbent MC ----
# Incumbents, by CCES variable (not by respondent -- so key sen1 and sen2 separate)
ri_mc_match  <- match_MC(df_current, inc_H, "rep", carry_vars)
s1i_mc_match <- match_MC(df_current, inc_S, "sen1", carry_vars)
s2i_mc_match <- match_MC(df_current, inc_S, "sen2", carry_vars)




# Save ---------
save(ri_mc_match, s1i_mc_match, s2i_mc_match, 
     file = "data/output/01_responses/incumbents_key.RData")
save(rc_key, sc_key, gc_key, 
     file = "data/output/01_responses/candidates_key.RData")
saveRDS(df, "data/output/01_responses/repsondent_contextual.Rds")


cat("Finished matching candidate info to identifiers\n")