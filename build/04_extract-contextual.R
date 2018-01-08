library(tidyverse)
library(data.table)

#' take a caseID - candidate key and melt to a df keyed on caseID _and_ candidate
melt_cand <- function(tbl, measure_regex, ids = carry_vars) {
  melt(as.data.table(tbl),
       id.vars = ids,
       measure.vars = patterns(measure_regex),
       variable.name = "cand",
       value.name = c("name", "party"),
       variable.factor = FALSE) %>%
    subset(cand != "") %>%
    mutate(cand = as.integer(cand)) %>%
    tbl_df()
}

#' unique incumbent match
#' If congsession + districts are not unique identifiers (like senators or midway retirements)
#' then merge in last name
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


# Variable Key ---

# each row is a variable for the starndardized data, each column is for the cces year0
master <- 
  tribble(~name,      ~`2006`, ~`2007`,     ~`2008`, ~`2009`, ~`2010`, ~`2011`, ~`2012`,            ~`2013`,            ~`2014`,            ~`2015`,            ~`2016`,
          "gov_inc",  "v5019", "govname",    "V508",  "v608",  "V529",  "V529", "CurrentGovName",   "CurrentGovName",   "CurrentGovName",   "CurrentGovName",   "CurrentGovName",
          "hou_inc",  "v5013", "repname",    "V527",  "v627",  "V501",  "V501", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName",
          "sen1_inc", "v5015", "sen1name",   "V551",  "v651",  "V513",  "V513", "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",
          "sen2_inc", "v5017", "sen2name",   "V552",  "v652",  "V521",  "V521", "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",
          "gov_can1", "v5009", NA,           "V501",  NA,      "V564",  NA,     "GovCand1Name",     NA,                 "GovCand1Name",     NA,                 "GovCand1Name",
          "gov_pty1", "v5010", NA,            NA,     NA,       NA,     NA,     "GovCand1Party",    NA,                 "GovCand1Party",    NA,                 "GovCand1Party",
          "gov_can2", "v5011", NA,           "V502",  NA,      "V567",  NA,     "GovCand2Name",     NA,                 "GovCand2Name",     NA,                 "GovCand2Name",
          "gov_pty2", "v5012", NA,            NA,     NA,       NA,     NA,     "GovCand2Party",    NA,                 "GovCand2Party",    NA,                 "GovCand2Party",
          "hou_can1", "v5001", NA,           "V518",  "v618",  "V533",  "V533", "HouseCand1Name",   NA,                 "HouseCand1Name",   NA,                 "HouseCand1Name",
          "hou_pty1", "v5002", NA,            NA,     NA,       NA,     NA,     "HouseCand1Party",  NA,                 "HouseCand1Party",  NA,                 "HouseCand1Party",
          "hou_can2", "v5003", NA,           "V519",  "v619",  "V536",  "V536", "HouseCand2Name",   NA,                 "HouseCand2Name",   NA,                 "HouseCand2Name",
          "hou_pty2", "v5004", NA,            NA,     NA,       NA,     NA,     "HouseCand2Party",  NA,                 "HouseCand2Party",  NA,                 "HouseCand2Party",
          "sen_can1", "v5005", NA,           "V553",  "v653",  "V548",  NA,     "SenCand1Name",     NA,                 "SenCand1Name",     NA,                 "SenCand1Name",
          "sen_pty1", "v5006", NA,            NA,     NA,       NA,     NA,     "SenCand1Party",    NA,                 "SenCand1Party",    NA,                 "SenCand1Party",
          "sen_can2", "v5007", NA,           "V555",  "v655",  "V551",  NA,     "SenCand2Name",     NA,                 "SenCand2Name",     NA,                 "SenCand2Name",
          "sen_pty2", "v5008", NA,            NA,     NA,       NA,     NA,     "SenCand2Party",    NA,                 "SenCand2Party",    NA,                 "SenCand2Party"
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

clean_out <- function(tbl) {
  tbl %>% 
    mutate_if(is.character, function(x) replace(x, x == "__NA__" | x == "", NA)) %>% # make NA if empty or "_NA_" 
    select(!!c("year", "caseID", "state", "st", "cdid", intersect(master$name, colnames(tbl)))) %>% 
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
         cdid = replace(cdid, cdid == 0, 1L)) # At-LARGE is 1


# add D/R if in 2008, 2010 ----- 

assign_08_10_pty <- function(vec, yrvec, candvec, pty) {
  replace(vec, yrvec %in% c(2008:2011) & candvec != "", pty)
}

df <- df %>% 
  mutate(gov_pty1 = assign_08_10_pty(gov_pty1, year, gov_can1, "D"),
         hou_pty1 = assign_08_10_pty(hou_pty1, year, hou_can1, "D"),
         sen_pty1 = assign_08_10_pty(sen_pty1, year, sen_can1, "D"),
         gov_pty2 = assign_08_10_pty(gov_pty2, year, gov_can2, "R"),
         hou_pty2 = assign_08_10_pty(hou_pty2, year, hou_can1, "R"),
         sen_pty2 = assign_08_10_pty(sen_pty2, year, sen_can2, "R")
         )

# standardize to D/R -----
df <- df %>%
  mutate_at(vars(matches("_pty")), function(vec) gsub("Democrat.*", "D", gsub("Repub.*", "R", vec)))


# long cand-party df --

carry_vars <- c("year", "caseID", "state", "st", "cdid")

hc_key <- melt_cand(df, c("hou_can", "hou_pty"), carry_vars)
sc_key <- melt_cand(df, c("sen_can", "sen_pty"), carry_vars)
gc_key <- melt_cand(df, c("gov_can", "gov_pty"), carry_vars)


i_hou_name <- left_join(i_rep, hc_key, by = c("year", "caseID", "intent_rep_num" = "cand"))
i_sen_name <- left_join(i_sen, sc_key, by = c("year", "caseID", "intent_sen_num" = "cand"))
i_gov_name <- left_join(i_gov, gc_key, by = c("year", "caseID", "intent_gov_num" = "cand"))
  
  
# Lautenberg 2016 NJ sen
  
  
# create key of candidates -------
fec_hou <- filter(feckey, office_sought == "federal:house", elec %in% 2006:2016)



hc_key_name <- hc_key %>% 
    mutate(name = str_to_upper(gsub(", (MD|M\\.D\\.|Jr\\.|Sr\\.)$", "", name)),
           namelast = word(name, -1),
           namefirst = word(name, 1))

fec_hou_uniq <- fec_hou %>% 
  distinct(elec, st, dist, party, name, namelast, .keep_all = TRUE) %>% 
  rename(name_fec = name,
         namefirst_fec = namefirst)

hc_fec <- left_join(hc_key_name, fec_hou_uniq,
                    by = c("year" = "elec",
                           "st",
                           "cdid" = "dist",
                           "party",
                           "namelast"))
  
table(is.na(filter(hc_fec, name != "") %>% pull(fec)))

View(sample_n(hc_fec, 100) %>% arrange(year, st, cdid))

# create key of incumbents----

# Incumbents, by CCES variable (not by respondent -- so key sen1 and sen2 separate)
key_hou_inc  <- match_MC(df, inc_H, "hou", carry_vars)
key_sen1_inc <- match_MC(df, inc_S, "sen1", carry_vars)
key_sen2_inc <- match_MC(df, inc_S, "sen2", carry_vars)




# from 03 -----

# replicate the filler value each respondent chose
showCand <- function(stacked, var, cand_key = cand_key) {
  var <- enquo(var)
  var_name <- quo_name(var)
  
  if (grepl("rep", var_name)) race <- "House"
  if (grepl("sen", var_name)) race <- "Sen"
  if (grepl("gov", var_name)) race <- "Gov"
  
  stacked %>%
    mutate(number = gsub(".*cand([0-9]+)name.*", "\\1", !! var)) %>%
    left_join(cand_key[[race]], by = c("year", "caseID", "number")) %>%
    mutate(!! var_name := paste0(cand, " (", party, ")")) %>%
    select(-number, -cand, -party)
}

# separate out those that need `showCand`, then bidn
sep_bind <- function(tbl, var) {
  var <- enquo(var)
  
  changed <- showCand(filter(tbl, grepl("cand", !! var)), !! var)
  unchanged <- filter(tbl, !grepl("cand", !! var))
  
  bind_rows(changed, unchanged) %>%
    arrange(year, caseID)
}





races <- c("House", "Sen", "Gov")
cand_regex <- c(
  paste0(paste0("^", races, "cand[0-9+]"), "name$"),
  paste0(paste0("^", races, "cand[0-9+]"), "party$")
)


# employ melt_year_reg to 12, 14, 16
cand_key <- foreach(r = races, .combine = "c") %do% {
  measure_regex <- paste0(paste0("^", r, "Cand[0-9+]"), c("Name$", "Party$"))
  key <- list()
  
  year_2012 <- melt_year_reg(cc12, measure_regex)
  year_2014 <- melt_year_reg(cc14, measure_regex)
  year_2016 <- melt_year_reg(cc16, measure_regex)
  
  
  key[[r]] <- bind_rows(year_2012, year_2014, year_2016)
  key
}



# mutate vote variables that are HouseCand fillers
i_rep <- sep_bind(i_rep, intent_rep_char)
i_sen <- sep_bind(i_sen, intent_sen_char)
i_gov <- sep_bind(i_gov, intent_gov_char)

v_rep <- sep_bind(v_rep, voted_rep_char)
v_sen <- sep_bind(v_sen, voted_sen_char)
v_gov <- sep_bind(v_gov, voted_gov_char)
