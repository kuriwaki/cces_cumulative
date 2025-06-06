library(tidyverse)
library(haven)
library(glue)
suppressPackageStartupMessages(library(stringdist))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(scales))
library(dtplyr)
library(cli)

#' take a case_id - candidate key and melt to a df keyed on case_id _and_ 
#' candidate
#' 
#' @param tbl the wide dataset
#' @param measure_regex the office-party columns to  melt
#' @param ids the identifying variables to keep
#' @param remove_regex regex to remove from the names -- suffix that would hinder 
#' the last name extraction
#' 
#' @return a dataset with usually 2 x nrow(tbl) rows, or 3 if we record 3 options for office
melt_cand <- function(tbl, measure_regex, ids = carry_vars, 
           remove_regex = suffixes) {
  office <- unique(str_extract(measure_regex, "(rep|sen|gov)"))
  
  tbl |> 
    select(all_of(ids), matches(measure_regex[1]), matches(measure_regex[2])) |> 
    pivot_longer(
      cols = -all_of(ids),
      names_to = c(".value", "cand"),
      names_pattern = glue("{office}_(pty|can)([1-9])"), 
      values_drop_na = TRUE
    ) |> 
    rename(name = can, party = pty) |>
    filter(cand != "") |>
    mutate(cand = as.integer(cand)) |>
    mutate(name_caps = str_to_upper(gsub(remove_regex , "", name)),
           namelast = word(name_caps, -1))
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
  cli_div(theme = list(span.strong = list(color = "orange")))
  cli_h2("Matching {.code {var}}")
  
  # variables that define a constituency 
  # mcs that are unique and not unique wrt district
  if (var %in% c("sen1", "sen2")) {
    mc_counts <- key |> group_by(congress, chamber, st) |> tally()
    tbl <- tbl |> filter(st != "DC")
  }
  if (var == "rep") mc_counts <- key |> group_by(congress, chamber, st, dist) |> tally()
  
  if (var %in% c("sen1", "sen2")) match_vars <- c("congress", "chamber", "st")
  if (var == "rep") match_vars <- c("congress", "chamber", "st", "dist")
  key_uniq <- semi_join(key, filter(mc_counts, n == 1), by = match_vars)
  key_notu <- semi_join(key, filter(mc_counts, n != 1), by = match_vars)
  
  # vars to match on   
  if (var %in% c("sen1", "sen2")) match_vars <- c("cong" = "congress", "st")
  if (var == "rep") match_vars <- c("cong" = "congress", "st", "dist")
  
  # match on district
  uniq_matched1 <- inner_join(tbl, key_uniq, by = match_vars)
  persn_unmatch <- anti_join(tbl, key_uniq, by = match_vars)
  mr <- percent(nrow(uniq_matched1) / nrow(tbl))
  cli_alert_info("Out of {comma(nrow(tbl))} incumbent-rows, {comma(nrow(uniq_matched1))} ({.strong {mr}}) matched uniquely by district")
  
  # for second round, extract last name
  persn_remain <- persn_unmatch |> 
    mutate(namelast = gsub(remove_regex, "", .data[[glue("{var}_inc")]]),
           namelast = str_remove(namelast, "\\s\\((republican|democrat|independent)\\)"), 
           namelast = word(namelast, -1), # find lastname
           namelast = toupper(namelast)) |> 
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
  
  mr2 <- percent((nrow(uniq_matched1) + nrow(uniq_matched2)) / nrow(tbl))
  cli_alert_info("Out of {comma(nrow(persn_remain))} incumbent-rows that didn't match on first try, {comma(nrow(uniq_matched2))} matched uniquely by district-lastname (match rate up to {.strong {mr2}})")
  
  # check match rows
  stopifnot(nrow(persn_unmatch2) + nrow(uniq_matched2) + nrow(uniq_matched1) == nrow(tbl))
  
  
  namevar <- glue("{var}_inc")
  ptyvar <- glue("{var}_ipt")
  
  
  bind_rows(uniq_matched1, uniq_matched2, persn_unmatch2) |> 
    mutate(!!paste0(var, "_current") := concatenate_current(
      namevec = .data[[namevar]], 
      partyvec = .data[[ptyvar]])) |>
    arrange(year, case_id) |> 
    select(all_of(ids),
           matches("_current"),
           icpsr)
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


#' fuzzy merge
#' @param i the index of cdata to look for 
#' @param type0 the office
#' @param cdata the cell data that is keyed to district-party
#' @param rdata respondent-side data
#' @param matchvar vector to key on in final match
#' @param thresh the string distance threshold
#' 
#' @return a dataset with cdata$n[i] rows
#' 
stringdist_left_join <- function(i, type0, cdata, rdata, matchvar, thresh) {
  
  pty_i <- cdata$party[i]
  
  # If  party not coded, expand searchto all others
  if (!is.na(pty_i) & !pty_i %in% c("D", "R", "L", "I")) {
    r_consider_i <- filter(rdata, year == cdata$year[i], st == cdata$st[i], party == pty_i)
  } 
  
  # If no party, use the appropriate rows (b/c R wont accept party == NA). And consider everyone in district
  if (is.na(pty_i)) {
    r_consider_i <- filter(rdata, year == cdata$year[i], st == cdata$st[i], is.na(party))
  }
  
  # Usually there's no trouble
  if (!is.na(pty_i) & pty_i %in% c("D", "R", "L", "I")) {
    r_consider_i <- filter(rdata, year == cdata$year[i], st == cdata$st[i], party == pty_i)
  }
  
  # further subset by district
  if (type0 == "federal:house") {
    r_consider_i <- filter(r_consider_i, dist_up == cdata$dist_up[i])
  } 
  stopifnot(cdata$n[i] == nrow(r_consider_i))
  
  r_consider_i
}

#' Remove NAs, change labelled (from the dta) to factors (better for R)
#' @param tbl A dataset of respondents
clean_out <- function(tbl, cvars = carry_vars, m = master) {
  tbl |> 
    # make NA if empty or "_NA_" 
    mutate_if(is.character, function(x) replace(x, x == "__NA__" | x == "", NA)) |> 
    # the carry_vars specified and any vars in master
    select(!!c(cvars, intersect(m$name, colnames(tbl)))) |> 
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
cli_h1("Load data")
load("data/output/01_responses/common_all.RData")
inc_H <- read_csv("data/output/03_contextual/voteview_H_key.csv", show_col_types = FALSE)
inc_S <- read_csv("data/output/03_contextual/voteview_S_key.csv", show_col_types = FALSE) |> 
  mutate(dist = NA)
statecode <- read_csv("data/source/statecode.csv", show_col_types = FALSE)

# parameters
# remove generations, MDs, Jr/Srs.
suffixes <- "(,?\\sIV|,?\\sI{1,3}|,?\\sM\\.?D\\.?|,?\\sJr\\.|,?\\sSr\\.)$" 
# carry these along as id vectors 
carry_vars <- c("year", "case_id", "state", "st", "dist", "dist_up", "cong", "cong_up") 

  
# Rename variables ----
master <- readRDS("data/output/02_questions/variable_std_key.Rds")
master$`2008h` <- master$`2008`
master$`2009r` <- master$`2009`
master$`2012p` <- master$`2012`
master$`2018a` <- master$`2018`
master$`2018b` <- master$`2018`
master$`2018c` <- master$`2018`

master$`2010_post` <- str_c(master$`2010`, "_post")
master$`2012_post` <- str_c(master$`2012`, "_post")
master$`2014_post` <- str_c(master$`2014`, "_post")
master$`2016_post` <- str_c(master$`2016`, "_post")
master$`2018_post` <- str_c(master$`2018`, "_post")
master$`2020_post` <- str_c(master$`2020`, "_post")
master$`2022_post` <- str_c(master$`2022`, "_post")
master$`2024_post` <- str_c(master$`2024`, "_post")

# trick functions that it uses post
cli_h1("Rename variables")
blend_post <- function(tbl) {
  mutate(tbl,
         st = st_post,
         dist = dist_post,
         dist_up = dist_up_post,
         cd = cd_post,
         cd_up = cd_up_post) |> 
    filter(!is.na(st) | !is.na(dist) | !is.na(cd))
}

# list to store data with renamed variables
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
               `2018` = cc18,
               `2018c` = cc18_cnew,
               `2019` = cc19,
               `2020` = cc20,
               `2021` = cc21,
               `2022` = cc22,
               `2023` = cc23,
               `2024` = cc24,
               `2010_post` = blend_post(cc10),
               `2012_post` = blend_post(cc12),
               `2014_post` = blend_post(cc14),
               `2016_post` = blend_post(cc16),
               `2018_post` = blend_post(cc18),
               `2020_post` = blend_post(cc20),
               `2022_post` = blend_post(cc22),
               `2024_post` = blend_post(cc24)
               )
# `2018a` = hua18,
# `2018b` = hub18)

for (yr in c(2006:2024, str_c(seq(2010, 2024, 2), "_post"), "2012p", "2018c")) { # "2006m", "2008h", "2009r","2012p"
  for (var in master$name) {
    
    # lookup this var
    rename_from <- filter(master, name == var) |> 
      pull(!!as.character(yr))
    
    # if it shouldn't exist, ensure it doesn't exist
    if (is.na(rename_from)) {
      cclist[[as.character(yr)]][[var]] <- NULL
    }
    
    # if it should exist, rename
    if (!is.na(rename_from)) {
      cclist[[as.character(yr)]] <- cclist[[as.character(yr)]] |> 
        rename({{var}} := all_of(rename_from))
    }
  }
}


# bind ------
dfcc <- map_dfr(cclist, .f = clean_out, cvars = carry_vars, m = master, .id = "dataset")

# gov_inc = CurrentGovName) # NJ and VA Gov
# gov_inc = CurrentGovName, # KY, LA, MS Gov

# standardize party label add (without checking) D/R if in 2008, 2010 ----- 
assign_08_10_pty <- function(vec, yrvec, candvec, pty) {
  replace(vec, yrvec %in% c(2008:2011) & !is.na(candvec), pty)
}

df_current <- dfcc |> 
  mutate(gov_pty1 = assign_08_10_pty(gov_pty1, year, gov_can1, "D"),
         rep_pty1 = assign_08_10_pty(rep_pty1, year, rep_can1, "D"),
         sen_pty1 = assign_08_10_pty(sen_pty1, year, sen_can1, "D"),
         gov_pty2 = assign_08_10_pty(gov_pty2, year, gov_can2, "R"),
         rep_pty2 = assign_08_10_pty(rep_pty2, year, rep_can2, "R"),
         sen_pty2 = assign_08_10_pty(sen_pty2, year, sen_can2, "R")
         ) |>
  mutate_at(.vars = vars(matches("(_pty|_ipt)")), .funs = std_ptylabel)

# unique by dataset
carry_vars2 <- c("dataset", carry_vars)


# wide to long cand-party df -----
cli_h1("Format candidates")
rc_key <- melt_cand(df_current, c("rep_can", "rep_pty"), carry_vars2)
sc_key <- melt_cand(df_current, c("sen_can", "sen_pty"), carry_vars2)
gc_key <- melt_cand(df_current, c("gov_can", "gov_pty"), carry_vars2)


# create key of incumbent MC ----
cli_h1("Format incumbents")
# Incumbents, by CCES variable (not by respondent -- so key sen1 and sen2 separate)
ri_mc_match  <- match_MC(df_current, inc_H, "rep",  carry_vars2)
s1i_mc_match <- match_MC(df_current, inc_S, "sen1", carry_vars2)
s2i_mc_match <- match_MC(df_current, inc_S, "sen2", carry_vars2)

# a bit more work for governors
r_govinc <- df_current |>
  select(all_of(carry_vars2), gov_inc, gov_ipt) |> 
  mutate(namelast = str_to_upper(word(gsub(suffixes, "", gov_inc), -1)))

gov_inc_match <- r_govinc |> 
  mutate(gov_current = concatenate_current(gov_inc, gov_ipt)) |>
  arrange(year, case_id) |> 
  select(all_of(carry_vars2), matches("_current"))
  

# Save ---------
cli_h1("Save formatted")
save(ri_mc_match, s1i_mc_match, s2i_mc_match, gov_inc_match,
     file = "data/output/01_responses/incumbents_key.RData")
save(rc_key, sc_key, gc_key, 
     file = "data/output/01_responses/candidates_key.RData")


cli_alert_success("Finished matching candidate info to identifiers")
