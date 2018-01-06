library(tidyverse)
library(haven)


# functions --- 
# rudimentary standardization for data that comes out of dataverse
std_dv <- function(path, guess_year = TRUE) {
  if (guess_year) guessed_yr <- as.numeric(gsub(".*/([0-9]+)_cc.*", "\\1", path))
  
  ## then
  tbl <- haven::read_dta(path)
  
  
  ## guess ID
  cnames <- colnames(tbl)
  if ("caseid" %in% cnames) orig_key <- "caseid"
  if ("V101" %in% cnames) orig_key <- "V101"
  if ("V100" %in% cnames) orig_key <- "V100"
  if ("v100" %in% cnames) orig_key <- "v100"
  if ("v1000" %in% cnames) orig_key <- "v1000"
  
  
  # add year
  if (!"year" %in% colnames(tbl)) tbl <- mutate(tbl, year = guessed_yr)
  
  
  # then rename id
  tbl %>%
    rename(caseID = !! orig_key) %>%
    dplyr::select(year, caseID, everything())
}




# 2012 and before (compiled by Stephen Pettigrew and others)
ccp <- std_dv(
  "data/source/cces/2006_2012_cumulative.dta",
  guess_year = FALSE
)


pid10_raw <- read_dta("data/source/cces/cc10_pid.dta")
pid3_cc10 <- pid10_raw %>%
  mutate(
    pid3 = CC421a,
    caseID = V100
  ) %>%
  mutate(
    year = 2010,
    pid3_char = as.character(as_factor(pid3)),
    pid3_num = as.numeric(pid3)
  ) %>%
  mutate(
    pid3_char = replace(pid3_char, pid3_char == "NaN", NA),
    pid3_num = replace(pid3_num, is.nan(pid3_num), NA)
  ) %>%
  select(year, caseID, pid3_char, pid3_num)


# individual files versions from 2008, 2010, and 2012
cc06 <- std_dv("data/source/cces/2006_cc.dta")
cc07 <- std_dv("data/source/cces/2007_cc.dta")
cc08 <- std_dv("data/source/cces/2008_cc.dta")
cc09 <- std_dv("data/source/cces/2009_cc.dta")
cc10 <- std_dv("data/source/cces/2010_cc.dta")
cc11 <- std_dv("data/source/cces/2011_cc.dta")
cc12 <- std_dv("data/source/cces/2012_cc.dta")
cc13 <- std_dv("data/source/cces/2013_cc.dta")
cc14 <- std_dv("data/source/cces/2014_cc.dta")
cc15 <- std_dv("data/source/cces/2015_cc.dta")
cc16 <- std_dv("data/source/cces/2016_cc_vv.dta")


# save ---- 
save(ccp, cc06, cc07, cc08, cc09, cc10, cc11, cc12, cc13, cc14, cc15, cc16, 
     file = "data/output/01_responses/common_all.RData")
