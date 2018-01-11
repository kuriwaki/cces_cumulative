library(tidyverse)
library(haven)

# helper data ---
statecode <- read_csv("data/source/statecode.csv")


# functions ---
#' rudimentary standardization for data that comes out of dataverse
std_dv <- function(path, guess_year = TRUE) {
  if (guess_year) guessed_yr <- as.integer(gsub(".*/([0-9]+)_cc.*", "\\1", path))
  if (!guess_year) guessed_yr <- NA

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
  
  # add congressional session
  tbl_cong <- tbl %>% 
    mutate(
      cong = as.integer(ceiling((year - 1788)/2)),
      cong_up = cong + 1L
    )
  
  # state
  tbl_st <- std_state(tbl_cong, guess_year, guessed_yr)
  
  # CD number
  tbl_stc <- std_cdid(tbl_st, guess_year, guessed_yr)


  # then rename id
  tbl_stc %>%
    rename(caseID = !! orig_key) %>%
    select(year, caseID, state, st, cdid, cdid_up, cong, cong_up, everything())
}


#' change class of state for specific years
std_state <- function(tbl, guess_year, guessed_yr) {
  if (guess_year) {
    statevar <- case_when(
      guessed_yr %in% c(2007, 2012:2016) ~ "inputstate",
      guessed_yr %in% c(2008, 2010:2011) ~ "V206",
      guessed_yr %in% 2009 ~ "v259",
      guessed_yr %in% 2006 ~ "v1002"
    )
    
    if (!guessed_yr %in% c(2006, 2009)) {
      tbl <- tbl %>%
        mutate(state = as.character(as_factor(.data[[statevar]]))) %>%
        left_join(select(statecode, state, st), by = "state")
    }
    
    if (guessed_yr %in% 2006) { # 2006 codes abbreviations as character
      tbl <- tbl %>%
        rename(st = !! statevar) %>%
        left_join(select(statecode, state, st), by = "st")
    }
    
    if (guessed_yr %in% 2009) { # 2009 codes lower case labels
      tbl <- tbl %>%
        mutate(state = str_to_title(as.character(as_factor(.data[[statevar]])))) %>%
        left_join(select(statecode, state, st), by = "state")
    }
  }
  
  if (identical(as.integer(unique(tbl$year)), 2006L:2012L)) { # for cumulative, swap around names
    tbl <- tbl %>%
      mutate(state = as.character(as_factor(state_pre))) %>%
      left_join(select(statecode, state, st), by = "state")
  }
  tbl
}


#' change class of cdid for specific years
std_cdid <- function(tbl, guess_year, guessed_yr) {
  if (guess_year) {
    cdidvar <- case_when(
      guessed_yr %in% c(2013, 2016) ~ "cdid113",
      guessed_yr %in% c(2012, 2015, 2014) ~ "cdid",
      guessed_yr %in% 2006 ~ "v1003",
      guessed_yr %in% 2007 ~ "cdid_num",
      guessed_yr %in% 2008 ~ "V250",
      guessed_yr %in% 2009 ~ "v264",
      guessed_yr %in% c(2010, 2011) ~ "V276"
    )
    
    # district in the upcoming election
    cdidupvar <- cdidvar
    if (guessed_yr == 2012) cdidupvar <- "cdid113"
    if (guessed_yr == 2016) cdidupvar <- "cdid115"
    
    
    if (!guessed_yr %in% c(2006, 2007)) {
      
      if (cdidupvar == cdidvar) {
        tbl <- tbl %>%
          rename(cdid = !!cdidvar) %>%
          mutate(cdid = as.integer(cdid)) %>% 
          mutate(cdid_up = cdid)
      } else {
        tbl <- tbl %>%
          rename(cdid = !!cdidvar,
                 cdid_up = !!cdidupvar) %>%
          mutate(cdid = as.integer(cdid),
                 cdid_up = as.integer(cdid_up))
      }
    }
    
    if (guessed_yr %in% 2006) { # 2006 codes abbreviations as character
      tbl <- tbl %>%
        mutate(cdid = as.integer(zap_labels(.data[[cdidvar]])),
               cdid_up = cdid)
    }
    
    if (guessed_yr %in% 2007) { # 2009 codes lower case labels
      tbl <- tbl %>%
        mutate(cdid = as.integer(.data[[cdidvar]]),
               cdid_up = cdid)
    }
  }
  
  if (identical(as.integer(unique(tbl$year)), 2006L:2012L)) { # for cumulative, swap around names
    tbl <- tbl %>%
      mutate(cdid = as.integer(zap_labels(congdist_pre)),
             cdid_up = as.integer(zap_labels(congdist_redist_pre))) %>% 
      mutate(cdid_up = replace(cdid_up, year %in% 2006:2011, NA)) %>% # these were left as missing, except at-large. fix to missing.
      mutate(cdid_up = coalesce(cdid_up, cdid)) # for 2006:2011, append cdid for now
  }
  # fix at large
  fix_al <- tbl %>%
       mutate(
         cdid = replace(cdid, cdid == 0, 1L), # At-LARGE is 1
         cdid_up = replace(cdid_up, cdid_up == 0, 1L)
         )
  
  fix_al
}




# 2012 and before (compiled by Stephen Pettigrew and others)
ccp <- std_dv(
  "data/source/cces/2006_2012_cumulative.dta",
  guess_year = FALSE
)

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
save(
  ccp, cc06, cc07, cc08, cc09, cc10, cc11, cc12, cc13, cc14, cc15, cc16,
  file = "data/output/01_responses/common_all.RData"
)

