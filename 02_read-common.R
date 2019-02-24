library(tidyverse)
library(haven)

# helper data ---
statecode <- read_csv("data/source/statecode.csv")


# functions ---
#' rudimentary standardization for data that comes out of dataverse
std_dv <- function(path, guess_year = TRUE) {
  if (guess_year) guessed_yr <- as.integer(gsub(".*/([0-9]+)_(cc|hum|panel).*", "\\1", path))
  if (!guess_year) guessed_yr <- NA

  ## then
  tbl <- haven::read_dta(path, encoding = 'latin1')


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
  tbl_stc <- std_dist(tbl_st, guess_year, guessed_yr)


  # then rename id
  tbl_stc %>%
    rename(case_id = !! orig_key) %>%
    select(year, case_id, state, st, dist, dist_up, cong, cong_up, everything())
}


#' change class of state for specific years
std_state <- function(tbl, guess_year, guessed_yr) {
  if (guess_year) {
    statevar <- case_when(
      guessed_yr %in% c(2007, 2012:2017) ~ "inputstate",
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


#' change class of dist for specific years
std_dist <- function(tbl, guess_year, guessed_yr) {
  if (guess_year) {
    distvar <- case_when(
      guessed_yr %in% c(2017) ~ "cdid115",
      guessed_yr %in% c(2013, 2016) ~ "cdid113",
      guessed_yr %in% c(2012, 2015, 2014) ~ "cdid",
      guessed_yr %in% 2006 ~ "v1003",
      guessed_yr %in% 2007 ~ "cdid_num",
      guessed_yr %in% 2008 ~ "V250",
      guessed_yr %in% 2009 ~ "v264",
      guessed_yr %in% c(2010, 2011) ~ "V276"
    )
    
    # district in the upcoming election
    distupvar <- distvar
    if (guessed_yr == 2012) distupvar <- "cdid113"
    if (guessed_yr == 2016) distupvar <- "cdid115"
    
    
    if (!guessed_yr %in% c(2006, 2007)) {
      
      if (distupvar == distvar) {
        tbl <- tbl %>%
          rename(dist = !!distvar) %>%
          mutate(dist = as.integer(dist)) %>% 
          mutate(dist_up = dist)
      } else {
        tbl <- tbl %>%
          rename(dist = !!distvar,
                 dist_up = !!distupvar) %>%
          mutate(dist = as.integer(dist),
                 dist_up = as.integer(dist_up))
      }
    }
    
    if (guessed_yr %in% 2006) { # 2006 codes abbreviations as character
      tbl <- tbl %>%
        mutate(dist = as.integer(zap_labels(.data[[distvar]])),
               dist_up = dist)
    }
    
    if (guessed_yr %in% 2007) { # 2009 codes lower case labels
      tbl <- tbl %>%
        mutate(dist = as.integer(.data[[distvar]]),
               dist_up = dist)
    }
  }
  
  if (identical(as.integer(unique(tbl$year)), 2006L:2012L)) { # for cumulative, swap around names
    tbl <- tbl %>%
      mutate(dist = as.integer(zap_labels(congdist_pre)),
             dist_up = as.integer(zap_labels(congdist_redist_pre))) %>% 
      mutate(dist_up = replace(dist_up, year %in% 2006:2011, NA)) %>% # these were left as missing, except at-large. fix to missing.
      mutate(dist_up = coalesce(dist_up, dist)) # for 2006:2011, append dist for now
  }
  # fix at large
  fix_al <- tbl %>%
       mutate(
         dist = replace(dist, dist == 0, 1L), # At-LARGE is 1
         dist_up = replace(dist_up, dist_up == 0, 1L)
         )
  
  fix_al
}


# 2012 and before (compiled by Stephen Pettigrew and others)
ccp <- std_dv("data/source/cces/2006_2012_cumulative.dta", guess_year = FALSE)
ccp <- filter(ccp, !(st == "MS" & dist == 8)) # drop one obs with a CD that does not existreturn(code


# individual files versions from 2008, 2010, and 2012
cc06 <- std_dv("data/source/cces/2006_cc.dta")
cc07 <- std_dv("data/source/cces/2007_cc.dta")
cc08 <- std_dv("data/source/cces/2008_cc.dta")
cc09 <- std_dv("data/source/cces/2009_cc.dta")
hu09 <- std_dv("data/source/cces/2009_hum_recontact.dta")
cc10 <- std_dv("data/source/cces/2010_cc.dta")
cc11 <- std_dv("data/source/cces/2011_cc.dta")
cc12 <- std_dv("data/source/cces/2012_cc.dta")
panel12 <- std_dv("data/source/cces/2012_panel_h.dta")
cc13 <- std_dv("data/source/cces/2013_cc.dta")
cc14 <- std_dv("data/source/cces/2014_cc.dta")
cc15 <- std_dv("data/source/cces/2015_cc.dta")
cc16 <- std_dv("data/source/cces/2016_cc.dta")
cc17 <- std_dv("data/source/cces/2017_cc.dta")



# 2006 module addition
mit06_raw <- read_dta("data/source/cces/2006_mit_final_withcommon_validated_new.dta", encoding = 'latin1')
mit_fmt <- mit06_raw %>%
  mutate(year = 2006,
         cong = 109L,
         cong_up = 110L,
         dist = as.integer(district),
         dist_up = as.integer(district),
         starttime = lubridate::as_datetime(as.POSIXct(starttime, format = "%a %B %d %X %Y", tz = "")),
         stfips  = as.integer(zap_labels(inputstate)),
         state = NULL) %>%
  rename(case_id = caseid) %>% 
  left_join(transmute(statecode,  stfips = as.integer(fips), state, st), by = c("stfips")) %>% 
  select(year, case_id, state, st, cong, dist, dist_up, everything())
mit06_add <- anti_join(mit_fmt, select(cc06, year, case_id))  


# save ----
save(
  ccp, mit06_add, cc06, cc07, cc08, cc09, hu09, cc10, cc11, cc12, panel12, cc13, cc14, cc15, cc16, cc17,
  file = "data/output/01_responses/common_all.RData"
)


cat("Finished standardizing input\n")