library(tidyverse)
library(rcces)
library(haven)
library(lubridate)

# helper data ---
statecode <- read_csv("data/source/statecode.csv")

cces_cd <- function(s, d) {
  str_c(s, "-", str_pad(d, width = 2, pad = '0'))
} 

# functions ---


#' rudimentary standardization for data that comes out of dataverse
std_dv <- function(path, guess_year = TRUE) {
  if (guess_year) guessed_yr <- as.integer(gsub(".*/([0-9]+)_(cc|hu|panel).*", "\\1", path))
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
  
  if (!guess_year | guessed_yr %% 2 == 0)
    tbl_st <- std_statepost(tbl_st, guess_year, guessed_yr)
  
  # CD number
  tbl_cd <- std_dist(tbl_st, guess_year, guessed_yr)
  
  if (!guess_year | guessed_yr %% 2 == 0)
    tbl_cd <- std_distpost(tbl_cd, guess_year, guessed_yr)
  
  # 2006 is special but just for CC
  if (!guess_year) {
    tbl_cd_06 <- filter(tbl_cd, year == 2006, survey_complete == 1) %>% 
      mutate(state_post = state,
             st_post = st,
             dist_post = dist,
             dist_up_post = dist_up) %>% 
      mutate(cd_post = cces_cd(st_post, dist_post),
             cd_up_post = cces_cd(st_post, dist_up_post))
    
    tbl_cd <- bind_rows(tbl_cd_06, anti_join(tbl_cd, select(tbl_cd_06, year, caseid)))
  }
  
  # then rename id
  tbl_cd %>%
    rename(case_id = !! orig_key) %>%
    relocate(year, case_id, 
             state, st, matches("(state|st)_post"),
             dist, dist_up, matches("(dist|dist_up)_post"),
             cd, cd_up, matches("(cd|cd_up)_post"),
             cong, cong_up)
}


#' change class of state for specific years
std_state <- function(tbl, guess_year, guessed_yr) {
  
  if (!guess_year & identical(as.integer(unique(tbl$year)), 2006L:2012L)) { # for cumulative, swap around names
      tbl <- tbl %>%
        mutate(state = as.character(as_factor(state_pre))) %>%
        left_join(select(statecode, state, st), by = "state")
    return(tbl)
  }
  
  # guess variable based on year
  statevar <- case_when(
    guessed_yr %in% c(2007, 2012:2021) ~ "inputstate",
    guessed_yr %in% c(2008, 2010:2011) ~ "V206",
    guessed_yr %in% 2009 ~ "v259",
    guessed_yr %in% 2006 ~ "v1002"
  )
  
  if (!guessed_yr %in% c(2006, 2009)) {
    tbl <- tbl %>%
      mutate(state = as.character(as_factor(.data[[statevar]]))) %>%
      mutate(state = str_replace(str_to_title(state), "\\sOf\\s", " of ")) %>% 
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
  tbl
}

std_statepost <- function(tbl, guess_year, guessed_yr) {
  if (!guess_year & identical(as.integer(unique(tbl$year)), 2006L:2012L)) {
    tbl <- tbl %>%
      mutate(state_post = as.character(as_factor(state_post))) %>%
      left_join(select(statecode, state_post = state, st_post = st), by = "state_post")
    return(tbl) 
  }
  
  if (guessed_yr == 2006) { # no distinction
    tbl <- tbl %>% 
      mutate(state_post = state,
             st_post = st)
    
    return(tbl)
  }
  
  statevar <- case_when(
    guessed_yr %in% c(2012, 2014, 2016, 2018, 2020) ~ "inputstate_post",
    guessed_yr %in% c(2010) ~ "V206_post",
    guessed_yr %in% 2008 ~ "V259"
  )
  
  # 2012-2014 panel is an exception
  if (any(str_detect(colnames(tbl), "post_inputstate")))
    statevar <- "post_inputstate"
  
  # create state_post and st_post
  tbl %>%
    mutate(state_post = as.character(as_factor(.data[[statevar]]))) %>%
    mutate(state_post = str_replace(str_to_title(state_post), "\\sOf\\s", " of ")) %>% 
    left_join(select(statecode, state_post = state, st_post = st), by = "state_post")
}


#' change class of dist for specific years

std_dist <- function(tbl, guess_year, guessed_yr) {
  if (!guess_year & identical(as.integer(unique(tbl$year)), 2006L:2012L)) {
    tbl <- tbl %>%
      mutate(dist = as.integer(zap_labels(congdist_pre)),
             dist_up = as.integer(zap_labels(congdist_redist_pre))) %>% 
      mutate(dist_up = replace(dist_up, year %in% 2006:2011, NA)) %>% # these were left as missing, except at-large. fix to missing.
      mutate(dist_up = coalesce(dist_up, dist)) # for 2006:2011, append dist for now
  }
  
  if (guess_year) {
    distvar <- case_when(
      guessed_yr %in% c(2021, 2022) ~ "cdid117",
      guessed_yr %in% c(2019, 2020) ~ "cdid116",
      guessed_yr %in% c(2017, 2018) ~ "cdid115",
      guessed_yr %in% c(2013, 2016) ~ "cdid113",
      guessed_yr %in% c(2012, 2015, 2014) ~ "cdid",
      guessed_yr %in% c(2010, 2011) ~ "V276",
      guessed_yr %in% 2009 ~ "v264",
      guessed_yr %in% 2008 ~ "V250",
      guessed_yr %in% 2007 ~ "cdid_num",
      guessed_yr %in% 2006 ~ "v1003"
    )
    
    # district in the upcoming election
    distupvar <- distvar
    if (guessed_yr == 2012) distupvar <- "cdid113"
    if (guessed_yr == 2016) distupvar <- "cdid115"
    if (guessed_yr == 2018) distupvar <- "cdid116"
    
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
  
  # fix at large and add cd
  fix_al <- tbl %>%
    mutate(
      dist = replace(dist, dist == 0, 1L), # At-LARGE is 1
      dist_up = replace(dist_up, dist_up == 0, 1L)
    ) %>% 
    mutate(cd    = cces_cd(st, dist),
           cd_up = cces_cd(st, dist_up))
  
  fix_al
}


# post is only for even years
std_distpost <- function(tbl, guess_year, guessed_yr) {
  
  if (!guess_year & identical(as.integer(unique(tbl$year)), 2006L:2012L)) {
    tbl <- tbl %>%
      mutate(dist_post = as.integer(zap_labels(congdist_post)),
             dist_up_post = as.integer(zap_labels(congdist_redist_post))) %>% 
      mutate(dist_up_post = coalesce(dist_up_post, dist_post)) %>% 
      mutate(dist_up_post = replace(dist_up_post, year %% 2 == 1, NA)) 
  }
  
  if (guess_year && guessed_yr == 2006) {
    tbl <- tbl %>% 
      mutate(dist_post = dist,
             dist_up_post = dist_up)
  }
  
  if (guess_year && guessed_yr != 2006) {
    distvar <- case_when(
      guessed_yr %in% c(2020) ~ "cdid116_post",
      guessed_yr %in% c(2018) ~ "cdid115_post",
      guessed_yr %in% c(2016) ~ "cdid113_post",
      guessed_yr %in% c(2012, 2014) ~ "cdid_post",
      guessed_yr %in% c(2010) ~ "V276_post",
      guessed_yr %in% 2008 ~ "V264"
    )
    
    distupvar <- distvar
    if (guessed_yr == 2018) distupvar <- "cdid116_post"
    if (guessed_yr == 2016) distupvar <- "cdid115_post"
    if (guessed_yr == 2012) distupvar <- "cdid113_post"
    
    # 2012-2014 panel is an exception; overwrite
    if (any(str_detect(colnames(tbl), "post_cdid"))) {
      distvar   <- "post_cdid"
      distupvar <- "post_cdid113"
    }
    
    tbl <- tbl %>%
      rename(dist_post = !!distvar) %>%
      mutate(dist_post = as.integer(dist_post))
    
    if (distupvar != distvar) {
      tbl <- tbl %>%
        rename(dist_up_post = !!distupvar) %>%
        mutate(dist_up_post = as.integer(dist_up_post))
    }
    
    if (distupvar == distvar) {
      tbl <- tbl %>%
        mutate(dist_up_post = as.integer(dist_post))
    }
  }
  
  # fix at large and add cd
  fix_al <- tbl %>%
    mutate(
      dist_post = replace(dist_post, dist_post == 0, 1L), 
      dist_up_post = replace(dist_up_post, dist_up_post == 0, 1L)
    ) %>% 
    mutate(cd_post    = cces_cd(st_post, dist_post),
           cd_up_post = cces_cd(st_post, dist_up_post))
  fix_al
}

# Check that at least some state and district are  different between  pre and post
check_pre_post <- function(tbl) {
  tbl_movers <- filter(tbl,
                       state != state_post,
                       dist != dist_post,
                       dist_up != dist_up_post,
                       cd != cd_post
  )
  
  stopifnot(nrow(tbl_movers) > 0)
}

# Data -----------
# 2012 and before (compiled by Stephen Pettigrew and others)
ccp <- std_dv("data/source/cces/2006_2012_cumulative.dta", guess_year = FALSE)
ccp <- filter(ccp, !(st == "MS" & dist == 8)) # drop one obs with a CD that does not existreturn(code


# individual files versions from 2008, 2010, and 2012
cc06 <- std_dv("data/source/cces/2006_cc.dta")
cc07 <- std_dv("data/source/cces/2007_cc.dta")
cc08 <- std_dv("data/source/cces/2008_cc.dta")
cc09 <- std_dv("data/source/cces/2009_cc.dta")
cc10 <- std_dv("data/source/cces/2010_cc.dta")
cc11 <- std_dv("data/source/cces/2011_cc.dta")
cc12 <- std_dv("data/source/cces/2012_cc.dta")
panel12 <- std_dv("data/source/cces/2012_panel_h.dta")
cc13 <- std_dv("data/source/cces/2013_cc.dta")
cc14 <- std_dv("data/source/cces/2014_cc.dta")
cc15 <- std_dv("data/source/cces/2015_cc.dta")
cc16 <- std_dv("data/source/cces/2016_cc.dta")
cc17 <- std_dv("data/source/cces/2017_cc.dta")
cc18 <- std_dv("data/source/cces/2018_cc.dta")

cc18_comp <- std_dv("data/source/cces/2018_cc_competitive.dta")
cc18_cnew <- anti_join(cc18_comp, select(cc18, year, case_id))

cc19 <- std_dv("data/source/cces/2019_cc.dta")
cc20 <- std_dv("data/source/cces/2020_cc.dta")
cc21 <- std_dv("data/source/cces/2021_cc.dta")

# modules
hu08 <- std_dv("data/source/cces/2008_hum_allcapvars.dta")
hu09 <- std_dv("data/source/cces/2009_hum_recontact.dta")
hua18 <- std_dv("data/source/cces/2018_hua.dta")
hub18 <- std_dv("data/source/cces/2018_hub.dta")


# additional moduels ---------
# 2006 module addition
# need for accountability paper 
mit06_raw <- read_dta("data/source/cces/2006_mit_final_withcommon_validated_new.dta", encoding = 'latin1')
mit_fmt <- mit06_raw %>%
  mutate(year = 2006,
         cong = 109L,
         cong_up = 110L,
         dist = as.integer(district),
         dist_up = as.integer(district),
         starttime = lubridate::as_datetime(as.POSIXct(starttime, format = "%a %B %d %X %Y", tz = "")),
         fips  = zap_labels(inputstate),
         state = NULL) %>%
  left_join(select(statecode, fips, state, st)) %>% 
  mutate(dist    = replace(dist, st %in% c("DE", "AK", "ND", "NY", "VT", "WY"), 1), # At large
         dist_up = replace(dist_up, st %in% c("DE", "AK", "ND", "NY", "VT", "WY"), 1)) %>% 
  rename(case_id = caseid) %>% 
  select(year, case_id, state, st, cong, dist, dist_up, everything())
mit06_add <- anti_join(mit_fmt, select(cc06, year, case_id))  


# 2008 addition
# (used above in std_dv, because that only takes a path)
read_dta("data/source/cces/2008_hum.dta") %>% 
  rename_all(str_to_upper) %>% 
  select(-HUM302, -HUM304) %>% # decimal labelled
  write_dta("data/source/cces/2008_hum_allcapvars.dta")

hu08 <- anti_join(hu08, select(cc08, year, case_id)) %>% 
  mutate(V300 = as_datetime(V300))


check_pre_post(cc08)
check_pre_post(cc10)
check_pre_post(cc12)
check_pre_post(cc14)
check_pre_post(cc16)
check_pre_post(cc18)

# save ----
save(
  ccp, 
  cc06, cc07, cc08, cc09, 
  cc10, cc11, cc12, cc13, 
  cc14, cc15, cc16, cc17, 
  cc18, cc18_cnew, cc19, 
  cc20, cc21,
  panel12, 
  mit06_add,
  hu08,  hu09, 
  hua18, hub18,
  file = "data/output/01_responses/common_all.RData"
)


cat("Finished standardizing input\n")

# write_rds(mit_fmt, "~/Dropbox/CCES_representation/data/source/cces/2006_mit_fmt.rds")