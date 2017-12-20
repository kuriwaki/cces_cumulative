
library(tidyverse)
library(haven)
library(foreach)
library(stringr)
library(lubridate)
library(data.table)


# helper data -- 
statecode <- read_csv("~/Dropbox/cces_rollcall/data/source/statecode.csv",
                      col_types = cols())


# functions ----- 
# rudimentary standardization for data that comes out of dataverse
std_dv <- function(path, guess_year = TRUE) {
  
  if (guess_year) guessed_yr <- as.numeric(gsub(".*/([0-9]+)_cc.*", "\\1", path))
  
  ## then
  tbl <- haven::read_dta(path)
  
  
  ## guess ID
  if ("caseid" %in% colnames(tbl)) orig_key <- "caseid"
  if ("V101" %in% colnames(tbl)) orig_key <- "V101"
  if ("V100" %in% colnames(tbl)) orig_key <- "V100"
  
  
  # add year
  if (!"year" %in% colnames(tbl)) tbl <- mutate(tbl, year = guessed_yr)
  
  
  # then rename id
  tbl %>%
    rename(caseID = !!orig_key) %>%
    dplyr::select(year, caseID, everything())
  
}

# inner extraction function for findStack
extract_yr <- function(tbl, var, var_name, chr_var_name, num_var_name, 
                       is_factor = TRUE) {
  if (is_factor) {
    if (var_name %in% colnames(tbl))  {
      select(tbl, year, caseID, !!var) %>%
        mutate(!!chr_var_name := as.character(as_factor(.data[[var_name]])),
               !!num_var_name := as.numeric(.data[[var_name]])) %>% 
        select(-!!var)  
    } else {
      select(tbl, year, caseID) %>%
        mutate(!!chr_var_name := NA,
               !!num_var_name := NA)
    }
  } else {
    if (var_name %in% colnames(tbl))  {
      select(tbl, year, caseID, !!var) %>%
        mutate(!!var_name := zap_labels(!!var))
    } else {
      select(tbl, year, caseID) %>%
        mutate(!!var_name := NA)
    }
  }
}


# takes all datasets available, and given a var, pulls it out of each and stacks
findStack <- function(dflist = list(), var, type = "factor", makeLabelled = FALSE) {
  
  var <- enquo(var)
  var_name <- quo_name(var)
  chr_var_name <- paste0(var_name, "_char")
  num_var_name <- paste0(var_name, "_num")
  
  if (type == "factor") {
    list_yr <- foreach(yr = 1:length(dflist), .combine = "bind_rows") %do% {
        extract_yr(dflist[[yr]], enquo(var), var_name, chr_var_name, num_var_name)
      }
    
    ## format to change NaN to NA
    list_yr <- list_yr %>%
      mutate(!!chr_var_name := replace(.data[[chr_var_name]], .data[[chr_var_name]] == "NaN", NA),
             !!num_var_name := replace(.data[[num_var_name]], is.nan(.data[[num_var_name]]), NA)) %>%
      mutate(!!chr_var_name := str_to_title(.data[[chr_var_name]]),
             !!chr_var_name := replace(.data[[chr_var_name]],
                                       .data[[chr_var_name]] == "Never Heard",
                                       "Never Heard Of This Person"),
             !!chr_var_name := replace(.data[[chr_var_name]],
                                       .data[[chr_var_name]] == "No Hs",
                                       "No HS")) 
  }
  
  
  if (type != "factor") {
    list_yr <- foreach(yr = 1:length(dflist), .combine = "bind_rows") %do% {
      extract_yr(dflist[[yr]], enquo(var), var_name, chr_var_name, num_var_name, FALSE)
    }
    
    # change to specified type
    if (type == "numeric") {
      list_yr <- mutate(list_yr, !!var_name := as.numeric(.data[[var_name]]))
    }
    
    if (type == "character") {
      list_yr <- mutate(list_yr, !!var_name := as.character(as_factor(!!var)))
    }
    
    if (type == "datetime") {
      list_yr <- mutate(list_yr, !!var_name := as.POSIXct(.data[[var_name]]))
    }
    
    list_yr <-  list_yr %>% 
      mutate(!!var_name := replace(.data[[var_name]], is.nan(.data[[var_name]]), NA))
  }
  
  # coerce to labelled?
  if (type == "factor" & makeLabelled) {
    
    # change consistent vars in to a labelled factor
    # make numbered vector
    key_arr <- select(list_yr, -year, -caseID) %>% distinct() %>% 
      filter(!is.na(.data[[num_var_name]]))
    
    nvec <- key_arr %>% select(matches("num")) %>%  pull()
    names(nvec) <- key_arr %>% select(matches("char")) %>% pull()
    
    
    list_yr <- list_yr %>% 
      mutate(!!var_name := labelled(as.integer(.data[[num_var_name]]), sort(nvec))) %>%
      select(year, caseID, !!var_name)
  }
  
  list_yr
}


  

# more name standardization
stdName <- function(tbl){
  
  cces_year <- as.integer(unique(tbl$year))
  
  
  if (identical(cces_year, 2006:2011)) {
    tbl <- tbl %>% 
      rename(state = state_pre,
             cdid = congdist_pre,
             zipcode = zip_pre,
             countyFIPS = county_fips_pre,
             starttime = start_pre,
             reg_true = reg_validation,
             reg_self = registered_pre,
             validt_trn = gen_validated,
             economy_retro = economy_retrospective,
             voted_pres_08 = vote_pres_08,
             voted_rep = vote_house, 
             voted_sen = vote_sen,
             voted_gov = vote_gov,
             intent_pres_08 = vote_intent_pres_08,
             intent_pres_12 = vote_intent_pres_12,
             intent_rep = vote_intent_house,
             intent_sen = vote_intent_senate,
             intent_gov = vote_intent_gov,
             vv_regstatus = reg_validation,
             vv_turnout_gvm = gen_validated,
             vv_turnout_pvm = prim_validated)
  }
  
  if (identical(cces_year, 2012L)) {
    tbl <- rename(tbl, 
                  weight = V103,
                  approval_pres = CC308a,
                  approval_rep = CC315a,
                  approval_sen1 = CC315b,
                  approval_sen2 = CC315c,
                  economy_retro = CC302,
                  approval_gov = CC308d,
                  intent_pres_12 = CC354c,
                  intent_pres_12x = CC354b,
                  voted_pres_12 = CC410a,
                  voted_pres_08 = CC317,
                  voted_rep = CC412,
                  voted_sen = CC410b,
                  voted_gov = CC411,
                  intent_sen = CC355b,
                  intent_senx = CC355,
                  intent_gov = CC356b,
                  intent_govx = CC356,
                  intent_rep = CC390b,
                  intent_repx = CC390,
                  vv_turnout_gvm = e2012g,
                  vv_turnout_pvm = e2012congp,
                  vv_turnout_ppvm = e2012presp,
                  vv_regstatus = voter_status,
                  vv_party_gen = PartyRegist,
                  vv_party_prm = congprim_pty,
                  vv_party_pprm = presprim_pty) %>% 
      mutate(voted_pres_12 = coalesce(voted_pres_12, intent_pres_12x),
             voted_rep = coalesce(voted_rep, intent_repx),
             voted_sen = coalesce(voted_sen, intent_senx),
             voted_gov = coalesce(voted_gov, intent_govx))
  }

  if (identical(cces_year, 2013L)) {
    tbl <- tbl %>% mutate(fips = floor(as.numeric(countyfips)/1000),
                          cdid = as.numeric(cdid113)) %>% 
      rename(approval_pres = CC312a,
             approval_rep = CC13_313a,
             approval_sen1 = CC13_313b,
             approval_sen2 = CC13_313c,
             approval_gov = CC312d,
             economy_retro = CC13_302,
             voted_pres_12 = CC13_315) %>% 
      left_join(statecode, by = "fips")
  }
  
  if (identical(cces_year, 2014L)) {
    tbl <- rename(tbl, 
                  approval_rep = CC14_315a,
                  approval_sen1 = CC14_315b,
                  # approval_sen2 = CC14_315c,
                  approval_gov = CC14_308d,
                  economy_retro = CC14_302,
                  voted_rep = CC412,
                  voted_pres_12 = CC14_317,
                  voted_sen = CC410b,
                  voted_gov = CC411,
                  intent_sen = CC355,
                  intent_senx = CC355x,
                  intent_gov = CC356,
                  intent_govx = CC356x,
                  intent_rep = CC360,
                  intent_repx = CC360x,
                  vv_turnout_gvm = e2014gvm,
                  vv_turnout_pvm = e2014pvm,
                  vv_regstatus = voterstatus,
                  vv_party_gen = partyaffiliation,
                  vv_party_prm = e2014pep,
                  vv_st = state_cl) %>% 
      mutate(voted_rep = coalesce(voted_rep, intent_repx),
             voted_sen = coalesce(voted_sen, intent_senx),
             voted_gov = coalesce(voted_gov, intent_govx))
  } 
  
  if (identical(cces_year, 2015L)) {
    tbl <- rename(tbl, CC350 = CC15_350) %>% 
      rename(approval_pres = CC15_312a,
             approval_rep = CC15_313a,
             approval_sen1 = CC15_313b,
             approval_sen2 = CC15_313c,
             approval_gov = CC15_312f,
             economy_retro = CC15_302,
             voted_pres_12 = CC15_315)
  }
  
  if (identical(cces_year, 2016L)) {
    tbl <- tbl %>% 
      rename(weight = commonweight,
             CC350 = CC16_360,
             cdid = cdid113,
             starttime = starttime_pre,
             approval_pres = CC16_320a,
             approval_rep = CC16_320f,
             approval_sen1 = CC16_320g,
             approval_sen2 = CC16_320h,
             approval_gov = CC16_320d,
             economy_retro = CC16_302,
             intent_trn = CC16_364,
             intent_pres_16 = CC16_364c,
             intent_pres_16x = CC16_364b,
             intent_rep = CC16_367,
             intent_repx = CC16_367x, # house early vote (already voted)
             intent_sen = CC16_365,
             intent_senx = CC16_365x,
             intent_gov = CC16_366,
             intent_govx = CC16_366x,
             voted_trn = CC16_401,
             voted_pres_12 = CC16_326,
             voted_pres_16 = CC16_410a,
             voted_rep = CC16_412,
             voted_sen = CC16_410b,
             voted_gov = CC16_411,
             vv_turnout_gvm = CL_E2016GVM,
             vv_turnout_pvm = CL_E2016PVM,
             vv_turnout_ppvm = CL_E2016PPVM,
             vv_regstatus = CL_voterstatus,
             vv_party_gen = CL_partyaffiliation,
             vv_party_prm = CL_E2016PEP,
             vv_party_pprm = CL_E2016PPEP,
             vv_st = CL_state
      ) %>% # combine early vote
      mutate(voted_pres_16 = coalesce(voted_pres_16, intent_pres_16x),
             voted_rep = coalesce(voted_rep, intent_repx),
             voted_sen = coalesce(voted_sen, intent_senx),
             voted_gov = coalesce(voted_gov, intent_gov))
    
  }
  
  
  # more standardization for post 2012
  if (cces_year[1] %in% 2012:2016) {
    tbl <- tbl %>% 
      rename(state = inputstate, 
             reg_self = votereg,
             family_income = faminc,
             marriage_status = marstat,
             zipcode = lookupzip,
             countyFIPS = countyfips,
             partyreg = CC350) %>% 
      mutate(age = year - birthyr,
             countyFIPS = as.numeric(countyFIPS),
             cdid = as.numeric(cdid))
  }
  
  return(tbl)
}

# replicate the filler value each respondent chose 
showCand  <- function(stacked, var) {
  var <- enquo(var)
  var_name <- quo_name(var)
  
  if (grepl("rep", var_name)) race <- "House"
  if (grepl("sen", var_name)) race <- "Sen"
  if (grepl("gov", var_name)) race <- "Gov"
  
  stacked %>% 
    mutate(number = gsub(".*cand([0-9]+)name.*", "\\1", !!var)) %>% 
    left_join(cand_key[[race]], by = c("year", "caseID", "number")) %>% 
    mutate(!!var_name := paste0(cand, " (", party, ")")) %>%
    select(-number, -cand, -party)
}

# separate out those that need `showCand`, then bidn
sep_bind <- function(tbl, var) {
  var <- enquo(var)
  
  changed <- showCand(filter(tbl, grepl("cand", !!var)), !!var)
  unchanged <- filter(tbl, !grepl("cand", !!var))
  
  bind_rows(changed, unchanged) %>%
    arrange(year, caseID)
}



# READ ------

# 2012 and before (compiled by Stephen Pettigrew and others)
ccp <- std_dv("data/source/cces/2006_2012_cumulative.dta", 
              guess_year = FALSE)


pid10_raw <- read_dta("data/source/cces/cc10_pid.dta") 
pid3_cc10 <- pid10_raw %>% 
  mutate(pid3 = CC421a,
         caseID = V100) %>%
  mutate(year = 2010,
         pid3_char = as.character(as_factor(pid3)),
         pid3_num = as.numeric(pid3)) %>% 
  mutate(pid3_char = replace(pid3_char, pid3_char == "NaN", NA),
         pid3_num = replace(pid3_num, is.nan(pid3_num), NA)) %>%
  select(year, caseID, pid3_char, pid3_num)


# take the needed columns for 2013- 2016
cc13 <- std_dv("data/source/cces/2013_cc.dta")
cc14 <- std_dv("data/source/cces/2014_cc.dta")
cc15 <- std_dv("data/source/cces/2015_cc.dta")
cc16 <- std_dv("data/source/cces/2016_cc_vv.dta")


# old versions from 2008, 2010, and 2012 (for vote variables)?
cc08 <- std_dv("data/source/cces/2008_cc.dta")
cc10 <- std_dv("data/source/cces/2010_cc.dta")
cc12 <- std_dv("data/source/cces/2012_cc.dta")





# key to label ----
# cand info for 2013 - 2016

races <- c("House", "Sen", "Gov")
cand_regex <- c(paste0(paste0("^", races, "cand[0-9+]"), "name$"),
                paste0(paste0("^", races, "cand[0-9+]"), "party$"))

# function to melt assigned options
melt_year_reg <- function(tbl, measure_regex) {
  melt(as.data.table(tbl), 
       id.vars = c("year", "caseID"),
       measure.vars = patterns(measure_regex),
       variable.name = "number",
       value.name = c("cand", "party"),
       variable.factor = FALSE) %>%
    subset(cand != "") %>%
    tbl_df() 
}

# employ melt_year_reg to 14 and 16
cand_key <- foreach(r = races, .combine = "c") %do% {
  measure_regex <- paste0(paste0("^", r, "Cand[0-9+]"), c("Name$", "Party$"))
  key <- list()
  
  year_2012 <- melt_year_reg(cc12, measure_regex)
  year_2014 <- melt_year_reg(cc14, measure_regex)
  year_2016 <- melt_year_reg(cc16, measure_regex)
  

  key[[r]] <- bind_rows(year_2012, year_2014, year_2016)
  key
}



  
# do this for each year and of the three offices 

# extract the choice number from the responses
# then left join to vote variable to get the name and party


# execute name standardization -----
# in list form
ccs <- list(stdName(filter(ccp, year != 2012)), 
            stdName(cc12), 
            stdName(cc13), 
            stdName(cc14), 
            stdName(cc15), 
            stdName(cc16))

# mutations to data -----

# fix county misalignment
ccs[[1]] <- ccs[[1]] %>% 
  mutate(countyFIPS = (countyFIPS < 1000)*as.numeric(state)*1000 + countyFIPS)


# Extract variable by variable iniitial version -----

# admin
wgt <- findStack(ccs, weight, "numeric")

time <- findStack(ccs, starttime, type = "datetime", makeLabelled = FALSE) %>% 
  filter(year != 2006, year != 2009) %>% 
  bind_rows(readRDS("data/source/cces/cc06_datetime.Rds")) %>%
  bind_rows(readRDS("data/source/cces/cc09_datetime.Rds"))


# demos
pid3 <- findStack(ccs, pid3) %>%
  filter(year != 2010) %>%  # fix the missing 2010
  bind_rows(pid3_cc10) %>%
  mutate(pid3 = labelled(as.integer(pid3_num), 
                         c("Democrat" = 1,
                           "Republican" = 2,
                           "Independent" = 3,
                           "Other" = 4,
                           "Not sure" = 5,
                           "Skipped" = 8))) %>% 
  select(year, caseID, pid3) # manually do only this one
  
pid7 <- findStack(ccs, pid7, makeLabelled = TRUE)
gend <- findStack(ccs, gender, makeLabelled = TRUE)
educ <- findStack(ccs, educ, makeLabelled = TRUE)
race <- findStack(ccs, race, makeLabelled = TRUE)
bryr <- findStack(ccs, birthyr, "numeric")

# geography
state <- findStack(ccs, state)
zipcode <- findStack(ccs, zipcode, "character")
countyFIPS <- findStack(ccs, countyFIPS, "numeric")
cdid <- findStack(ccs, cdid, "numeric")

i_pres08 <- findStack(ccs, intent_pres_08)
i_pres12 <- findStack(ccs, intent_pres_12)
i_pres16 <- findStack(ccs, intent_pres_16)


i_rep <- findStack(ccs, intent_rep)
i_sen <- findStack(ccs, intent_sen)
i_gov <- findStack(ccs, intent_gov)

v_pres08 <- findStack(ccs, voted_pres_08)
v_pres12 <- findStack(ccs, voted_pres_12)
v_pres16 <- findStack(ccs, voted_pres_16)

v_rep <- findStack(ccs, voted_rep)
v_sen <- findStack(ccs, voted_sen)
v_gov <- findStack(ccs, voted_gov)


apvpres <- findStack(ccs, approval_pres, makeLabelled = TRUE) 
apvrep <- findStack(ccs, approval_rep, makeLabelled = FALSE)  # slightly different labels ax years
apvsen1 <- findStack(ccs, approval_sen1, makeLabelled = FALSE)
apvsen2 <- findStack(ccs, approval_sen2, makeLabelled = FALSE)
apvgov <- findStack(ccs, approval_gov, makeLabelled = TRUE) 


econ <- findStack(ccs, economy_retro, makeLabelled = TRUE)


vv_regstatus <- findStack(ccs, vv_regstatus)
vv_party_gen <- findStack(ccs, vv_party_gen)
vv_party_prm <- findStack(ccs, vv_party_prm)
vv_turnout_gvm <- findStack(ccs, vv_turnout_gvm)
vv_turnout_pvm <- findStack(ccs, vv_turnout_pvm)


# mutate vote variables that are HouseCand fillers
i_rep <- sep_bind(i_rep, intent_rep_char)
i_sen <- sep_bind(i_sen, intent_sen_char)
i_gov <- sep_bind(i_gov, intent_gov_char)

v_rep <- sep_bind(v_rep, voted_rep_char)
v_sen <- sep_bind(v_sen, voted_sen_char)
v_gov <- sep_bind(v_gov, voted_gov_char)




# format state and CD, then zipcode and county ----
stcd <- left_join(state, cdid) %>% 
  mutate(state = state_char) %>%
  left_join(select(statecode, State, StateAbbr), by = c("state" = "State")) %>% 
  rename(st = StateAbbr) %>% 
  mutate(CD = paste0(st, "-", cdid)) %>% 
  select(year, caseID, state, st, cdid, CD)

geo <- stcd %>% 
  left_join(zipcode) %>% 
  left_join(countyFIPS)


# bind together ----
ccc <- geo %>%
  left_join(wgt) %>% 
  left_join(time) %>% 
  left_join(pid3) %>%
  left_join(pid7) %>% 
  left_join(gend) %>%
  left_join(bryr) %>%
  left_join(race) %>%
  left_join(educ) %>% 
  left_join(econ) %>% 
  left_join(apvpres) %>% 
  left_join(apvgov) %>% 
  left_join(apvrep) %>% 
  left_join(apvsen1) %>% 
  left_join(apvsen2) %>% 
  left_join(i_pres08) %>%
  left_join(i_pres12) %>%
  left_join(i_pres16) %>% 
  left_join(i_rep) %>% 
  left_join(i_sen) %>% 
  left_join(i_gov) %>%
  left_join(v_pres08) %>%
  left_join(v_pres12) %>%
  left_join(v_pres16) %>% 
  left_join(v_rep) %>% 
  left_join(v_sen) %>% 
  left_join(v_gov)

stopifnot(nrow(ccc) == nrow(pid3))

# Order 
ccc <- ccc %>%
  select(year:approval_gov,
         matches("_char$"),
         matches("_num$"))

# View(sample_n(ccc, 30) %>% arrange(year))




# Common manipulations ----
# Weight --
size_year <- ccc %>% 
  group_by(year) %>% 
  summarize(size = n()) %>% 
  mutate(size_factor = size / median(size)) # manageable constant -- divide by median

ccc <-  ccc  %>%
  left_join(select(size_year, year, size_factor)) %>% 
  mutate(weight_cumulative = weight / size_factor) %>% 
  select(-size_factor) %>%
  select(year, caseID, weight, weight_cumulative, everything())




# Format for output  --------
# make char variables a factor so crunch knows it's a categorical?
ccc_factor <- ccc %>% 
  mutate(caseID = as.character(caseID)) %>% # better this than let crunch think its a numeric
  mutate(zipcode = as.character(zipcode)) %>% 
  mutate(cdid = as.factor(cdid)) %>% # we don't want to take summary stats of this, so better a factor
  mutate(countyFIPS = as.character(countyFIPS)) %>%
  mutate_at(vars(matches("_char")), as.factor) %>%
  mutate_at(vars(matches("^CD$")), as.factor) %>%
  mutate_at(vars(matches("(state$|st$)")), as.factor)




# Write -----
saveRDS(ccc, "data/output/cumulative_2006_2016.Rds")
write_sav(ccc_factor, "data/release/cumulative_2006_2016.sav")
write_dta(ccc_factor, "data/release/cumulative_2006_2016.dta")




