# rm(list = ls())

library(haven)
library(dplyr)
library(readr)
library(foreach)


# READ ------

# 2012 and before (compiled by Stephen Pettigrew and others)
ccp <- std_dv("data/source/cces/2006_2012_cumulative.dta", 
              guess_year = FALSE)



# take the needed columns for 2013- 2016
cc13 <- std_dv("data/source/cces/2013_cc.dta")
cc14 <- std_dv("data/source/cces/2014_cc.dta")
cc15 <- std_dv("data/source/cces/2015_cc.dta")
cc16 <- std_dv("data/source/cces/2016_cc.dta")


# helper data -- 
statecode <- read_csv("~/Dropbox/cces_rollcall/data/source/statecode.csv")


# functions ----- 
# rudimentary standardization for data that comes out of dataverse
std_dv <- function(path, guess_year = TRUE) {
  
  if (guess_year) guessed_yr <- as.numeric(gsub(".*/([0-9]+)_cc.*", "\\1", path))
  
  ## then
  tbl <- haven::read_dta(path)
  
  
  ## guess ID
  if ("caseid" %in% colnames(tbl)) orig_key <- "caseid"
  if ("V101" %in% colnames(tbl)) orig_key <- "V101"
  

  # add year
  if (!"year" %in% colnames(tbl)) tbl <- mutate(tbl, year = guessed_yr)
    
  
  # then rename id
  tbl %>%
    rename(caseID = !!orig_key) %>%
    dplyr::select(year, caseID, everything())
  
}

# takes all datasets available, and given a var, pulls it out of each and stacks
findStack <- function(dflist = list(), var, type = "factor") {
  var <- enquo(var)
  
  var_name <- quo_name(var)
  chr_var_name <- paste0(quo_name(var), "_char")
  num_var_name <- paste0(quo_name(var), "_num")
  
  if (type == "factor") {
    list_yr <- 
      foreach(yr = 1:length(dflist), .combine = "bind_rows") %do% {
        if (quo_name(var) %in% colnames(dflist[[yr]]))  {
          dplyr::select(dflist[[yr]], year, caseID, !!var) %>%
            mutate(!!chr_var_name := as.character(as_factor(!!var)),
                   !!num_var_name := as.numeric(!!var)) %>% 
            dplyr::select(-!!var)  
        } else {
          dplyr::select(dflist[[yr]], year, caseID) %>%
            mutate(!!chr_var_name := NA,
                   !!num_var_name := NA)
        }
      }
  }
  
  
  
  if (type == "character") {
    list_yr <- 
      foreach(yr = 1:length(dflist), .combine = "bind_rows") %do% {
        
        if (quo_name(var) %in% colnames(dflist[[yr]]))  {
          dplyr::select(dflist[[yr]], year, caseID, !!var) %>%
            mutate(!!var_name := as.character(as_factor(!!var)))
        } else {
          dplyr::select(dflist[[yr]], year, caseID) %>%
            mutate(!!var_name := NA)
        }
      }
  }
  
  if (type == "numeric") {
    print(var_name)
    list_yr <- 
      foreach(yr = 1:length(dflist), .combine = "bind_rows") %do% {
        
        if (quo_name(var) %in% colnames(dflist[[yr]]))  {
          dplyr::select(dflist[[yr]], year, caseID, !!var) %>%
            mutate(!!var_name := as.integer(!!var))
        } else {
          dplyr::select(dflist[[yr]], year, caseID) %>%
            mutate(!!var_name := NA)
        }
      }
  }
  
  
  return(list_yr)
}


# more name standardization
stdName <- function(tbl){
  
    cces_year <- as.integer(unique(tbl$year))

    
    if (identical(cces_year, 2006:2012)) {
      tbl <- tbl %>% 
        rename(wgt = weight,
               state = state_pre,
               cdid = congdist_pre,
               zipcode = zip_pre,
               countyFIPS = county_fips_pre,
               reg_true = reg_validation,
               reg_self = registered_pre,
               validt_trn = gen_validated,
               voted_pres_08 = vote_pres_08,
               voted_rep = vote_house, 
               voted_sen = vote_sen,
               voted_gov = vote_gov,
               # intent_trn = vote_intent_general,
               intent_pres_08 = vote_intent_pres_08,
               intent_pres_12 = vote_intent_pres_12,
               intent_rep = vote_intent_house,
               intent_sen = vote_intent_senate,
               intent_gov = vote_intent_gov)
    }
    
    
    if (identical(cces_year, 2013L)) {
      tbl <- tbl %>% mutate(fips = floor(as.numeric(countyfips)/1000),
                            cdid = as.numeric(cdid113)) %>% 
        rename(approval_pres = CC312a,
               approval_rep = CC13_313a,
               approval_sen1 = CC13_313b,
               approval_sen2 = CC13_313c,
               approval_gov = CC312d,
               voted_pres_12 = CC13_315) %>% 
        left_join(statecode, by = "fips")
    }
    
    if (identical(cces_year, 2014L)) {
      tbl <- rename(tbl, 
                    approval_rep = CC14_315a,
                    approval_sen1 = CC14_315b,
                    # approval_sen2 = CC14_315c,
                    approval_gov = CC14_308d,
                    vote_rep = CC412,
                    voted_pres_12 = CC14_317,
                    intent_sen = CC355,
                    intent_senx = CC355x,
                    intent_gov = CC356,
                    intent_rep = CC360,
                    intent_repx = CC360x)
    }
    
    if (identical(cces_year, 2015L)) {
      tbl <- rename(tbl, CC350 = CC15_350) %>% 
        rename(approval_pres = CC15_312a,
               approval_rep = CC15_313a,
               approval_sen1 = CC15_313b,
               approval_sen2 = CC15_313c,
               approval_gov = CC15_312f,
               voted_pres_12 = CC15_315)
    }
    
    if (identical(cces_year, 2016L)) {
      tbl <- tbl %>% 
        rename(weight = commonweight,
               CC350 = CC16_360,
               cdid = cdid113,
               approval_pres = CC16_320a,
               approval_rep = CC16_320f,
               approval_sen1 = CC16_320g,
               approval_sen2 = CC16_320h,
               approval_gov = CC16_320d,
               voted_pres_12 = CC16_326,
               intent_trn = CC16_364,
               intent_pres_16 = CC16_364c,
               intent_pres_16x = CC16_364b,
               intent_rep = CC16_367,
               intent_repx = CC16_367x, # house early vote (already voted)
               intent_sen = CC16_365,
               intent_senx = CC16_365x,
               intent_gov = CC16_366,
               intent_govx = CC16_366x
               # voted_rep = ,
               # voted_sen = ,
               # voted_gov = ,
               # voted_pres_16 = ,
               ) 
      
    }
    

    # more standardization for post 2012
    if (cces_year[1] %in% 2013:2016) {
      tbl <- tbl %>% 
        rename(state = inputstate, 
               reg_self = votereg,
               family_income = faminc,
               marriage_status = marstat,
               wgt = weight,
               zipcode = lookupzip,
               countyFIPS = countyfips,
               partyreg = CC350) %>% 
        mutate(age = year - birthyr,
               countyFIPS = as.numeric(countyFIPS),
               cdid = as.numeric(cdid))
    }
   
    return(tbl)
}

# Start extracting variables -----
# in list form
ccs <- list(stdName(ccp), 
            stdName(cc13), 
            stdName(cc14), 
            stdName(cc15), 
            stdName(cc16))


# first same name vars -----
pid3 <- findStack(ccs, pid3)
pid7 <- findStack(ccs, pid7)
gend <- findStack(ccs, gender)
educ <- findStack(ccs, educ)
race <- findStack(ccs, race)
bryr <- findStack(ccs, birthyr, "numeric")
state <- findStack(ccs, state, "character")
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
# 
#   
# presapv
apvrep <- findStack(ccs, approval_rep)
apvsen1 <- findStack(ccs, approval_sen1)
apvsen2 <- findStack(ccs, approval_sen2)
apvgov <- findStack(ccs, approval_gov)

# format state and CD ----
stcd <- left_join(state, cdid) %>% 
  left_join(select(statecode, State, StateAbbr), by = c("state" = "State")) %>% 
  rename(st = StateAbbr) %>% 
  mutate(CD = paste0(st, "-", cdid)) %>% 
  select(year, caseID, state, st, cdid, CD)


# bind together ----
ccc <- stcd %>%
  left_join(pid3) %>%
  left_join(pid7) %>% 
  left_join(gend) %>%
  left_join(bryr) %>%
  left_join(race) %>%
  left_join(educ) %>% 
  left_join(apvrep) %>% 
  left_join(apvsen1) %>% 
  left_join(apvsen2) %>% 
  left_join(apvgov) %>% 
  left_join(pres08) %>%
  left_join(pres12) %>%
  left_join(pres16) %>% 
  left_join(voterep) %>% 
  left_join(votesen) %>% 
  left_join(votegov)
  



stopifnot(nrow(ccc) == nrow(pid3))


# Write dta -----

write_dta(ccc, "data/output/cumulative_2006_2016.dta")
write_csv(ccc, "data/output/cumulative_2006_2016.csv")
saveRDS(ccc, "data/output/cumulative_2006_2016.Rds")
