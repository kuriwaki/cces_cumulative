library(tidyverse)
library(haven)
library(foreach)
library(stringr)
library(glue)
library(lubridate)
library(data.table)


# helper data --
statecode <- read_csv("data/source/statecode.csv")

# functions -----

# name standardization
stdName <- function(tbl) {
  cces_year <- as.integer(unique(tbl$year))
  
  
  if (identical(cces_year, 2006:2011)) {
    tbl <- tbl %>%
      rename(
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
        vv_regstatus = reg_validation, # check for vv_st as well, cc06 has matchState
        vv_turnout_gvm = gen_validated,
        vv_turnout_pvm = prim_validated
      )
  }
  
  if (identical(cces_year, 2012L)) {
    tbl <- rename(
      tbl,
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
      vv_party_pprm = presprim_pty
    ) %>%
      mutate(
        voted_pres_12 = coalesce(voted_pres_12, intent_pres_12x),
        voted_rep = coalesce(voted_rep, intent_repx),
        voted_sen = coalesce(voted_sen, intent_senx),
        voted_gov = coalesce(voted_gov, intent_govx)
      )
  }
  
  if (identical(cces_year, 2013L)) {
    tbl <- tbl %>%
      rename(
        approval_pres = CC312a,
        approval_rep = CC13_313a,
        approval_sen1 = CC13_313b,
        approval_sen2 = CC13_313c,
        approval_gov = CC312d,
        economy_retro = CC13_302,
        voted_pres_12 = CC13_315
      )
  }
  
  if (identical(cces_year, 2014L)) {
    tbl <- rename(
      tbl,
      approval_pres = CC14_308a,
      approval_rep = CC14_315a,
      approval_sen1 = CC14_315b,
      approval_sen2 = CC14_313c,
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
      vv_st = state_cl
    ) %>%
      mutate(
        voted_rep = coalesce(voted_rep, intent_repx),
        voted_sen = coalesce(voted_sen, intent_senx),
        voted_gov = coalesce(voted_gov, intent_govx)
      )
  }
  
  if (identical(cces_year, 2015L)) {
    tbl <- rename(tbl, CC350 = CC15_350) %>%
      rename(
        approval_pres = CC15_312a,
        approval_rep = CC15_313a,
        approval_sen1 = CC15_313b,
        approval_sen2 = CC15_313c,
        approval_gov = CC15_312f,
        economy_retro = CC15_302,
        voted_pres_12 = CC15_315
      )
  }
  
  if (identical(cces_year, 2016L)) {
    tbl <- tbl %>%
      rename(
        weight = commonweight,
        CC350 = CC16_360,
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
      mutate(
        voted_pres_16 = coalesce(voted_pres_16, intent_pres_16x),
        voted_rep = coalesce(voted_rep, intent_repx),
        voted_sen = coalesce(voted_sen, intent_senx),
        voted_gov = coalesce(voted_gov, intent_gov)
      )
  }
  
  
  # more standardization for post 2012
  if (cces_year[1] %in% 2012:2016) {
    tbl <- tbl %>%
      rename(
        reg_self = votereg,
        family_income = faminc,
        marriage_status = marstat,
        zipcode = lookupzip,
        countyFIPS = countyfips,
        partyreg = CC350
      ) %>%
      mutate(
        age = year - birthyr)
  }
  
  return(tbl)
}


# inner extraction function for findStack
extract_yr <- function(tbl, var, var_name, chr_var_name, num_var_name, is_factor = TRUE) {
  if (is_factor) {
    if (var_name %in% colnames(tbl)) { # factor 
      select(tbl, year, caseID, !! var) %>%
        mutate(
          !! chr_var_name := as.character(as_factor(.data[[var_name]])),
          !! num_var_name := as.integer(.data[[var_name]])
        ) %>%
        select(-!! var)
    } else { # if var does not exist
      select(tbl, year, caseID) %>%
        mutate(
          !! chr_var_name := NA,
          !! num_var_name := NA
        )
    }
  } else { # if not factor
    if (var_name %in% colnames(tbl)) {
      select(tbl, year, caseID, !! var) %>%
        mutate(!! var_name := zap_labels(!! var))
    } else {
      select(tbl, year, caseID) %>%
        mutate(!! var_name := NA)
    }
  }
}

# takes all datasets available, and given a var, pulls it out of each and stacks
#' 
#' @param dflist a list of cces datasets. column names need to be standardized.
#' @param var a NSE variable to find and stack
#' @param type a string, "factor", "numeric", or "dattetime". If numeric or datetime
#'  that type is returned. if factor the label and values are returned separately as
#'  different columns, unless labelled = TRUE
#' @param makeLabelled to bind the two _char and _num columns to a label. ONLY
#'   do this if the numbers and labels match 1:1 across all years
findStack <- function(dflist = list(), var, type = "factor", makeLabelled = FALSE, newReorder = TRUE) {
  var <- enquo(var)
  var_name <- quo_name(var)
  chr_var_name <- paste0(var_name, "_char")
  num_var_name <- paste0(var_name, "_num")

  if (type == "factor") {
    list_yr <- foreach(yr = 1:length(dflist), .combine = "bind_rows") %do% {
      extract_yr(dflist[[yr]], enquo(var), var_name, chr_var_name, num_var_name)
    }
    
    if (grepl("^vv_", chr_var_name)) { # vv were not displayed questions so can be sorted by frequency, and no numeric left
      
      list_yr <- mutate(list_yr, 
                        !!chr_var_name := std_vvv(.data[[chr_var_name]], varname = chr_var_name, yrvec = .data[["year"]]))
      
      list_yr_factor <- list_yr %>% 
        clean_values(chr_var_name = chr_var_name, num_var_name = num_var_name) %>% 
        mutate(!!var_name := fct_infreq(.data[[chr_var_name]])) %>% 
        select(year, caseID, !!var_name)
      
      return(list_yr_factor)
    }
    
    # otherwise just clean up values as text
    list_yr <- clean_values(list_yr, chr_var_name = chr_var_name, num_var_name = num_var_name)
  }
    

  if (type != "factor") {
    list_yr <- foreach(yr = 1:length(dflist), .combine = "bind_rows") %do% {
      extract_yr(dflist[[yr]], enquo(var), var_name, chr_var_name, num_var_name, is_factor = FALSE)
    }

    # change to specified type
    if (type == "numeric") {
      list_yr <- mutate(list_yr, !! var_name := as.numeric(.data[[var_name]]))
    }

    if (type == "character") {
      list_yr <- mutate(list_yr, !! var_name := as.character(as_factor(!! var)))
    }

    if (type == "datetime") {
      list_yr <- mutate(list_yr, !! var_name := as.POSIXct(.data[[var_name]]))
    }

    list_yr <- list_yr %>%
      mutate(!! var_name := replace(.data[[var_name]], is.nan(.data[[var_name]]), NA))
  }

  # coerce to labelled? do this if same across year
  if (type == "factor" & makeLabelled) {

    # change consistent vars in to a labelled factor
    # make numbered vector
    key_arr <- select(list_yr, -year, -caseID) %>%
      distinct() %>%
      filter(!is.na(.data[[num_var_name]]))

    nvec <- key_arr %>% select(matches("num")) %>% pull()
    names(nvec) <- key_arr %>% select(matches("char")) %>% pull()
    list_yr <- list_yr %>%
      mutate(!! var_name := labelled(as.integer(.data[[num_var_name]]), sort(nvec))) %>%
      select(year, caseID, !! var_name)
  }
  
  # if not labelled, consider reordering rather than keeping them separate
  if (type == "factor" & newReorder & !makeLabelled) {
    
    # order x by y
    median2 <- function(x, y) {
      median(x[order(y, na.last = FALSE)])
    }
    
    list_yr <- list_yr %>% 
      mutate(!!var_name := fct_reorder2(.data[[chr_var_name]],
                                        x = .data[[num_var_name]],
                                        y = .data[["year"]], 
                                        fun = median2,
                                        .desc = FALSE)) %>% 
      select(year, caseID, !!var_name)
  }

  list_yr
}

#' clean up missing values, format to change NaN to NA
clean_values <- function(tbl, chr_var_name, num_var_name) {
  tbl %>%
    mutate( # replace NaN to NA
      !! chr_var_name := replace(.data[[chr_var_name]], .data[[chr_var_name]] == "NaN", NA),
      !! num_var_name := replace(.data[[num_var_name]], is.nan(.data[[num_var_name]]), NA)
    ) %>%
    mutate( # change al lvalues to title case
      !! chr_var_name := str_to_title(.data[[chr_var_name]]),
      !! chr_var_name := replace(.data[[chr_var_name]], .data[[chr_var_name]] == "Never Heard", "Never Heard Of This Person"),
      !! chr_var_name := replace(.data[[chr_var_name]], .data[[chr_var_name]] == "John Mccain", "John McCain"),
      !! chr_var_name := replace(.data[[chr_var_name]], .data[[chr_var_name]] == "Cynthia Mckinney", "Cynthia McKinney")
      ) %>% 
    mutate(
      !! chr_var_name := replace(.data[[chr_var_name]], .data[[chr_var_name]] == "No Hs", "No HS")
      )
}
#' standardize validated vote values 
std_vvv <- function (vec, varname, yrvec) {
  
  vtd <- "Voted"
  nov <- "No Record of Voting"
  # turnout
  if (grepl("turnout", varname)) {
    recoded <- recode(vec,
                      `Polling` = vtd,
                      `polling` = vtd,
                      `Absentee` = vtd,
                      `absentee` = vtd,
                      `Early` = vtd,
                      `earlyVote` = vtd,
                      `mail` = vtd,
                      `Mail` = vtd,
                      `unknown` = vtd,
                      `Unknown` = vtd,
                      `UnknownMethod` = vtd,
                      `validated record of voting in general election` = vtd,
                      `Virginia doesn't maintain vote history files` = "No Voter File",
                      `MatchedNoVote` = nov,
                      .default  = nov
                      )
  }
  
  # regstatus
  if (grepl("regstatus", varname)) {
    recoded <- recode(vec,
                      inactiv = "Inactive",
                      multipl = "Multiple Appearances",
                      multipleAppearances = "Multiple Appearances",
                      multipleAppe = "Multiple Appearances",
                      unregis = "unregistered",
                      MovedUnregistered = "unregistered",
                      dropped = "Dropped",
                      NoVoterFileMatch = "No Record of Registration"
    )
    recoded <- replace(recoded, yrvec %in% c(2006, 2007, 2009, 2011), NA) # these should be missing
    recoded <- replace(recoded, recoded == "", "No Record of Registration") 
  }
  
  
  # regstatus
  if (grepl("party", varname)) {
    dem <- "Democratic Party"
    gop <- "Republican Party"
    npa <- "No Party Affiliation"
    
    recoded <- recode(vec,
                      `DEM` = dem,
                      `Democratic.Party` = dem,
                      `Democratic` = dem,
                      `REP` = gop,
                      `Republican.Party` = gop,
                      `Republican` = gop,
                      `NPA` = npa,
                      `No.Party.Affiliation` = npa,
                      `Constitution.Party` = "Constitution Party",
                      `CST` = "Constitution Party",
                      `Green.Party` = "Green Party",
                      `GRE` = "Green Party",
                      `Libertarian.Party` = "Liberatarian Party",
                      `LIB` = "Liberatarian Party",
                      `Independent.Party` = "Independent Party",
                      `IND` = "Independent Party",
                      `Reform.Party` = "Reform Party",
                      `REF` = "Reform Party",
                      `Socialist.Party` = "Socialist Party",
                      `SOC` = "Socialist Party",
                      `Declined.To.State` = "Declined to State",
                      `DTS` = "Declined to State",
                      `OTH` = "Other",
                      `UNK` = "Unknown"
    )
    recoded <- replace(recoded, recoded == "", "No Record of Party Registration") 
  }
  
  recoded
} 



# READ ------
load("data/output/01_responses/common_all.RData")
pid3_cc10 <- readRDS("data/output/01_responses/pid3_cc10.Rds")


# execute name standardization -----

# in list form
ccs <- list(
  "pettigrew" = stdName(filter(ccp, year != 2012)),
  "2012" = stdName(cc12),
  "2013" = stdName(cc13),
  "2014" = stdName(cc14),
  "2015" = stdName(cc15),
  "2016" = stdName(cc16)
)

# mutations to data -----

# fix county misalignment
ccs[["pettigrew"]] <- ccs[["pettigrew"]] %>%
  mutate(countyFIPS = (countyFIPS < 1000) * as.numeric(state_pre) * 1000 + countyFIPS) %>% 
  mutate(countyFIPS = as.character(countyFIPS))


# Extract variable by variable iniitial version -----

# admin
wgt <- findStack(ccs, weight, "numeric")

time <- findStack(ccs, starttime, type = "datetime", makeLabelled = FALSE) %>%
  filter(year != 2006, year != 2009) %>%
  bind_rows(readRDS("data/source/cces/cc06_datetime.Rds")) %>%
  bind_rows(readRDS("data/source/cces/cc09_datetime.Rds"))


# demos
pid3 <- findStack(ccs, pid3) %>%
  filter(year != 2010) %>% # fix the missing 2010
  bind_rows(pid3_cc10) %>%
  mutate(pid3 = labelled(as.integer(pid3_num),
                         c(
                           "Democrat" = 1,
                           "Republican" = 2,
                           "Independent" = 3,
                           "Other" = 4,
                           "Not sure" = 5,
                           "Skipped" = 8
                         )
  )) %>%
  select(year, caseID, pid3) # manually do only this one

pid7 <- findStack(ccs, pid7, makeLabelled = TRUE)
gend <- findStack(ccs, gender, makeLabelled = TRUE)
educ <- findStack(ccs, educ, makeLabelled = TRUE)
race <- findStack(ccs, race, makeLabelled = TRUE)
bryr <- findStack(ccs, birthyr, "numeric")
age <- findStack(ccs, age, "numeric")

# geography
state      <- findStack(ccs, state, "character")
zipcode    <- findStack(ccs, zipcode, "character")
countyFIPS <- findStack(ccs, countyFIPS, "numeric")
cdid       <- findStack(ccs, cdid, "numeric")

# voting -- don't reorder because we'll need it to match candidates
i_pres08 <- findStack(ccs, intent_pres_08)
i_pres12 <- findStack(ccs, intent_pres_12)
i_pres16 <- findStack(ccs, intent_pres_16)

i_rep <- findStack(ccs, intent_rep, newReorder = FALSE)
i_sen <- findStack(ccs, intent_sen, newReorder = FALSE)
i_gov <- findStack(ccs, intent_gov, newReorder = FALSE)

v_pres08 <- findStack(ccs, voted_pres_08)
v_pres12 <- findStack(ccs, voted_pres_12)
v_pres16 <- findStack(ccs, voted_pres_16)

v_rep <- findStack(ccs, voted_rep, newReorder = FALSE)
v_sen <- findStack(ccs, voted_sen, newReorder = FALSE)
v_gov <- findStack(ccs, voted_gov, newReorder = FALSE)

# approval -- reorder and factor
apvpres <- findStack(ccs, approval_pres, makeLabelled = TRUE)
apvrep  <- findStack(ccs, approval_rep, makeLabelled = FALSE) # slightly different labels ax years
apvsen1 <- findStack(ccs, approval_sen1, makeLabelled = FALSE)
apvsen2 <- findStack(ccs, approval_sen2, makeLabelled = FALSE)
apvgov  <- findStack(ccs, approval_gov, makeLabelled = TRUE)

# other
econ <- findStack(ccs, economy_retro, makeLabelled = TRUE)

# validated vote
vv_regstatus   <- findStack(ccs, vv_regstatus, newReorder = FALSE) # will reorder by frequency later
vv_party_gen   <- findStack(ccs, vv_party_gen, newReorder = FALSE)
vv_party_prm   <- findStack(ccs, vv_party_prm, newReorder = FALSE)
vv_turnout_gvm <- findStack(ccs, vv_turnout_gvm, newReorder = FALSE)
vv_turnout_pvm <- findStack(ccs, vv_turnout_pvm, newReorder = FALSE)




# format state and CD, then zipcode and county ----
stcd <- left_join(state, cdid) %>%
  left_join(select(statecode, state, st), by = "state") %>%
  mutate(cdid = as.integer(cdid),
         CD = glue("{st}-{cdid}")) %>%
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
  left_join(age) %>%
  left_join(race) %>%
  left_join(educ) %>%
  left_join(econ) %>%
  left_join(apvpres) %>%
  left_join(apvrep) %>%
  left_join(apvsen1) %>%
  left_join(apvsen2) %>%
  left_join(apvgov) %>%
  left_join(i_pres08) %>%
  left_join(i_pres12) %>%
  left_join(i_pres16) %>%
  left_join(v_pres08) %>%
  left_join(v_pres12) %>%
  left_join(v_pres16) %>%
  left_join(vv_regstatus) %>%
  left_join(vv_party_gen) %>%
  left_join(vv_party_prm) %>%
  left_join(vv_turnout_gvm) %>%
  left_join(vv_turnout_pvm)


stopifnot(nrow(ccc) == nrow(pid3))



# Common manipulations ----
# Weight --
size_year <- ccc %>%
  group_by(year) %>%
  summarize(size = n()) %>%
  mutate(size_factor = size / median(size)) # manageable constant -- divide by median

ccc <- ccc %>%
  left_join(select(size_year, year, size_factor)) %>%
  mutate(weight_cumulative = weight / size_factor) %>%
  select(-size_factor) %>%
  select(year, caseID, weight, weight_cumulative, everything())



# Write -----
save(i_rep, i_sen, i_gov, v_rep, v_sen, v_gov, file = "data/output/01_responses/vote_responses.RData")
save(vv_party_gen, vv_party_prm, vv_regstatus, vv_turnout_gvm, vv_turnout_pvm, file = "data/output/01_responses/vv_responses.RData")
saveRDS(ccc, "data/output/01_responses/cumulative_stacked.Rds")
