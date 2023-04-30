library(tidyverse)
library(labelled)
library(haven)
library(foreach)
library(stringr)
library(glue)
library(lubridate)
library(questionr)

stopifnot(packageVersion("labelled") >= "2.4.0")


# functions -----

# name standardization
std_name <- function(tbl, is_panel = FALSE) {
  cces_year <- as.integer(unique(tbl$year))
  if (is_panel) cces_year <- paste0(cces_year, "_", "panel")
  
  
  # pettigrew ------
  if (identical(cces_year, 2006:2011)) {
    tbl <- tbl %>%
      rename(
        tookpost = survey_complete,
        zipcode = zip_pre,
        county_fips = county_fips_pre,
        starttime = start_pre,
        reg_self = registered_pre,
        economy_retro = economy_retrospective,
        newsint = news_interest,
        marstat = marriage_status,
        religpew = religion,
        pew_religimp = relig_importance,
        pew_bornagain = born_again,
        religpew_protestant = relig_protestant,
        pew_churatd = church_attendence,
        voted_pres_08 = vote_pres_08,
        voted_rep = vote_house,
        voted_sen = vote_sen,
        voted_gov = vote_gov,
        intent_pres_08 = vote_intent_pres_08,
        intent_pres_12 = vote_intent_pres_12,
        intent_rep = vote_intent_house,
        intent_sen = vote_intent_senate,
        intent_gov = vote_intent_gov,
        immstat = immigration_status,
        unionhh = union_household,
        ownhome = home_owner,
        child18 = children,
        milstat_5 = military_none,
        healthins_6 = health_ins_none,
        vv_regstatus = reg_validation, # check for vv_st as well, cc06 has matchState
        vv_turnout_gvm = gen_validated,
        vv_turnout_pvm = prim_validated
      ) %>% 
      mutate(
        voted_trn = coalesce(as_factor(turnout_10), as_factor(turnout_08), as_factor(turnout_06)),
        voted_trn = replace(voted_trn, !year %in% c(2006, 2008, 2010), NA),
        tookpost  = replace(tookpost, year %% 2 == 1, NA), #  % NA for odd years
        voted_rep = replace(voted_rep, year %% 2 == 1, NA), #  % NA for odd years
        voted_sen = replace(voted_sen, year %% 2 == 1, NA)) #  % NA for odd years
  }
  
  # 2008 -------
  if (identical(cces_year, 2008L)) {
    tbl <- tbl %>%
      rename(
        approval_pres = CC335BUSH,
        approval_rep = CC335REP,
        approval_sen1 = CC335SEN1,
        approval_sen2 = CC335SEN2,
        approval_gov = CC335GOV,
        economy_retro = CC302,
        family_income_old = V246,
        marstat = V214,
        starttime = V300,
        intent_trn = CC326,
        voted_trn = CC401,
        intent_rep = CC339,
        intent_sen  = CC335,
        intent_gov  = CC336,
        intent_pres_08 = CC327,
        voted_pres_08 = CC410,
        pid3 = CC307,
        pid7 = CC307A,
        ideo5 = V243,
        weight = V201,
        educ = V213,
        newsint = V244,
        gender = V208,
        birthyr = V207,
        race = V211,
        partyreg =  CC402
      ) %>% 
      mutate(zipcode = as.character(as_factor(V202)))
  }
  
  # 2006 ------
  if (identical(cces_year, 2006L)) {
    tbl <- tbl %>%
      rename(
        approval_pres = gwbapp,
        approval_rep = congmanapp,
        approval_sen1 = sen1app,
        approval_sen2 = sen2app,
        approval_gov = govapp,
        economy_retro = natleconyear,
        family_income_old = income,
        zipcode = inputzip,
        county_fips = profile_fips,
        intent_trn = vote2006,
        voted_trn = v4004,
        intent_rep = profile_housevote_coded,
        intent_sen  = profile_senvote_coded,
        intent_gov  = profile_govvote_coded,
        vv_turnout_gvm = g2006
      ) %>% 
      mutate(marstat = coalesce(profile_marstat, marstat)) %>% 
      labelled::add_value_labels(race = c("Other" = 7)) %>% 
      labelled::add_value_labels(pid7 = c("Not Very Strong Democrat" = 2,
                                          "Not Very Strong Republican" = 6))
  }
  
  # 2009 --------
  if (identical(cces_year, 2009L)) {
    tbl <- tbl %>%
      rename(
        approval_pres = cc09_43e,
        approval_rep = cc09_43a,
        approval_sen1 = cc09_43b,
        approval_sen2 = cc09_43c,
        approval_gov = cc09_43f,
        economy_retro = cc09_20,
        voted_pres_08 = cc09_31,
        pid3 = cc423,
        pid7 = cc424,
        ideo5 = v261,
        weight = v200,
        educ = v213,
        newsint = v244,
        marstat = v214,
        family_income_old = v246,
        gender = v208,
        age = v288,
        birthyr = v207,
        race = v211,
      ) %>% 
      mutate(zipcode = as.character(as_factor(v253)),
             county_fips = as.character(as_factor(v269)))
  }
  
  # 2012 ----------
  if (identical(cces_year, 2012L)) {
    tbl <- rename(
      tbl,
      weight = weight_vv,
      weight_post = weight_vv_post,
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
      voted_trn = CC401,
      intent_sen = CC355b,
      intent_senx = CC355,
      intent_gov = CC356b,
      intent_govx = CC356,
      intent_rep = CC390b,
      intent_repx = CC390,
      intent_trn = CC354,
      vv_turnout_gvm = e2012g,
      vv_turnout_pvm = e2012congp,
      vv_turnout_ppvm = e2012presp,
      vv_regstatus = voter_status,
      vv_party_gen = PartyRegist,
      vv_party_prm = congprim_pty,
      vv_party_pprm = presprim_pty
    )
  }
  
  
  if (identical(cces_year, "2012_panel")) {
    tbl <- tbl %>% 
      rename(
        approval_pres = CC12_308a,
        approval_rep = CC12_315a,
        approval_sen1 = CC12_315b,
        approval_sen2 = CC12_315c,
        approval_gov = CC12_308d,
        economy_retro = CC12_302,
        intent_pres_12 = CC12_354c,
        intent_pres_12x = CC12_354b,
        voted_pres_12 = CC12_410a,
        voted_pres_08 = CC12_317,
        voted_rep = CC12_412,
        voted_sen = CC12_410b,
        voted_gov = CC12_411,
        intent_sen = CC12_355b,
        intent_senx = CC12_355,
        intent_gov = CC12_356b,
        intent_govx = CC12_356,
        intent_rep = CC12_390b,
        intent_repx = CC12_390,
        CC350 = CC12_350 # rename later
      )
  }
  
  # 2013 ----------
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
  
  # 2014 - 2015 -------
  if (identical(cces_year, 2014L)) {
    tbl <- tbl %>% rename(
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
      intent_trn = CC354,
      voted_trn = CC401,
      vv_turnout_gvm = e2014gvm,
      vv_turnout_pvm = e2014pvm,
      vv_regstatus = voterstatus,
      vv_party_gen = partyaffiliation,
      vv_party_prm = e2014pep,
      vv_st = state_cl
    ) %>%
      mutate(
        tookpost = labelled(tookpost, labels = c(`1` = 1, `0` = 0)) # for consistency
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
  
  # 2016 - 2017  ----
  if (identical(cces_year, 2016L)) {
    tbl <- tbl %>%
      rename(
        weight = commonweight_vv,
        weight_post = commonweight_vv_post,
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
      )
  }
  
  if (identical(cces_year, 2017L)) {
    tbl <- rename(tbl, CC350 = CC17_325) %>%
      rename(
        weight = weights_common,
        approval_pres = CC17_322a,
        approval_rep = CC17_323a,
        approval_sen1 = CC17_323b,
        approval_sen2 = CC17_323c,
        approval_gov = CC17_322e,
        economy_retro = CC17_301,
        faminc = faminc_new,
        voted_pres_16 = CC17_327) %>%
      mutate(
        countyfips = NA
      ) %>% 
      labelled::add_value_labels(marstat = c("Domestic Partnership" = 6, "Single" = 5))
  }
  
  # 2018 - 2019 ------
  if (identical(cces_year, 2018L)) {
    
    tbl <- tbl %>%
      rename(
        weight = commonweight,
        rvweight = vvweight,
        rvweight_post = vvweight_post,
        weight_post = commonpostweight,
        approval_pres = CC18_308a,
        approval_rep = CC18_311a,
        approval_sen1 = CC18_311b,
        approval_sen2 = CC18_311c,
        approval_gov = CC18_308d,
        economy_retro = CC18_301,
        faminc = faminc_new,
        intent_trn = CC18_350,
        intent_rep = CC18_353,
        intent_repx = CC18_353x,
        intent_sen = CC18_351,
        intent_senx = CC18_351x,
        intent_gov = CC18_352,
        intent_govx = CC18_352x,
        voted_trn = CC18_401,
        voted_pres_16 = CC18_317,
        voted_rep = CC18_412,
        voted_sen = CC18_410b,
        voted_gov = CC18_411,
        vv_turnout_gvm = CL_2018gvm,
        vv_turnout_pvm = CL_2018pvm,
        vv_regstatus = CL_voter_status,
        vv_party_gen = CL_party,
        vv_party_prm = CL_2018pep,
        vv_st = CL_state
      ) %>% 
      # party straight ticket
      mutate(voted_rep = replace(voted_rep, CC18_409 == 1, 1),
             voted_rep = replace(voted_rep, CC18_409 == 2, 2),
             # we found that only in US House, sometimes party 2 was not a Republican.
             voted_rep = replace(voted_rep, CC18_409 == 2 & HouseCand2Party_post != "Republican", NA),
             voted_sen = replace(voted_sen, CC18_409 == 1, 1),
             voted_sen = replace(voted_sen, CC18_409 == 2, 2),
             voted_gov = replace(voted_gov, CC18_409 == 1, 1),
             voted_gov = replace(voted_gov, CC18_409 == 2, 2),
      ) %>%
      mutate_at(vars(matches("^vv")), ~replace_na(as.character(as_factor(.x)), "")) %>% 
      labelled::add_value_labels(marstat = c("Domestic Partnership" = 6, "Single" = 5))
  }
  
  if (identical(cces_year, 2019L)) {
    tbl <- tbl %>%
      rename(
        weight = commonweight,
        approval_pres = CC19_308a,
        approval_rep = CC19_311a,
        approval_sen1 = CC19_311b,
        approval_sen2 = CC19_311c,
        approval_gov = CC19_308e,
        economy_retro = CC19_301,
        faminc = faminc_new,
        voted_pres_16 = presvote16post
      ) %>%
      labelled::add_value_labels(marstat = c("Domestic Partnership" = 6, "Single" = 5))
  }
  
  # 2020 - 2021 ----
  if (identical(cces_year, 2020L)) {
    
    tbl <- tbl %>%
      # called "Two or more races" in 2020
      mutate(race = sjlabelled::replace_labels(
        race, labels = c("Mixed" = 6))) %>%
      # rename
      rename(
        weight = commonweight,
        rvweight = vvweight,
        rvweight_post = vvweight_post,
        weight_post = commonpostweight,
        approval_pres = CC20_320a,
        approval_rep = CC20_320f,
        approval_sen1 = CC20_320g,
        approval_sen2 = CC20_320h,
        approval_gov = CC20_320d,
        economy_retro = CC20_302,
        faminc = faminc_new,
        intent_trn = CC20_363,
        intent_pres_20 = CC20_364b,
        intent_pres_20x = CC20_364a, # double check if this is actually voted
        intent_rep = CC20_367,
        intent_repx = CC20_367_voted,
        intent_sen = CC20_365,
        intent_senx = CC20_365_voted,
        intent_gov = CC20_366,
        intent_govx = CC20_366_voted,
        voted_trn = CC20_401,
        voted_pres_16 = presvote16post,
        voted_pres_20 = CC20_410,
        voted_rep = CC20_412,
        voted_sen = CC20_411,
        voted_gov = CC20_413,
        vv_turnout_gvm = CL_2020gvm,
        vv_turnout_pvm = CL_2020pvm,
        vv_turnout_ppvm = CL_2020ppvm,
        vv_regstatus = CL_voter_status,
        vv_party_gen = CL_party,
        vv_party_prm = CL_2020pep,
        vv_party_pprm = CL_2020ppep,
        vv_st = CL_state
      ) %>% # combine early vote?
      mutate_at(vars(matches("^vv")), ~replace_na(as.character(as_factor(.x)), "")) %>% 
      labelled::add_value_labels(marstat = c("Domestic Partnership" = 6, "Single" = 5))
  }
  
  if (identical(cces_year, 2021L)) {
    tbl <- tbl %>%
      # called "Two or more races" in 2020-2021
      mutate(race = sjlabelled::replace_labels(
        race, labels = c("Mixed" = 6))) %>%
      rename(
        weight = commonweight,
        approval_pres = CC21_315a,
        approval_rep = CC21_315f,
        approval_sen1 = CC21_315g,
        approval_sen2 = CC21_315h,
        approval_gov = CC21_315d,
        economy_retro = CC21_301,
        faminc = faminc_new,
        voted_pres_16 = presvote16post,
        voted_pres_20 = presvote20post
      ) %>%
      labelled::add_value_labels(marstat = c("Domestic Partnership" = 6, "Single" = 5))
  }
  
  # more standardization for post 2012 ------
  if (cces_year[1] %in% c(2012:2021) | cces_year[1] == "2012_panel") {
    tbl <- tbl %>%
      rename(
        reg_self = votereg,
        family_income = faminc,
        zipcode = lookupzip,
        county_fips = countyfips
      ) %>%
      mutate(
        age = year - birthyr
      )
  }
  
  return(tbl)
}


# inner extraction function for find_stack
#' @param tbl data
#' @param var NSE variable name to look for
#' @param var_name in characters
#' @param chr_var_name name of the variablethat  will hold the labels (in chars)
#' @param num_var_name name of the variable that will hold the levels (in integers)
#' 
#' @return 
#' A dataset, for each year, with standardized names, separating labels and values (if factor).
#' Sets Not Asked to NA
extract_yr <- function(tbl, var, var_name, chr_var_name, num_var_name, is_factor = TRUE) {
  if (is_factor) {
    if (var_name %in% colnames(tbl)) { # factor 
      select(tbl, year, case_id, !! var) %>%
        mutate(
          !! chr_var_name := as.character(as_factor(.data[[var_name]])),
          !! num_var_name := as.integer(.data[[var_name]])
        ) %>%
        select(-!! var)
    } else {# if var does not exist
      select(tbl, year, case_id) %>%
        mutate(
          !! chr_var_name := NA,
          !! num_var_name := NA
        )
    }
  } else {# if not factor
    if (var_name %in% colnames(tbl)) {
      select(tbl, year, case_id, !! var) %>%
        mutate(!! var_name := zap_labels(!! var))
    } else {
      select(tbl, year, case_id) %>%
        mutate(!! var_name := NA)
    }
  }
}

# takes all datasets available, and given a var, pulls it out of each and stacks
#' 
#' @param dflist a list of cces datasets. column names need to be standardized.
#' @param var a NSE variable to find and stack
#' @param type a string, "factor", "numeric", "integer", or "dattetime". If numeric or datetime
#'  that type is returned. if factor the label and values are returned separately as
#'  different columns, unless labelled = TRUE
#' @param make_labelled to bind the two _char and _num columns to a label. ONLY
#'   do this if the numbers and labels match 1:1 across all years
find_stack <- function(dflist = list(), var, type = "factor", make_labelled = FALSE, new_reorder = TRUE) {
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
        select(year, case_id, !!var_name)
      
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
    if (type == "numeric") list_yr <- mutate(list_yr, !! var_name := as.numeric(.data[[var_name]]))
    
    if (type == "integer") list_yr <- mutate(list_yr, !! var_name := as.integer(.data[[var_name]]))
    
    if (type == "character") list_yr <- mutate(list_yr, !! var_name := as.character(as_factor(!! var)))
    
    if (type == "datetime") list_yr <- mutate(list_yr, !! var_name := as.POSIXct(.data[[var_name]]))
    
    list_yr <- list_yr %>%
      mutate(!! var_name := replace(.data[[var_name]], is.nan(.data[[var_name]]), NA))
  }
  
  # specific recoding
  if (grepl("approval_(rep|sen|pres|gov)", chr_var_name)) {
    list_yr <- recode_apv(list_yr, chr_var_name)
  }
  
  
  # fit to labels or factors
  # coerce to labelled? do this if same across year
  if (type == "factor" & make_labelled) list_yr <- set_to_label(list_yr, num_var_name, var_name)
  
  # if not labelled, consider reordering rather than keeping them separate
  if (type == "factor" & new_reorder & !make_labelled) {
    
    list_yr <- list_yr %>% 
      mutate(!!var_name := fct_reorder2(.data[[chr_var_name]],
                                        .x = .data[[num_var_name]],
                                        .y = .data[["year"]], 
                                        .fun = median2,
                                        .na_rm = FALSE,
                                        .desc = FALSE)) %>% 
      select(year, case_id, !!var_name)
  }
  
  
  list_yr
}


#' Coerce-labelled 
#' 
#' @param df the slim table
#' @param numvarname the name of the variable (number)

set_to_label <- function(df, numvarname, varname) {
  # change consistent vars in to a labelled factor
  # make numbered vector
  label_key <- select(df, -year, -case_id) %>%
    distinct() %>%
    select(matches("_char"), matches("_num")) %>%
    arrange(!!sym(numvarname)) %>% 
    deframe()
  
  df %>%
    mutate(!! varname := labelled(as.integer(.data[[numvarname]]), label_key)) %>%
    select(year, case_id, !! varname)
}


# order x by y
median2 <- function(x, y) {
  median(x[order(y, na.last = FALSE)], na.rm = TRUE)
}


#' recode approval
#' 
#' 
recode_apv <- function(tbl, char_name) {
  tbl %>% 
    mutate(!!char_name := recode(.data[[char_name]],
                                 `Approve`              = "Approve / Somewhat Approve", 
                                 `Somewhat Approve`     = "Approve / Somewhat Approve", 
                                 `Disapprove`           = "Disapprove / Somewhat Disapprove", 
                                 `Somewhat Disapprove`  = "Disapprove / Somewhat Disapprove", 
                                 `Never Heard`          = "Never Heard / Not Sure",
                                 `Never Heard Of`       = "Never Heard / Not Sure",
                                 `Not Sure`             = "Never Heard / Not Sure"))
}


#' clean up missing values, format to change NaN to NA
clean_values <- function(tbl, chr_var_name, num_var_name) {
  if (grepl("approval|hispanic", chr_var_name)) {
    skipped_num  <- 8
    notasked_num <- 9
  } else {
    skipped_num  <- 98
    notasked_num <- 99
  }
  
  tbl %>%
    mutate( # replace NaN to NA
      !! chr_var_name := na_if(.data[[chr_var_name]], "NaN"),
      !! num_var_name := replace(.data[[num_var_name]], is.nan(.data[[num_var_name]]), NA)
    ) %>%
    mutate( # change al lvalues to title case
      !! chr_var_name := my_var_case(str_trim(.data[[chr_var_name]])),
      !! chr_var_name := recode(.data[[chr_var_name]],
                                `Never Heard Of This Person` = "Never Heard",
                                `No Hs` = "No HS")
    ) %>% 
    mutate( # change not asked and skipped to NAs
      !! chr_var_name := na_if(.data[[chr_var_name]], "Not Asked"),
      !! chr_var_name := na_if(.data[[chr_var_name]], "Skipped"),
      !! num_var_name := na_if(.data[[num_var_name]], notasked_num),
      !! num_var_name := na_if(.data[[num_var_name]], skipped_num))
}

#' custom title case
my_var_case <- function(chr) {
  str_to_title(chr) %>% 
    str_replace_all(" A ", " a ") %>% 
    str_replace_all(" An ", " an ") %>% 
    str_replace_all(" About ", " about ") %>% 
    str_replace_all(" By ", " by ") %>% 
    str_replace_all(" And ", " and ") %>% 
    str_replace_all(" For ", " for ") %>% 
    str_replace_all(" To ", " to ") %>% 
    str_replace_all(" Or ", " or ") %>% 
    str_replace_all(" Of ", " of ") %>% 
    str_replace_all(" In ", " in ") %>% 
    str_replace_all(" This ", " this ") %>% 
    str_replace_all(" The ", " the ") %>% 
    str_replace_all(" At ", " at ") %>% 
    str_replace_all(" Nor ", " nor ") %>% 
    str_replace_all(" Not ", " not ") %>% 
    str_replace_all("I'm not Sure", "Not Sure") %>% 
    str_replace_all("Most of the Time", "Most of the time") %>% 
    str_replace_all("Some of the Time", "Some of the time") %>% 
    str_replace_all("Only Now and Then", "Only now and then") %>% 
    str_replace_all("Mccain", "McCain") %>% 
    str_replace_all("Hardly at All", "Hardly at all")
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
  
  
  # regparty
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
                      `Libertarian.Party` = "Libertarian Party",
                      `LIB` = "Libertarian Party",
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

#' Fix for voted08
clps_pres08 <- function(vec) {
  vec %>% 
    as.character() %>% 
    str_trim() %>% 
    as_factor() %>% 
    fct_collapse(
      `Barack Obama` = c("Barack Obama", 
                         "Barack Obama (Democratic)"),
      `John McCain` = c("John McCain (Republican)", 
                        "John McCain"),
      `Other / Someone Else` = c("Someone Else"), 
      `Did not Vote` = c("Did not Vote"),
      `Not Sure / Don't Recall` = c("Don't Recall")
    ) %>% 
    fct_relevel("Barack Obama", "John McCain", "Other / Someone Else", "Did not Vote") %>% 
    fct_drop()
}

#' Quick fix for 2012 voted, where labels are too mixed to fix automatically
clps_pres12 <- function(vec) {
  fct_collapse(
    vec, 
    `Barack Obama` = c(
      "Barack Obama", 
      "Barack Obama (Democratic)", 
      "Vote for Barack Obama"),
    `Mitt Romney` = c(
      "Mitt Romney", 
      "Mitt Romney (Republican)", 
      "Vote for Mitt Romney"),
    `Other / Someone Else` = c(
      "Someone Else", 
      "Vote for Someone Else", 
      "Other"),
    `Did not Vote for this Office` = c(
      "Did Not Vote", 
      "I Did Not Vote", 
      "Not Vote", 
      "Not Vote for this Office", 
      "I Did not Vote in this Race"),
    `Not Sure / Don't Recall` = c("Not Sure", "Don't Recall")
  ) %>% 
    fct_lump(n = 6)
}
clps_pres16 <- function(vec) {
  fct_collapse(
    vec, 
    `Hilary Clinton` = c(
      "Hillary Clinton", 
      "Hillary Clinton (Democrat)"),
    `Donald Trump` = c(
      "Donald Trump", 
      "Donald Trump (Republican)"),
    `Other / Someone Else` = c(
      "Gary Johnson (Libertarian)", "Gary Johnson",
      "Evan Mcmullin (Independent)", "Evan Mcmullin",
      "Jill Stein (Green)", "Jill Stein",
      "Other", "Someone Else"),
    `Did not Vote for this Office` = c(
      "I Didn't Vote in this Election",
      "Did not Vote for President",
      "I Did not Cast a Vote for President"),
    `Not Sure / Don't Recall` = c(
      "I'm not Sure", "I Don't Recall")
  ) %>% 
    fct_relevel("Hilary Clinton", "Donald Trump") %>% 
    fct_lump(n = 5)
}

clps_pres20 <- function(vec) {
  fct_collapse(
    vec, 
    `Joe Biden` = c(
      "Joe Biden", 
      "Joe Biden (Democrat)"),
    `Donald Trump` = c(
      "Donald Trump", 
      "Donald Trump (Republican)", 
      "Donald J. Trump (Republican)"),
    `Other / Someone Else` = c(
      "Other", 
      "Someone Else", 
      "Jo Jorgensen", 
      "Howie Hawkins"),
    `Did not Vote for this Office` = c(
      "I Did not Vote in this Race",
      "I Did not Vote",
      "Did Not Vote for President"),
    `Not Sure / Don't Recall` = c("I'm not Sure")
  ) %>% 
    fct_relevel("Joe Biden", "Donald Trump") %>% 
    fct_lump(n = 5)
}

#' give pres party from chars of pres names
pres_names <- function(vec) {
  case_when(
    str_detect(vec, regex("(Obama|Clinton|Biden)", ignore_case = TRUE)) ~ "Democratic",
    str_detect(vec, regex("(Mccain|Romney|Trump)", ignore_case = TRUE)) ~ "Republican",
    str_detect(vec, regex("(Mckinney|Paul|Barr|Stein|Johnson)", ignore_case = TRUE)) ~ "Third Party",
    str_detect(vec, regex("(McMullin|Nader)", ignore_case = TRUE)) ~ "Independent",
    str_detect(vec, regex("Other", ignore_case = TRUE)) ~ "Other Candidate",
    str_detect(vec, regex("Did Not", ignore_case = TRUE)) ~ "Did not Vote",
    TRUE ~ NA_character_) %>% 
    factor(levels = c("Democratic", "Republican", "Third Party", "Independent", "Other Candidate", "Did not Vote"))
}

# READ ------
load("data/output/01_responses/common_all.RData")
cc06_time <- readRDS("data/output/01_responses/cc06_datetime.Rds")
cc09_time <- readRDS("data/output/01_responses/cc09_datetime.Rds")
cc10_pid3 <- readRDS("data/output/01_responses/cc10_pid3.Rds")
cc09_econ <- readRDS("data/output/01_responses/cc09_econ_retro.Rds")
cc17_county <- read_csv("data/source/cces/CCES17_Common_county.csv", show_col_types = FALSE) |>
  transmute(year = 2017, case_id = V101, countyfips)

# execute name standardization -----

# in list form
ccs <- list(
  "pettigrew" = std_name(filter(ccp, year != 2012)),
  # "2006mit" = std_name(mit06_add),
  # "2008hu" = std_name(hu08),
  "2009hu" = std_name(hu09),
  "2012" = std_name(cc12),
  "2012panel" = std_name(panel12, is_panel = TRUE),
  "2013" = std_name(cc13),
  "2014" = std_name(cc14),
  "2015" = std_name(cc15),
  "2016" = std_name(cc16),
  "2017" = std_name(cc17),
  "2018" = std_name(cc18),
  "2018comp" = std_name(mutate(cc18_cnew,
                               commonweight = NA,
                               commonpostweight = NA,
                               vvweight = NA,
                               vvweight_post = NA)),
  "2019" = std_name(cc19),
  "2020" = std_name(cc20),
  "2021" = std_name(cc21)
)





# mutations to data -----

# fix county misalignment
ccs[["pettigrew"]] <- ccs[["pettigrew"]] %>%
  mutate(county_fips = (county_fips < 1000) * as.numeric(state_pre) * 1000 + county_fips) %>% 
  mutate(county_fips = as.character(county_fips))


# Extract variable by variable iniitial version -----

# admin ------
wgt        <- find_stack(ccs, weight, "numeric")
wgt_post   <- find_stack(ccs, weight_post, "numeric")

vwgt        <- find_stack(ccs, rvweight, "numeric")
vwgt_post <- find_stack(ccs, rvweight_post, "numeric")

tookpost <- find_stack(ccs, tookpost, make_labelled =  FALSE, new_reorder = FALSE) %>% 
  mutate(tookpost = labelled(as.integer(tookpost_num == 1 & year < 2018 | 
                                          tookpost_num == 2 & year %in% c(2018, 2020)), # diff number in 2018
                             labels = c("Took Post-Election Survey" = 1,
                                        "Did Not Take Post-Election Survey" = 0))) %>% 
  mutate(tookpost  = replace(tookpost, year %% 2 == 1, NA)) %>% 
  select(year, case_id, tookpost)

time <- find_stack(ccs, starttime, type = "datetime") %>%
  filter(year != 2006, year != 2009) %>% 
  bind_rows(cc06_time, cc09_time)

# pid -------
pid3_labels <- c("Democrat" = 1,  "Republican" = 2, "Independent" = 3,
                 "Other" = 4, "Not Sure" = 5)

pid3 <- find_stack(ccs, pid3, make_labelled = FALSE, new_reorder = FALSE) %>%
  filter(year != 2010) %>% # fix the missing 2010
  bind_rows(cc10_pid3) %>%
  mutate(pid3_num = na_if(pid3_num, 8)) %>%
  mutate(pid3_num = na_if(pid3_num, 9)) %>%
  mutate(pid3 = labelled(as.integer(pid3_num), pid3_labels)) %>%
  select(year, case_id, pid3) # manually do only this one

pid7 <- find_stack(ccs, pid7, make_labelled = TRUE)

# put leaners into partisans
leaner_lbl_code <- c(`Democrat (Including Leaners)` = 1L,
                     `Republican (Including Leaners)` = 2L,
                     `Independent (Excluding Leaners)` = 3L,
                     `Not Sure` = 8L)
pid3_leaner <- pid7 %>%
  mutate(pid3_leaner = as_factor(pid7)) %>% 
  mutate(pid3_leaner = fct_collapse(pid3_leaner, 
                                    "Republican (Including Leaners)" = c("Strong Republican", "Not Very Strong Republican", "Lean Republican"),
                                    "Democrat (Including Leaners)" = c("Strong Democrat", "Not Very Strong Democrat", "Lean Democrat"),
                                    "Independent (Excluding Leaners)" = "Independent")) %>% 
  mutate(pid3_leaner_num = recode(pid3_leaner, !!!leaner_lbl_code)) %>% 
  mutate(pid3_leaner = labelled(pid3_leaner_num, leaner_lbl_code)) %>% 
  select(-pid3_leaner_num, -pid7)

ideo5 <- find_stack(ccs, ideo5)

# demographics ----

gend <- find_stack(ccs, gender, make_labelled = TRUE)
educ <- find_stack(ccs, educ, make_labelled = TRUE)
race <- find_stack(ccs, race, make_labelled = TRUE)
hisp <- find_stack(ccs, hispanic, make_labelled = TRUE)
bryr <- find_stack(ccs, birthyr, "integer")
age <- find_stack(ccs, age, "integer")

# income wrangling -----
inc_old <- find_stack(ccs, family_income_old, "integer", make_labelled = FALSE) %>%
  mutate(faminc = recode(
    family_income_old,
    `1` = "Less than 10k",
    `2` = "10k - 20k",
    `3` = "10k - 20k",
    `4` = "20k - 30k",
    `5` = "20k - 30k",
    `6` = "30k - 40k",
    `7` = "40k - 50k",
    `8` = "50k - 60k",
    `9` = "60k - 70k",
    `10` = "70k - 80k",
    `11` = "80k - 100k",
    `12` = "100k - 120k",
    `13` = "120k - 150k",
    `14` = "150k+",
    `15` = "Prefer not to say"))

inc_new <- find_stack(ccs, family_income, "integer", make_labelled = FALSE) %>%
  mutate(faminc = recode(
    family_income,
    `1` = "Less than 10k",
    `2` = "10k - 20k",
    `3` = "20k - 30k",
    `4` = "30k - 40k",
    `5` = "40k - 50k",
    `6` = "50k - 60k",
    `7` = "60k - 70k",
    `8` = "70k - 80k",
    `9` = "80k - 100k",
    `10` = "100k - 120k",
    `11` = "120k - 150k",
    `12` = "150k+",
    `13` = "150k+",
    `14` = "150k+",
    `15` = "150k+",
    `16` = "150k+",
    `31` = "150k+",
    `32` = "150k+",
    `97` = "Prefer not to say",
    `98` = "Skipped",
    `99` = "Not Asked"))

faminc <- inner_join(inc_old, inc_new, by = c("year", "case_id")) %>% 
  mutate(faminc_char = coalesce(faminc.x, faminc.y),
         faminc_num = coalesce(family_income_old, family_income)) %>% 
  transmute(year, case_id, faminc = fct_reorder(faminc_char, faminc_num, .na_rm = FALSE))

# union ----
union <- find_stack(ccs, union, make_labelled = TRUE) %>% 
  mutate(union = labelled(zap_label(union), 
                          c("Yes, Currently" = 1,
                            "Yes, Formerly" = 2, 
                            "No, Never" = 3)),
         union = na_if(union, 8))

union_hh <- find_stack(ccs, unionhh, make_labelled = FALSE) %>% 
  mutate(union_hh = fct_collapse(unionhh,
                                 `1` = c(
                                   "Current Member in Household",
                                   "Yes, a Member of My Household Is Currently a Union Member"),
                                 `2` = c(
                                   "A Member of My Household Was Formerly a Member of a Labor Union, But Is not Now",
                                   "Former Member in Household"),
                                 `3` = c(
                                   "No Union Members in Household",
                                   "No, No One in My Household Has Ever Been a Member of a Labor Union"),
                                 `4` = c("Not Sure")
  )) %>% 
  mutate(union_hh = labelled(as.integer(union_hh), 
                             c("Yes, Currently" = 1, 
                               "Yes, Formerly" = 2,
                               "No, Never" = 3, 
                               "Not Sure" = 4))) %>% 
  select(-unionhh)


# employment, military, child, homeownership, healthins -------
employ <- find_stack(ccs, employ)
ownhome <- find_stack(ccs, ownhome)

child18 <- find_stack(ccs, child18) %>% 
  rename(has_child = child18)
milstat <- find_stack(ccs, milstat_5) %>% 
  rename(no_milstat = milstat_5) %>% 
  mutate(no_milstat = recode_factor(no_milstat,
                                    Yes = "Yes",
                                    Selected = "Yes",
                                    No = "No", 
                                    `Not Selected` = "No"))

hi_most <- find_stack(ccs, healthins_6) %>% 
  filter(year != "2018") %>% 
  rename(no_healthins = healthins_6)
hi_18 <- find_stack(ccs[c("2018", "2018comp")], healthins_7) %>% 
  rename(no_healthins = healthins_7)
healthins <- bind_rows(hi_most, hi_18) %>% 
  mutate(no_healthins = recode_factor(no_healthins,
                                      Yes = "Yes",
                                      Selected = "Yes",
                                      No = "No", 
                                      `Not Selected` = "No"))


# religion -----
relig <- find_stack(ccs, religpew, make_labelled = TRUE) %>% 
  rename(religion = religpew)
religimp <- find_stack(ccs, pew_religimp, type = "factor") %>% 
  rename(relig_imp = pew_religimp)
bornagain <- find_stack(ccs, pew_bornagain, make_labelled = TRUE) %>%
  rename(relig_bornagain = pew_bornagain)
protestant <- find_stack(ccs, religpew_protestant, make_labelled = TRUE) |> 
  rename(relig_protestant = religpew_protestant)
churatd <- find_stack(ccs, pew_churatd, make_labelled = TRUE) |> 
  rename(relig_church = pew_churatd)

# president -------
i_pres08 <- find_stack(ccs, intent_pres_08)
i_pres12 <- find_stack(ccs, intent_pres_12)
i_pres16 <- find_stack(ccs, intent_pres_16)
i_pres20 <- find_stack(ccs, intent_pres_20)

v_pres08 <- find_stack(ccs, voted_pres_08)
v_pres12 <- find_stack(ccs, voted_pres_12)
v_pres16 <- find_stack(ccs, voted_pres_16)
v_pres20 <- find_stack(ccs, voted_pres_20)

# quick consolidations for multiple years (Asked in the past)
v_pres08 <- v_pres08 %>%
  mutate(voted_pres_08 = replace(voted_pres_08, year < 2008, NA)) %>% 
  mutate(voted_pres_08 = clps_pres08(voted_pres_08),
         voted_pres_08 = replace(voted_pres_08, year %in% 2008:2011 & voted_pres_08 == "Did not Vote for this Office", NA))

v_pres12 <- v_pres12 %>% 
  mutate(voted_pres_12 = clps_pres12(voted_pres_12))
v_pres16 <- v_pres16 %>%
  mutate(voted_pres_16 = clps_pres16(voted_pres_16),
         voted_pres_16 = replace(voted_pres_16, year %in% 2019:2021 & voted_pres_16 == "Did not Vote for this Office", NA))
v_pres20 <- v_pres20 %>%
  mutate(voted_pres_20 = clps_pres20(voted_pres_20),
         voted_pres_20 = replace(voted_pres_20, voted_pres_20 == "Did not Vote for this Office", NA))

# coalesce
pres_party <- i_pres08 %>% 
  left_join(i_pres12, by = c("year", "case_id")) %>% 
  left_join(i_pres16, by = c("year", "case_id")) %>% 
  left_join(i_pres20, by = c("year", "case_id")) %>% 
  left_join(v_pres08, by = c("year", "case_id")) %>% 
  left_join(v_pres12, by = c("year", "case_id")) %>% 
  left_join(v_pres16, by = c("year", "case_id")) %>% 
  left_join(v_pres20, by = c("year", "case_id")) %>% 
  mutate_if(is.factor, as.character) %>% 
  # NA for previous election
  mutate(voted_pres_08 = replace(voted_pres_08, year == 2012, NA),
         voted_pres_12 = replace(voted_pres_12, year == 2016, NA),
         voted_pres_16 = replace(voted_pres_16, year == 2020, NA)) %>%  
  transmute(
    year, case_id,
    intent_pres_party = pres_names(
      coalesce(intent_pres_20, intent_pres_16, intent_pres_12, intent_pres_08)),
    voted_pres_party  = pres_names(
      coalesce(voted_pres_20, voted_pres_16, voted_pres_12, voted_pres_08))
  )

# House, Sen, Gov -----
i_rep <- find_stack(ccs, intent_rep, new_reorder = FALSE)
i_sen <- find_stack(ccs, intent_sen, new_reorder = FALSE)
i_gov <- find_stack(ccs, intent_gov, new_reorder = FALSE)
v_rep <- find_stack(ccs, voted_rep, new_reorder = FALSE)
v_sen <- find_stack(ccs, voted_sen, new_reorder = FALSE)
v_gov <- find_stack(ccs, voted_gov, new_reorder = FALSE)


# approval -----
apvpres <- find_stack(ccs, approval_pres, make_labelled = TRUE)
apvrep  <- find_stack(ccs, approval_rep, make_labelled = FALSE)
apvsen1 <- find_stack(ccs, approval_sen1, make_labelled = FALSE)
apvsen2 <- find_stack(ccs, approval_sen2, make_labelled = FALSE)
apvgov  <- find_stack(ccs, approval_gov, make_labelled = TRUE) 

# economy -----
econ_char <- find_stack(ccs, economy_retro, make_labelled = FALSE, new_reorder = FALSE) %>% 
  mutate(economy_retro_char = recode(economy_retro_char,
                                     `Gotten Worse`           = "Gotten Worse / Somewhat Worse", 
                                     `Gotten Somewhat Worse`  = "Gotten Worse / Somewhat Worse", 
                                     `Gotten Better`          = "Gotten Better / Somewhat Better",
                                     `Gotten Somewhat Better` = "Gotten Better / Somewhat Better")) %>% 
  filter(year != 2009) %>% 
  bind_rows(cc09_econ) %>% 
  mutate(economy_retro_char = replace(economy_retro_char, economy_retro_num == 8, NA),
         economy_retro_num  = na_if(economy_retro_num, 8),
         economy_retro_char = str_to_lower(economy_retro_char),
         economy_retro_char = str_replace(economy_retro_char, "^g", "G"),
         economy_retro_char = str_replace(economy_retro_char, "^s", "S"),
         economy_retro_char = str_replace(economy_retro_char, "^n", "N"),
  )

# correct to labelled
econ_key <- deframe(distinct(select(econ_char, economy_retro_char, economy_retro_num)))
econ <-  econ_char %>% 
  mutate(economy_retro = labelled(economy_retro_num, labels = econ_key)) %>% 
  select(year, case_id, economy_retro)


# news interest ------
newsint <- find_stack(ccs, newsint, make_labelled = TRUE) %>% 
  remove_value_labels(newsint = 8) %>% 
  mutate(newsint = na_if(newsint, 8))

# marriage status -----
marstat <- find_stack(ccs, marstat, make_labelled = TRUE) %>% 
  remove_value_labels(marstat = 8) %>% 
  mutate(marstat = na_if(marstat, 8)) %>% 
  labelled::add_value_labels(marstat = c(`Single / Never Married` = 5))

# citizenship ----
# define by immstat
citizen <- find_stack(ccs, immstat) %>% 
  mutate(citizen = str_detect(immstat, regex("(Non-Citizen|Not A Citizen)", ignore_case = TRUE))) %>% 
  mutate(citizen = labelled(citizen + 1, labels = c(`Citizen` = 1, `Non-Citizen` = 2))) %>% 
  select(-immstat)


# self reported turnout ----
intent_trn <- find_stack(ccs, intent_trn, type = "factor") %>% 
  mutate(intent_turnout_self = recode(
    intent_trn, 
    `Yes, Definitely` = "Yes, definitely",
    `I Already Voted (Early or Absentee)` = "I already voted (early or absentee)",
    `I Plan to Vote Before November 3rd` = "Plan to vote early",
    `I Plan to Vote Before November 4th` = "Plan to vote early",
    `I Plan to Vote Before November 6th` = "Plan to vote early"))

voted_trn <- find_stack(ccs, voted_trn, type = "factor")  %>% 
  mutate(voted_turnout_self = case_when(
    str_detect(voted_trn, regex("Definitely Voted", ignore_case = TRUE)) ~ "Yes",
    str_detect(voted_trn, regex("yes", ignore_case = TRUE)) ~ "Yes",
    str_detect(voted_trn, regex("no", ignore_case = TRUE)) ~ "No",
    str_detect(voted_trn, regex("not sure", ignore_case = TRUE)) ~ "No",
    str_detect(voted_trn, regex("Did Not Vote", ignore_case = TRUE)) ~ "No",
    str_detect(voted_trn, regex("didn't Vote", ignore_case = TRUE)) ~ "No",
    str_detect(voted_trn, regex("But Didn't", ignore_case = TRUE)) ~ "No",
    str_detect(voted_trn, regex("But couldn't", ignore_case = TRUE)) ~ "No",
    str_detect(voted_trn, regex("But Did Not or Could Not", ignore_case = TRUE)) ~ "No",
    TRUE ~ NA_character_)
  ) %>% 
  mutate(voted_turnout_self = fct_relevel(voted_turnout_self, "Yes", "No"))

# checks before deleting
count(intent_trn, intent_turnout_self, intent_trn)
count(voted_trn, voted_turnout_self, voted_trn)
voted_trn$voted_trn <- NULL
intent_trn$intent_trn <- NULL


# validated vote turnout -----
vv_regstatus   <- find_stack(ccs, vv_regstatus, new_reorder = FALSE) # will reorder by frequency later
vv_party_gen   <- find_stack(ccs, vv_party_gen, new_reorder = FALSE)
vv_party_prm   <- find_stack(ccs, vv_party_prm, new_reorder = FALSE)
vv_turnout_gvm <- find_stack(ccs, vv_turnout_gvm, new_reorder = FALSE)
vv_turnout_pvm <- find_stack(ccs, vv_turnout_pvm, new_reorder = FALSE)

# year -----
cong       <- find_stack(ccs, cong, "integer")
cong_up    <- find_stack(ccs, cong_up, "integer")


# geography ----
state      <- find_stack(ccs, state, "character")
state_post      <- find_stack(ccs, state_post, "character")
st      <- find_stack(ccs, st, "character")
st_post      <- find_stack(ccs, st_post, "character")

zipcode    <- find_stack(ccs, zipcode, "character") %>% 
  mutate(zipcode = str_pad(zipcode, width = 5, pad = "0"))

county_fips <- find_stack(ccs, county_fips, "numeric") %>% 
  left_join(cc17_county, by = c("year", "case_id")) %>% 
  mutate(county_fips = coalesce(county_fips, as.numeric(countyfips))) %>% 
  select(-countyfips) %>% 
  filter(year != 2007) %>% 
  bind_rows(select(cc07, year, case_id, county_fips = CC06_V1004) %>% 
              mutate_all(zap_labels))

dist       <- find_stack(ccs, dist, "integer")
dist_up    <- find_stack(ccs, dist_up, "integer")
cd         <- find_stack(ccs, cd, "character")
cd_up      <- find_stack(ccs, cd_up, "character")

dist_post     <- find_stack(ccs, dist_post, "integer")
dist_up_post  <- find_stack(ccs, dist_up_post, "integer")
cd_post       <- find_stack(ccs, cd_post, "character")
cd_up_post    <- find_stack(ccs, cd_up_post, "character")


# format state and CD, then zipcode and county ----
stcd <- left_join(state, st) %>%
  left_join(cong) %>%
  left_join(cong_up) %>%
  left_join(state_post) %>%
  left_join(st_post) %>%
  left_join(dist) %>%
  left_join(dist_up) %>%
  left_join(cd) %>%
  left_join(cd_up) %>%
  left_join(dist_post) %>%
  left_join(dist_up_post) %>%
  left_join(cd_post) %>%
  left_join(cd_up_post)

geo <- stcd %>%
  left_join(zipcode) %>%
  left_join(county_fips)


# bind together ----
ccc <- geo %>%
  left_join(tookpost) %>%
  left_join(wgt) %>%
  left_join(wgt_post) %>%
  left_join(vwgt) %>%
  left_join(vwgt_post) %>%
  left_join(time) %>%
  left_join(pid3) %>%
  left_join(pid3_leaner) %>%
  left_join(pid7) %>%
  left_join(ideo5) %>%
  left_join(gend) %>%
  left_join(bryr) %>%
  left_join(age) %>%
  left_join(race) %>%
  left_join(hisp) %>%
  left_join(citizen) %>%
  left_join(educ) %>%
  left_join(marstat) %>%
  left_join(faminc) %>%
  left_join(union) %>%
  left_join(union_hh) %>%
  left_join(employ) %>%
  left_join(healthins) %>%
  left_join(child18) %>%
  left_join(ownhome) %>%
  left_join(milstat) %>%
  left_join(relig) %>%
  left_join(religimp) %>%
  left_join(bornagain) %>%
  left_join(protestant) %>%
  left_join(churatd) %>%
  left_join(econ) %>%
  left_join(newsint) %>%
  left_join(apvpres) %>%
  left_join(apvrep) %>%
  left_join(apvsen1) %>%
  left_join(apvsen2) %>%
  left_join(apvgov) %>%
  left_join(i_pres08) %>%
  left_join(i_pres12) %>%
  left_join(i_pres16) %>%
  left_join(i_pres20) %>%
  left_join(v_pres08) %>%
  left_join(v_pres12) %>%
  left_join(v_pres16) %>%
  left_join(v_pres20) %>%
  left_join(pres_party) %>%
  left_join(intent_trn) %>%
  left_join(voted_trn) %>%
  left_join(vv_regstatus) %>%
  left_join(vv_party_gen) %>%
  left_join(vv_party_prm) %>%
  left_join(vv_turnout_gvm) %>%
  left_join(vv_turnout_pvm)


stopifnot(nrow(ccc) == nrow(pid3))


# check no accidental duplicate id's within 2012 or 2009
foo_09 <- wgt %>% filter(year == 2009)
stopifnot(nrow(foo_09) == nrow(distinct(foo_09, year, case_id)))

foo_12 <- wgt %>% filter(year == 2012)
stopifnot(nrow(foo_12) == nrow(distinct(foo_12, year, case_id)))


# don't use panel rows for now -----
panel_id <- ccs[["2012panel"]] %>% select(year, case_id) %>% mutate(case_id = as.integer(case_id))
# mit06_id <- ccs[["2006mit"]] %>% select(year, case_id) %>% mutate(case_id = as.integer(case_id))
# hu08_id <- ccs[["2008hu"]] %>% select(year, case_id) %>% mutate(case_id = as.integer(case_id))
hu09_id <- ccs[["2009hu"]] %>% select(year, case_id) %>% mutate(case_id = as.integer(case_id))
comp_id <- ccs[["2018comp"]] %>% select(year, case_id) %>% mutate(case_id = as.integer(case_id))
addon_id <- bind_rows(hu09_id, panel_id, comp_id) # hu08_id, 


# Common manipulations ----
# Weight --
size_year <- ccc %>%
  anti_join(addon_id, by = c("year", "case_id")) %>% # don't count panel to get weights
  group_by(year) %>%
  summarize(size = n()) %>%
  mutate(size_factor = size / median(size)) # manageable constant -- divide by median

ccc_sort <- ccc %>%
  left_join(select(size_year, year, size_factor)) %>%
  mutate(weight_cumulative = weight / size_factor) %>%
  select(-size_factor) %>%
  select(year, case_id, weight, weight_cumulative, everything())


# Write ----- 
write_rds(ccs, "data/temp_cc-name-cleaned-list.rds")

save(i_rep, i_sen, i_gov, v_rep, v_sen, v_gov, file = "data/output/01_responses/vote_responses.RData")
save(vv_party_gen, vv_party_prm, vv_regstatus, vv_turnout_gvm, vv_turnout_pvm, file = "data/output/01_responses/vv_responses.RData")
saveRDS(ccc_sort, "data/output/01_responses/cumulative_stacked.Rds")
saveRDS(addon_id, "data/output/01_responses/addon_ids.Rds")
write_csv(size_year, "data/output/03_contextual/weight_rescale_by-year.csv")

cat("Finished stacking vars for cumulative \n")
