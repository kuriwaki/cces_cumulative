# functions for 05_stack-cumulative.R

#' name standardization
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
        voted_sen = replace(voted_sen, year %% 2 == 1, NA)) %>% #  % NA for odd years
      # fix county misalignment
      mutate(county_fips = (county_fips < 1000) * as.numeric(state_pre) * 1000 + county_fips) %>% 
      mutate(county_fips = as.character(county_fips))
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
  
  # 2008 -------
  if (identical(cces_year, 2008L)) {
    tbl <- tbl %>%
      rename(
        approval_pres = CC335bush,
        approval_rep = CC335rep,
        approval_sen1 = CC335sen1,
        approval_sen2 = CC335sen2,
        approval_gov = CC335gov,
        economy_retro = CC302,
        family_income_old = V246,
        marstat = V214,
        starttime = V300,
        intent_trn = CC326,
        voted_trn = CC403,
        intent_rep = CC339,
        intent_sen  = CC335,
        intent_gov  = CC336,
        intent_pres_08 = CC327,
        voted_pres_08 = CC410,
        pid3 = CC307,
        pid7 = CC307a,
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
  
  # 2010 - 2011 ----
  # only those necessary for custom fix
  if (identical(cces_year, 2010L)) {
    tbl <- tbl |> 
      rename(
        gender = V208,
        marstat = V214,
        voted_pres_08 = CC317
      )
  }
  
  if (identical(cces_year, 2011L)) {
    tbl <- tbl |> 
      rename(
        gender = V208,
        marstat = V214,
        voted_pres_08 = CC331
      )
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
  
  # 2022 - 2023 ----
  if (identical(cces_year, 2022L)) {
    
    tbl <- tbl %>%
      # called "Two or more races" in 2020
      mutate(race = sjlabelled::replace_labels(
        race, labels = c("Mixed" = 6))) %>%
      # rename
      rename(
        weight = commonweight,
        weight_post = commonpostweight,
        # rvweight = vvweight,
        # rvweight_post = vvweight_post,
        # gender = gender4,
        approval_pres = CC22_320a,
        approval_rep = CC22_320f,
        approval_sen1 = CC22_320g,
        approval_sen2 = CC22_320h,
        approval_gov = CC22_320d,
        economy_retro = CC22_302,
        faminc = faminc_new,
        intent_trn = CC22_363,
        intent_sen = CC22_365,
        intent_senx = CC22_365_voted,
        intent_gov = CC22_366,
        intent_govx = CC22_366_voted,
        intent_rep = CC22_367,
        intent_repx = CC22_367_voted,
        voted_trn = CC22_401,
        voted_pres_16 = presvote16post,
        voted_pres_20 = presvote20post,
        voted_sen = CC22_411,
        voted_rep = CC22_412,
        voted_gov = CC22_413,
        vv_turnout_gvm = TS_g2022,
        vv_turnout_pvm = TS_p2022,
        vv_regstatus = TS_voterstatus,
        vv_party_gen = TS_partyreg,
        vv_party_prm = TS_p2022_party,
        vv_st = TS_state
      ) %>%
      mutate_at(vars(matches("^vv")), ~replace_na(as.character(as_factor(.x)), "")) %>% 
      labelled::add_value_labels(marstat = c("Domestic Partnership" = 6, "Single" = 5))
  }
  
  if (identical(cces_year, 2023L)) {
    tbl <- tbl %>%
      # called "Two or more races" in 2020-2021
      mutate(race = sjlabelled::replace_labels(
        race, labels = c("Mixed" = 6))) %>%
      rename(
        weight = commonweight,
        approval_pres = CC23_312a,
        approval_rep = CC23_312f,
        approval_sen1 = CC23_312g,
        approval_sen2 = CC23_312h,
        approval_gov = CC23_312d,
        economy_retro = CC23_301,
        faminc = faminc_new,
        voted_pres_20 = presvote20post
      ) %>%
      labelled::add_value_labels(marstat = c("Domestic Partnership" = 6, "Single" = 5))
  }
  
  # 2024 ------
  if (identical(cces_year, 2024L)) {
    tbl <- tbl %>%
      mutate(race = sjlabelled::replace_labels(
        race, labels = c("Mixed" = 6))) %>%
      mutate(across(matches("(start|end)time"), ~ as.POSIXct(.x/1000, origin = "1960-01-01"))) |> 
      rename(
        weight = commonweight,
        approval_pres = CC24_312a,
        approval_rep  = CC24_312f,
        approval_sen1 = CC24_312g,
        approval_sen2 = CC24_312h,
        approval_gov  = CC24_312d,
        economy_retro = CC24_301,
        faminc = faminc_new,
        intent_trn = CC24_363,
        intent_pres_24 = CC24_364b,
        intent_pres_24x = CC24_364a, # double check if this is actually voted
        intent_rep = CC24_367,
        intent_repx = CC24_367_voted,
        intent_sen = CC24_365,
        intent_senx = CC24_365_voted,
        intent_gov = CC24_366,
        intent_govx = CC24_366_voted,
        voted_trn = CC24_401,
        voted_rep = CC24_412,
        voted_sen = CC24_411,
        voted_gov = CC24_413,
        voted_pres_16 = presvote16post,
        voted_pres_20 = presvote20post,
        voted_pres_24 = CC24_410
      ) %>%
      labelled::add_value_labels(marstat = c("Domestic Partnership" = 6, "Single" = 5))
  }
  
  
  # more standardization for post 2012 ------
  if (cces_year[1] %in% c(2012:2024) | cces_year[1] == "2012_panel") {
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
  
  if (cces_year[1] != 2022) {
    tbl <- tbl %>% 
      labelled::add_value_labels(marstat = c("Domestic Partnership" = 6, "Single" = 5))
  }
  
  # gender ----
  if (cces_year[1] %in% c(2021:2024)) {
    tbl <- tbl |> 
      mutate(
        gender = labelled(zap_labels(gender4), c("Male" = 1, "Female" = 2)),
        gender = replace(gender, gender %in% 3:4, NA_real_)
      )
  } else {
    tbl <- tbl |> 
      mutate(sex = gender)
  }
  
  # hispanic origin ----
  ## 20-22: "_hisp_", CC20_asian_1
  ## 16: "Hispanic_origin", Asian_Origin
  ## 15, 17-19: variants of CC15_354ax
  if (cces_year[1] >= 2015) {
    # regex to find the variables
    hisp_regex <- "(_hisp|Hispanic_origin|CC15_354ax|CC17_353a|CC18_354a|CC19_353a)_[0-9]"
    qlb_regex <- "(Country ancestry|Latin Heritage|Hispanic Origin|Hispanic_origin) - "
    hisp_key <-
      tbl |> 
      select(matches(hisp_regex)) |> 
      map_chr(\(x) attr(x, "label")) |> enframe() |> 
      transmute(
        name, 
        lab = str_squish(str_remove_all(value, regex(qlb_regex, ignore_case = TRUE))))
    
    # do not concatenate these
    rm_values <- c("No country in particular", 
                   "No Country in Particular",
                   "Not Latino, Hispanic", 
                   "I am not of Latino, Hispanic or Spanish heritage",
                   "I am not of Latino, Hispanic or Spanish Heritage",
                   "Other") 
    
    # reshape, recode, and concatenate
    hisp_origin <- tbl |> 
      select(case_id, matches(hisp_regex)) |> 
      pivot_longer(-case_id) |> 
      left_join(hisp_key) |> 
      mutate(lab = replace(lab, lab %in% rm_values, NA_character_)) |> 
      filter(!is.na(value) & value != 9 & value == 1 & !is.na(lab)) |> 
      arrange(name) |> 
      summarize(hisp_origin = str_c(lab, collapse = "!!"), .by = case_id)
    
    tbl <- tbl |> 
      left_join(hisp_origin, by = "case_id")
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
    drop_na() |> 
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
                      `polling place` = vtd,
                      `Absentee` = vtd,
                      `absentee` = vtd,
                      `Early` = vtd,
                      `early` = vtd,
                      `earlyVote` = vtd,
                      `mail` = vtd,
                      `Mail` = vtd,
                      `provisional` = vtd,
                      `voted by unknown method` = vtd,
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
      `Undervote` = c("Did not Vote"),
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
    `Other` = c(
      "Someone Else", 
      "Vote for Someone Else", 
      "Other"),
    `Undervote` = c(
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
    `Gary Johnson` = c("Gary Johnson (Libertarian)", "Gary Johnson"),
    `Evan McMullin` = c("Evan Mcmullin (Independent)", "Evan Mcmullin"),
    `Jill Stein` = c("Jill Stein (Green)", "Jill Stein"),
    `Other` = c(
      "Other", "Someone Else"),
    `Undervote` = c(
      "I Didn't Vote in this Election",
      "Did not Vote for President",
      "I Did not Cast a Vote for President"),
    `Not Sure / Don't Recall` = c(
      "I'm not Sure", "I Don't Recall")
  ) %>% 
    fct_relevel("Hilary Clinton", "Donald Trump") %>% 
    fct_lump(n = 6)
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
    `Jo Jorgensen` = "Jo Jorgensen",
    `Howie Hawkins` = "Howie Hawkins",
    `Other` = c(
      "Other", 
      "Someone Else"),
    `Undervote` = c(
      "I Did not Vote in this Race",
      "I Did not Vote",
      "Did Not Vote for President"),
    `Not Sure / Don't Recall` = c("I'm not Sure")
  ) %>% 
    fct_relevel("Joe Biden", "Donald Trump") %>% 
    fct_lump(n = 6)
}

clps_pres24 <- function(vec) {
  fct_collapse(
    vec, 
    `Kamala Harris` = c(
      "Kamala Harris", 
      "Kamala Harris (Democrat)"),
    `Donald Trump` = c(
      "Donald Trump", 
      "Donald Trump (Republican)"),
    `Other / Someone Else` = c(
      "Other", 
      "Someone Else"),
    `Jill Stein` = "Jill Stein", 
    `Cornel West` = "Cornel West", 
    `Chase Oliver` = "Chase Oliver",
    `Undervote` = c(
      "I Did not Vote in this Race",
      "I Did not Vote",
      "Did not Vote for President"),
    `Not Sure / Don't Recall` = c("I'm not Sure")
  ) %>% 
    fct_relevel("Kamala Harris", "Donald Trump") %>% 
    fct_lump(n = 6)
}

#' give pres party from chars of pres names
pres_names <- function(vec) {
  case_when(
    str_detect(vec, regex("(Obama|Clinton|Biden|Harris)", ignore_case = TRUE)) ~ "Democratic",
    str_detect(vec, regex("(Mccain|Romney|Trump)", ignore_case = TRUE)) ~ "Republican",
    str_detect(vec, regex("(Mckinney|Paul|Barr|Stein|Jorgensen|Johnson|West|Oliver|Kennedy)", ignore_case = TRUE)) ~ "Third Party",
    str_detect(vec, regex("(McMullin|Nader)", ignore_case = TRUE)) ~ "Independent",
    str_detect(vec, regex("Other", ignore_case = TRUE)) ~ "Other Candidate",
    str_detect(vec, regex("Did Not", ignore_case = TRUE)) ~ "Did not Vote",
    TRUE ~ NA_character_) %>% 
    factor(levels = c("Democratic", "Republican", "Third Party", "Independent", "Other Candidate", "Did not Vote"))
}

