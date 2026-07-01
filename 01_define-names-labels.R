library(tidyverse)
library(tibble)



# each row is a variable for the standardized data, each column is for the cces year0
master_11 <- tribble(
    ~name,      ~`2006`, ~`2006m`,      ~`2007`,     ~`2008`, ~`2009`, ~`2010`, ~`2011`,
    "rep_inc",  "v5013", "rep",         "repname",    "V527",  "v627",  "V501",  "V501",
    "rep_ipt",  "v5014", "reppid",      NA,           "V535",  NA,      "V502",  NA,     
    "sen1_inc", "v5015", "sen1",        "sen1name",   "V551",  "v651",  "V513",  "V513",
    "sen1_ipt", "v5016", "sen1pid",     NA,           "V544",  NA,      "V514",  NA,     
    "sen2_inc", "v5017", "sen2",        "sen2name",   "V552",  "v652",  "V521",  "V521",
    "sen2_ipt", "v5018", "sen2pid",     NA,           "V548",  NA,      "V522",  NA,     
    "gov_inc",  "v5019", "gov",         "govname",    "V508",  "v608",  "V529",  "V529",
    "gov_ipt",  "v5020", "govpid",      NA,           "V513",  NA,      "V530",  NA,     
    "rep_can1", "v5001", "repcand1",    NA,           "V518",  NA,      "V533",  NA,     
    "rep_pty1", "v5002", "repcand1pid", NA,            NA,     NA,      NA,      NA,     
    "rep_can2", "v5003", "repcand2",    NA,           "V519",  NA,      "V536",  NA,     
    "rep_pty2", "v5004", "repcand2pid", NA,            NA,     NA,      NA,      NA,     
    "rep_can3",  NA,     NA,            NA,           "V520",  NA,      "V539",  NA,     
    "rep_pty3",  NA,     NA,            NA,           "V533",  NA,      "V542",  NA,     
    "sen_can1", "v5005", "sencand1",    NA,           "V553",  NA,      "V548",  NA,     
    "sen_pty1", "v5006", "sencand1pid", NA,            NA,     NA,      NA,      NA,     
    "sen_can2", "v5007", "sencand2",    NA,           "V555",  NA,      "V551",  NA,     
    "sen_pty2", "v5008", "sencand2pid", NA,            NA,     NA,      NA,      NA,     
    "sen_can3", NA,      NA,            NA,           "V556",  NA,      "V554",  NA,     
    "sen_pty3", NA,      NA,            NA,           "V572",  NA,      "V556",  NA,     
    "gov_can1", "v5009", "govcand1",    NA,           "V501",  NA,      "V564",  NA,     
    "gov_pty1", "v5010", "govcand1pid", NA,            NA,     NA,      NA,      NA,     
    "gov_can2", "v5011", "govcand2",    NA,           "V502",  NA,      "V567",  NA,     
    "gov_pty2", "v5012", "govcand2pid", NA,            NA,     NA,      NA,      NA,     
    "gov_can3", NA,      NA,            NA,           "V503",  NA,      "V570",  NA,     
    "gov_pty3", NA,      NA,            NA,           "V512",  NA,      "V572",  NA,     
  )

master_17 <- tribble(
  ~name,      ~`2012`,            ~`2013`,            ~`2014`,            ~`2015`,            ~`2016`,             ~`2017`,
  "rep_inc",  "CurrentHouseName", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName",  "CurrentHouseName", 
  "rep_ipt",  "CurrentHouseParty","CurrentHouseParty","CurrentHouseParty","CurrentHouseParty","CurrentHouseParty", "CurrentHouseParty",
  "sen1_inc", "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",   "CurrentSen1Name",
  "sen1_ipt", "CurrentSen1Party", "CurrentSen1Party", "CurrentSen1Party", "CurrentSen1Party", "CurrentSen1Party",  "CurrentSen1Party",
  "sen2_inc", "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",   "CurrentSen2Name",
  "sen2_ipt", "CurrentSen2Party", "CurrentSen2Party", "CurrentSen2Party", "CurrentSen2Party", "CurrentSen2Party",  "CurrentSen2Party",
  "gov_inc",  "CurrentGovName",   "CurrentGovName",   "CurrentGovName",   "CurrentGovName",   "CurrentGovName",    "CurrentGovName",
  "gov_ipt",  "CurrentGovParty",  "CurrentGovParty",  "CurrentGovParty",  "CurrentGovParty",  "CurrentGovParty",   "CurrentGovParty",
  "rep_can1", "HouseCand1Name",   NA,                 "HouseCand1Name",   NA,                 "HouseCand1Name",    NA,
  "rep_pty1", "HouseCand1Party",  NA,                 "HouseCand1Party",  NA,                 "HouseCand1Party",   NA,
  "rep_can2", "HouseCand2Name",   NA,                 "HouseCand2Name",   NA,                 "HouseCand2Name",    NA,
  "rep_pty2", "HouseCand2Party",  NA,                 "HouseCand2Party",  NA,                 "HouseCand2Party",   NA,
  "rep_can3", "HouseCand3Name",   NA,                 "HouseCand3Name",   NA,                 "HouseCand3Name",    NA,
  "rep_pty3", "HouseCand3Party",  NA,                 "HouseCand3Party",  NA,                 "HouseCand3Party",   NA,
  "sen_can1", "SenCand1Name",     NA,                 "SenCand1Name",     NA,                 "SenCand1Name",      NA,
  "sen_pty1", "SenCand1Party",    NA,                 "SenCand1Party",    NA,                 "SenCand1Party",     NA,
  "sen_can2", "SenCand2Name",     NA,                 "SenCand2Name",     NA,                 "SenCand2Name",      NA,
  "sen_pty2", "SenCand2Party",    NA,                 "SenCand2Party",    NA,                 "SenCand2Party",     NA,
  "sen_can3", "SenCand3Name",     NA,                 "SenCand3Name",     NA,                 "SenCand3Name",      NA,
  "sen_pty3", "SenCand3Party",    NA,                 "SenCand3Party",    NA,                 "SenCand3Party",     NA,
  "gov_can1", "GovCand1Name",     NA,                 "GovCand1Name",     NA,                 "GovCand1Name",      NA,
  "gov_pty1", "GovCand1Party",    NA,                 "GovCand1Party",    NA,                 "GovCand1Party",     NA,
  "gov_can2", "GovCand2Name",     NA,                 "GovCand2Name",     NA,                 "GovCand2Name",      NA,
  "gov_pty2", "GovCand2Party",    NA,                 "GovCand2Party",    NA,                 "GovCand2Party",     NA,
  "gov_can3", NA,                 NA,                 NA,                 NA,                 "GovCand3Name",      NA,
  "gov_pty3", NA,                 NA,                 NA,                 NA,                 "GovCand3Party",    NA
)

master <- left_join(master_11, master_17, by = "name")

# same as previous years
master$`2018` <- master$`2016`
master$`2025` <- master$`2023` <- master$`2021` <- master$`2019` <- master$`2017`
master$`2024` <- master$`2022` <- master$`2020` <- master$`2018`
# don't exist in 2020
master$`2020`[master$`2020` %in% c("SenCand3Name", "SenCand3Party", "GovCand3Name", "GovCand3Party"
)] <- NA

# Odd years 2019+ have GovCand columns matching even-year structure
gov_cand_rows <- master$name %in% c("gov_can1", "gov_pty1", "gov_can2", "gov_pty2", "gov_can3", "gov_pty3")
for (yr in c("2019", "2021", "2023", "2025")) {
  master[gov_cand_rows, yr] <- master[gov_cand_rows, "2018"]
}
# 2021 and 2025 governor races have only two candidates
master[master$name %in% c("gov_can3", "gov_pty3"), c("2021", "2025")] <- NA

check_no_dupes <- function(c) if (n_distinct(master[[c]], na.rm = TRUE) != sum(!is.na(master[[c]]))) stop(glue("check column {c}"))
for (c in 2:ncol(master)) check_no_dupes(c)


ccc_meta <- tribble(
  ~alias, ~type, ~name, ~description,
  "year",                "categorical", "CCES year",                            "[Year of CCES Common Content]",
  "cong",                "categorical", "Congressional session",                               "[Current Congressional session. Use to join incumbents]",
  "cong_up",             "categorical", "Congressional session for upcoming Congress",         "[Upcoming Congressional session. Use to join candidates.]",
  "starttime",           "datetime",    "Start time",                           "[Pre-election wave start time (up to second)]",
  "case_id",             "text",        "Case identifier",                      "[Case (Respondent) Identifier. Unique within year]",
  "tookpost",            "categorical", "Took post-election wave",              "[Whether or not the respondent took the post-election wave of the survey (in even years)]",
  "weight",              "numeric",     "Survey weight (Year-Specific)",        "[weights for pre-election survey of each year]",
  "weight_cumulative",   "numeric",     "Survey weight (Cumulative)",           "[weight variable with simple adjustment: multiplied a constant within year to make years comparable]",
  "weight_post",         "numeric",     "Survey weight for post-election wave", "[weight for post-election wave respondents. Only available for some  of the even years.]",
  "vvweight",            "numeric",     "Survey weights to validated registered voters",        "[weights to validated registered voter population]",
  "vvweight_post",       "numeric",     "Survey weights to validated registered voters, post-election wave", "[weights to validated registered voter population, post-election wave]",
  "state",               "categorical", "State (FIPS)",                         "[State]",
  "st",                  "categorical", "State abbreviation (FIPS)",     "[State Abbreviation]",
  "dist",                "categorical", "Congressional district number in current Congress",   "[Current Congressional District Number]",
  "dist_up",             "categorical", "Congressional district number for upcoming Congress", "[Upcoming Congressional District Number]",
  "cd",                  "categorical", "Congressional district in current Congress",          "[Current Congressional District]",
  "cd_up",               "categorical", "Congressional district in upcoming Congress",         "[Upcoming Congressional District]",
  "state_post",          "categorical", "State (FIPS), post-election",                         "[State, post-election]",
  "st_post",             "categorical", "State abbreviation (FIPS), post-election",     "[State, post-election]",
  "dist_post",           "categorical", "Congressional district number in current Congress, post-election",   "[Current Congressional District Number, post-election]",
  "dist_up_post",        "categorical", "Congressional district number for upcoming Congress, post-election", "[Upcoming Congressional District Number, post-election]",
  "cd_post",             "categorical", "Congressional district in current Congress, post-election",          "[Current Congressional District, post-election]",
  "cd_up_post",          "categorical", "Congressional district in upcoming Congress, post-election",         "[Upcoming Congressional District, post-election]",
  "zipcode",             "text",        "Zipcode (lookupzip)", "[lookupzip in most years.] So that we can ask you about the news and events in your area, in what zip code do you currently reside?",
  "county_fips",         "text",        "County of residence",           "[County (Imputed from input zipcode)]",
  "gender",              "categorical", "Sex (standardized)",            "Are you...? <1> Male <2> Female [2018-2020] Are you male or female? [2006-2016]",
  "gender4",             "categorical", "Gender",                        "What is your gender?",
  "sex",                 "categorical", "Sex",                           "Are you male or female? [2006-2016]",
  "birthyr",             "numeric",     "Year of birth",                 "In what year were you born?",
  "age",                 "numeric",     "Age",                           "[Approximate age computed from the year of survey minus Year of Birth]",
  "educ",                "categorical", "Education",                     "What is the highest level of education you have completed?",
  "race",                "categorical", "Race",                          "What racial or ethnic group best describes you?",
  "hispanic",            "categorical", "Hispanic",                      "Are you of Spanish, Latino, or Hispanic origin or descent? [Asked if response to race is not Hispanic]",
  "race_h",              "categorical", "Race (any-part Hispanic)",      "[race (What racial or ethnic group best describes you?) combined with hispanic ethnicity]",
  "hisp_origin",         "categorical", "Hispanic origin",               "From which country or region do you trace your heritage or ancestry? (Check all that apply) [asked if any-part Hispanic]",
  "faminc",              "categorical", "Family Income",                 "Thinking back over the last year, what was your family's annual income? [Brackets coarsened]",
  "union",               "categorical", "Union membership",              "Are you a member of a union?",
  "union_hh",            "categorical", "Union membership in household", "Other than yourself, is any member of your household a union member?",
  "religion",            "categorical", "Religion",                      "What is your present religion, if any?",
  "relig_bornagain",     "categorical", "Evangelical Christian",         "Would you describe yourself as a born-again or evangelical Christian, or not?",
  "relig_protestant",    "categorical", "Branch of Protestantism",       "To which Protestant church or group do you belong?",
  "relig_imp",           "categorical", "Importance of religion",        "How important is religion in your life?",
  "relig_church",        "categorical", "Church Attendance",             "Aside from weddings and funerals, how often do you attend religious services?",
  "economy_retro",       "categorical", "Retrospective economy",         "OVER THE PAST YEAR the nation's economy has ...?",
  "newsint",             "categorical", "News Interest",                 "Some people seem to follow what's going on in government and public affairs most of the time, whether there's an election going on or not. Others aren't that interested. Would you say you follow what's going on in government and public affairs ..", 
  "marstat",             "categorical", "Marital Status",                "What is your marital status?", 
  "citizen",             "categorical", "Citizenship",                   "[Based on self-report for immigration status]", 
  "no_healthins",        "categorical", "Uninsured",                     "[Based on health insurance question; respondent has none of the insurance options given]", 
  "no_milstat",          "categorical", "Military Status (None)",        "[Based on military household question; neither respondent nor immediate family has served]", 
  "employ",              "categorical", "Employment Status",             "Which of the following best describes your current employment status?", 
  "sexuality",           "categorical", "Sexual Orientation",            "Which of the following describes your sexuality?", 
  "investor",            "categorical", "Investor",                      "Do you personally (or jointly with a spouse), have any money invested in the stock market right now, either in an individual stock or in a mutual fund?", 
  "ownhome",             "categorical", "Home Ownership",                "Do you own your home or pay rent?", 
  "has_child",           "categorical", "Parent of Young Children",      "Are you the parent or guardian of any children under the age of 18?", 
  "pid3",                "categorical", "Partisan identity (3 point)",   "Generally speaking, do you think of yourself as a ...?",
  "pid3_leaner",         "categorical", "Partisan identity (including leaners)", "[Codes self-identified Independents in pid3 who expressed leaning towards a party in pid7 (Lean Democrats / Republicans) as partisans.]",
  "pid7",                "categorical", "Partisan identity (7 point)",   "[Based on branching from Partisan Identity question]",
  "ideo5",               "categorical", "Ideology (5 point)",            "In general, how would you describe your own political viewpoint?",
  "approval_pres",       "categorical", "President approval",            "Do you approve of the way each is doing their job... [Pipe Incumbent President]",
  "approval_rep",        "categorical", "House Representative approval", "Do you approve of the way each is doing their job... [Pipe Incumbent Representative's Name]",
  "approval_sen1",       "categorical", "Senator 1 approval",            "Do you approve of the way each is doing their job... [Pipe Incumbent Senator 1's Name]",
  "approval_sen2",       "categorical", "Senator 2 approval",            "Do you approve of the way each is doing their job... [Pipe Incumbent Senator 2's Name]",
  "approval_gov",        "categorical", "Governor approval",             "Do you approve of the way each is doing their job... Governor of [Pipe State]",
  "vv_regstatus",        "categorical", "Validated registration status",                      "[Validation results. Missing if validation was not conducted in the year. Categories are aggregated. Both Matched-not registered and unmatched are labeled as a no record.]",
  "turnout_self",        "categorical", "Self-reported turnout General Election", "2020: Do you intend to vote in the 2020 general election on November 3rd?",
  "vv_party_gen",        "categorical", "Validated registered party",                         "[Validation results. Only available for some states and years]",
  "vv_party_prm",        "categorical", "Validated registered Primary party",                 "[Validation results. Only available for some states and years]",
  "vv_turnout_gvm",      "categorical", "Validated turnout General Election",                 "[Validation results. All vote methods (polling, mail, early, unknown, etc..) are aggregated as a vote.]",
  "vv_turnout_pvm",      "categorical", "Validated turnout Primary Election (Congressional)", "[Validation results. Congressional primaries.]",
  "vv_state",            "categorical", "Validated state of registration", "[Validation results.]", 
  "intent_turnout_self", "categorical", "Self-reported turnout (pre-election wave)", "2020: Do you intend to vote in the 2020 general election on November 3rd?",
  "voted_turnout_self",  "categorical", "Self-reported turnout (post-election wave)", "2020: Which of the following statements best describes you?",
  "reg_self",  "categorical", "Self-reported Registration (votereg)", "Are you registered to vote?",
  "voted_pres_24",       "categorical", "2024 President vote choice (after voting)",   "[If reported voting] 2024: For whom did you vote for President of the United States? [Post-election]",
  "intent_pres_24",      "categorical", "2024 President preference (before voting)",    "Which candidate for President of the United States do you prefer?",
  "voted_pres_20",       "categorical", "2020 President vote choice (after voting)",   "[If reported voting] 2024: For whom did you vote for President of the United States? [Post-election]",
  "intent_pres_20",      "categorical", "2020 President preference (before voting)",    "Which candidate for President of the United States do you prefer?",
  "voted_pres_16",       "categorical", "2016 President vote choice (after voting)",   "2017: In the election for U.S. President, who did you vote for? [If reported voting] 2016: For whom did you vote for President of the United States? [Post-election]",
  "intent_pres_16",      "categorical", "2016 President preference (before voting)",    "Which candidate did you prefer for President of the United States?",
  "voted_pres_12",       "categorical", "2012 President vote choice (after voting)",   "2012: For whom did you vote for President of the United States? 2016: In 2012, who did you vote for in the election for President? [see guide for wording in all years]",
  "intent_pres_12",      "categorical", "2012 President preference (before voting)",    "In the race for President of the United States, who do you prefer?",
  "voted_pres_08",       "categorical", "2008 President vote choice (after voting)",   "2008: For which candidate for President of the United States did you vote?  [see guide for wording in all years]",
  "intent_pres_08",      "categorical", "2008 President preference (before voting)",    "For which candidate for President of the United States would you vote?",
  "intent_rep",          "categorical", "House preference (before voting)",             "In the general election for U.S. House of Representatives in your area, who do you prefer?",
  "intent_sen",          "categorical", "Senate preference (before voting)",            "In the race for U.S. Senator in your state, who do you prefer?",
  "intent_gov",          "categorical", "Governor preference (before voting)",          "In the race for Governor in your state, who do you prefer?",
  "voted_rep",           "categorical", "House vote choice (after voting)",            "For whom did you vote for U.S. House?",
  "voted_sen",           "categorical", "Senate vote choice (after voting)",           "For whom did you vote for U.S. Senator?",
  "voted_gov",           "categorical", "Governor vote choice (after voting)",         "For whom did you vote for Governor?",
  "intent_rep_chosen",   "text",        "House preference name",     "[Text chosen by respondent in intent_rep]",
  "intent_sen_chosen",   "text",        "Senate preference name",    "[Text chosen by respondent in intent_sen]",
  "intent_gov_chosen",   "text",        "Governor preference name",  "[Text chosen by respondent in intent_gov]",
  "voted_rep_chosen",    "text",        "House vote choice name",    "[Text chosen by respondent in voted_rep]",
  "voted_sen_chosen",    "text",        "Senate vote choice name",   "[Text chosen by respondent in voted_sen]",
  "voted_gov_chosen",    "text",        "Governor vote choice name", "[Text chosen by respondent in voted_gov]",
  "intent_pres_party",   "text",        "President preference party", "[Party of presidential candidate chosen in intent_pres]",
  "intent_rep_party",    "text",        "House preference party",     "[Party of candidate chosen by respondent in intent_rep]",
  "intent_sen_party",    "text",        "Senate preference party",    "[Party of candidate chosen by respondent in intent_sen]",
  "intent_gov_party",    "text",        "Governor preference party",  "[Party of candidate chosen by respondent in intent_gov]",
  "voted_pres_party",     "text",       "President vote in last election", "[Party of presidential candidate chosen in last election]",
  "voted_rep_party",     "text",        "House vote choice party",    "[Party of candidate chosen by respondent in voted_rep]",
  "voted_sen_party",     "text",        "Senate vote choice party",   "[Party of candidate chosen by respondent in voted_sen]",
  "voted_gov_party",     "text",        "Governor vote choice party", "[Party of candidate chosen by respondent in voted_gov]",
  "intent_rep_fec",      "text",        "House preference candidate ID",      "[FEC ID for candidate chosen in intent_rep (via linkage)]",
  "intent_sen_fec",      "text",        "Senate preference candidate ID",     "[FEC ID for candidate chosen in intent_sen (via linkage)]",
  "intent_gov_fec",      "text",        "Governor preference candidate ID",   "[FEC ID for candidate chosen in intent_gov (via linkage)]",
  "voted_rep_fec",       "text",        "House vote choice candidate ID",     "[FEC ID for candidate chosen in voted_rep (via linkage)]",
  "voted_sen_fec",       "text",        "Senate vote choice candidate ID",    "[FEC ID for candidate chosen in voted_sen (via linkage)]",
  "voted_gov_fec",       "text",        "Governor vote choice candidate ID",  "[FEC ID for candidate chosen in voted_gov (via linkage)]",
  "rep_current",         "text",        "Current House Representative name", "[Name and party used in CCES]",
  "sen1_current",        "text",        "Current Senator 1 name",            "[Name and party used in CCES]",
  "sen2_current",        "text",        "Current Senator 2 name",            "[Name and party used in CCES]",
  "gov_current",         "text",        "Current Governor name",             "[Name and party used in CCES]",
  "rep_icpsr",           "text",        "Current House Representative ID", "[ICPSR / NOMINATE]",
  "sen1_icpsr",          "text",        "Current Senator 1 ID",     "[ICPSR / NOMINATE]",
  "sen2_icpsr",          "text",        "Current Senator 2 ID",     "[ICPSR / NOMINATE]",
  "gov_fec",             "text",        "Current Governor ID",      "[FEC ID]"
)

# need to be unique
stopifnot(n_distinct(ccc_meta$alias) == nrow(ccc_meta))
stopifnot(n_distinct(ccc_meta$name) == nrow(ccc_meta))

# NOTE: `iss_meta` (issue-variable metadata) was Crunch-only and has been moved to
# older/01_issuevars-labels.R (2026-06-26).


# Save ----
saveRDS(master, "data/output/02_questions/variable_std_key.Rds")
saveRDS(ccc_meta, "data/output/02_questions/cumulative_vartable.Rds")

cli::cli_alert_success("Finished naming and describing variables")
