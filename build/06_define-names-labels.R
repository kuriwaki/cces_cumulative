library(tibble)



# each row is a variable for the starndardized data, each column is for the cces year0
master <- 
  tribble(~name,      ~`2006`, ~`2007`,     ~`2008`, ~`2009`, ~`2010`, ~`2011`, ~`2012`,            ~`2013`,            ~`2014`,            ~`2015`,            ~`2016`,
          "rep_inc",  "v5013", "repname",    "V527",  "v627",  "V501",  "V501", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName",
          "rep_ipt",  "v5014", NA,           "V535",  NA,      "V502",  NA,     "CurrentHouseParty","CurrentHouseParty","CurrentHouseParty","CurrentHouseParty","CurrentHouseParty",
          "sen1_inc", "v5015", "sen1name",   "V551",  "v651",  "V513",  "V513", "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",
          "sen1_ipt", "v5016", NA,           "V544",  NA,      "V514",  NA,     "CurrentSen1Party", "CurrentSen1Party", "CurrentSen1Party", "CurrentSen1Party", "CurrentSen1Party",
          "sen2_inc", "v5017", "sen2name",   "V552",  "v652",  "V521",  "V521", "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",
          "sen2_ipt", "v5018", NA,           "V548",  NA,      "V522",  NA,     "CurrentSen2Party", "CurrentSen2Party", "CurrentSen2Party", "CurrentSen2Party", "CurrentSen2Party",
          "gov_inc",  "v5019", "govname",    "V508",  "v608",  "V529",  "V529", "CurrentGovName",   "CurrentGovName",   "CurrentGovName",   "CurrentGovName",   "CurrentGovName",
          "gov_ipt",  "v5020", NA,           "V513",  NA,      "V530",  NA,     "CurrentGovParty",  "CurrentGovParty",  "CurrentGovParty",  "CurrentGovParty",  "CurrentGovParty",
          "rep_can1", "v5001", NA,           "V518",  NA,      "V533",  NA,     "HouseCand1Name",   NA,                 "HouseCand1Name",   NA,                 "HouseCand1Name",
          "rep_pty1", "v5002", NA,            NA,     NA,      NA,      NA,     "HouseCand1Party",  NA,                 "HouseCand1Party",  NA,                 "HouseCand1Party",
          "rep_can2", "v5003", NA,           "V519",  NA,      "V536",  NA,     "HouseCand2Name",   NA,                 "HouseCand2Name",   NA,                 "HouseCand2Name",
          "rep_pty2", "v5004", NA,            NA,     NA,      NA,      NA,     "HouseCand2Party",  NA,                 "HouseCand2Party",  NA,                 "HouseCand2Party",
          "rep_can3",  NA,     NA,           "V520",  NA,      "V539",  NA,     "HouseCand3Name",   NA,                 "HouseCand3Name",   NA,                 "HouseCand3Name",
          "rep_pty3",  NA,     NA,           "V533",  NA,      "V542",  NA,     "HouseCand3Party",  NA,                 "HouseCand3Party",  NA,                 "HouseCand3Party",
          "sen_can1", "v5005", NA,           "V553",  NA,      "V548",  NA,     "SenCand1Name",     NA,                 "SenCand1Name",     NA,                 "SenCand1Name",
          "sen_pty1", "v5006", NA,            NA,     NA,      NA,      NA,     "SenCand1Party",    NA,                 "SenCand1Party",    NA,                 "SenCand1Party",
          "sen_can2", "v5007", NA,           "V555",  NA,      "V551",  NA,     "SenCand2Name",     NA,                 "SenCand2Name",     NA,                 "SenCand2Name",
          "sen_pty2", "v5008", NA,            NA,     NA,      NA,      NA,     "SenCand2Party",    NA,                 "SenCand2Party",    NA,                 "SenCand2Party",
          "sen_can3", NA,      NA,           "V556",  NA,      "V554",  NA,     "SenCand3Name",     NA,                 "SenCand3Name",     NA,                 "SenCand3Name",
          "sen_pty3", NA,      NA,           "V572",  NA,      "V556",  NA,     "SenCand3Party",    NA,                 "SenCand3Party",    NA,                 "SenCand3Party",
          "gov_can1", "v5009", NA,           "V501",  NA,      "V564",  NA,     "GovCand1Name",     NA,                 "GovCand1Name",     NA,                 "GovCand1Name",
          "gov_pty1", "v5010", NA,            NA,     NA,      NA,      NA,     "GovCand1Party",    NA,                 "GovCand1Party",    NA,                 "GovCand1Party",
          "gov_can2", "v5011", NA,           "V502",  NA,      "V567",  NA,     "GovCand2Name",     NA,                 "GovCand2Name",     NA,                 "GovCand2Name",
          "gov_pty2", "v5012", NA,            NA,     NA,      NA,      NA,     "GovCand2Party",    NA,                 "GovCand2Party",    NA,                 "GovCand2Party",
          "gov_can3", NA,      NA,           "V503",  NA,      "V570",  NA,     NA,                 NA,                 NA,                 NA,                 "GovCand3Name",
          "gov_pty3", NA,      NA,           "V512",  NA,      "V572",  NA,     NA,                 NA,                 NA,                 NA,                 "GovCand3Party"
  )

check_no_dupes <- function(c) if (n_distinct(master[[c]], na.rm = TRUE) != sum(!is.na(master[[c]]))) stop(glue("check column {c}"))
for (c in 2:ncol(master)) check_no_dupes(c)

saveRDS(master, "data/output/02_questions/variable_std_key.Rds")

ccc_meta <- tribble(
  ~alias, ~type, ~name, ~description,
  "year",                "categorical", "CCES year",  "[Year of CCES Common Content]",
  "starttime",           "datetime",    "Start time", "[Pre-election wave start time]",
  "case_id",             "text",        "Identifier", "[Case (Respondent) Idenfitier. Unique within year]",
  "tookpost",            "categorical", "Took Post Survey",                   "[Whether or not the respondent took the post-election wave of the survey (in even years)]",
  "weight",              "numeric",     "Common content weight",              "[weights from pre-survey of each year. ]",
  "weight_cumulative",   "numeric",     "Survey weight (Cumulative)",         "[weight variable with simple adjustment: mulitiplied a constant within year to make years comparable]",
  "weight_vv",           "numeric",     "Survey weight for validated voters", "[weight among validated voters to construct a representative sample of the voters. Only available for some years.]",
  "weight_vv_post",      "numeric",     "Survey weight for validated voters", "[weight among validated voters and post-election wave respondents to construct a representative sample of the voters. Only available for some years.]",
  "CD",                  "categorical", "Congressional district",                              "[Current Congressional District (Imputed from input zipcode)]",
  "dist",                "categorical", "Congressional district number",                       "[Congressional District (Imputed from input zipcode)]",
  "dist_up",             "categorical", "Congressional district number for upcoming Congress", "[Congressional District (Imputed from input zipcode)]",
  "cong",                "categorical", "Congressional session",                               "[Current Congressional session. Use to join incumbents]",
  "cong_up",             "categorical", "Congressional session for upcoming Congress",         "[Upcoming Congressional session. Use to join candidates.]",
  "state",               "categorical", "State",                "[State (Imputed from input zipcode)]",
  "st",                  "categorical", "State Abbreviation",   "[State (Imputed from input zipcode)]",
  "zipcode",             "text",        "Zipcode of Residence", "So that we can ask you about the news and events in your area, in what zip code do you currently reside?",
  "countyFIPS",          "text",        "County of Residence",  "[County (Imputed from input zipcode)]",
  "gender",              "categorical", "Gender",        "Are you male or female?",
  "birthyr",             "numeric",     "Year of Birth", "In what year were you born?",
  "age",                 "numeric",     "Age",           "[Approximate age computed from the year of survey minus Year of Birth]",
  "educ",                "categorical", "Education",     "What is the highest level of education you have completed?",
  "race",                "categorical", "Race",          "What racial or ethnic group best describes you?",
  "pid3",                "categorical", "Partisan identity",             "Generally speaking, do you think of yourself as a ...?",
  "pid7",                "categorical", "Partisan identity (7 point)",   "[based on branching from Partisan Identity question]",
  "approval_pres",       "categorical", "President approval",            "Do you approve of the way each is doing their job... [Pipe Incumbent President]",
  "approval_rep",        "categorical", "House Representative approval", "Do you approve of the way each is doing their job... [Pipe Incumbent Representative's Name]",
  "approval_sen1",       "categorical", "Senator 1 approval",            "Do you approve of the way each is doing their job... [Pipe Incumbent Senator 1's Name]",
  "approval_sen2",       "categorical", "Senator 2 approval",            "Do you approve of the way each is doing their job... [Pipe Incumbent Senator 2's Name]",
  "approval_gov",        "categorical", "Governor approval",             "Do you approve of the way each is doing their job... Governor of [Pipe State]",
  "economy_retro",       "categorical", "Retrospective economy",         "OVER THE PAST YEAR the nation's economy has ...?",
  "vv_regstatus",        "categorical", "Validated registration status",                      "[Validation Results. Missing if validation was not conducted in the year. Categories are aggregated. Both Matched-not registered and unmatched are labelled as a no record.]",
  "vv_party_gen",        "categorical", "Validated registered party",                         "[Validation Results.]",
  "vv_party_prm",        "categorical", "Validated registered Primary party",                 "[Validation Results. All vote methods (polling, mail, early, unknown, etc..) are aggregated as a vote.]",
  "vv_turnout_gvm",      "categorical", "Validated turnout General Election",                 "[Validation Results. All vote methods (polling, mail, early, unknown, etc..) are aggregated as a vote.]",
  "vv_turnout_pvm",      "categorical", "Validated turnout Primary Election (Congressional)", "[Validation Results]",
  "voted_pres_16",       "categorical", "2016 President vote choice",   "For whom did you vote for President of the United States? [post, with early voters in pre coalesced]",
  "intent_pres_16",      "categorical", "2016 President preference",    "Which candidate did you prefer for President of the United States?",
  "voted_pres_12",       "categorical", "2012 President vote choice",   "[2012 wording] For whom did you vote for President of the United States? [2016 wording]: In 2012, who did you vote for in the election for President? [see appendix for wording in all years]",
  "intent_pres_12",      "categorical", "2012 President preference",    "In the race for President of the United States, who do you prefer?",
  "voted_pres_08",       "categorical", "2008 President vote choice",   "[2008 wording] For which candidate for President of the United States did you vote?  [see appendix for wording in all years]",
  "intent_pres_08",      "categorical", "2008 President preference",    "For which candidate for President of the United States would you vote?",
  "intent_rep",          "categorical", "House preference",             "In the general election for U.S. House of Representatives in your area, who do you prefer?",
  "intent_sen",          "categorical", "Senate preference",            "In the race for U.S. Senator in your state, who do you prefer?",
  "intent_gov",          "categorical", "Governor preference",          "In the race for Governor in your state, who do you prefer?",
  "voted_rep",           "categorical", "House vote choice",            "For whom did you vote for U.S. House?",
  "voted_sen",           "categorical", "Senate vote choice",           "For whom did you vote for U.S. Senator?",
  "voted_gov",           "categorical", "Governor vote choice",         "For whom did you vote for Governor?",
  "intent_rep_chosen",   "text",        "House preference (shown)",     "[Text chosen by respondent]",
  "intent_sen_chosen",   "text",        "Senate preference (shown)",    "[Text chosen by respondent]",
  "intent_gov_chosen",   "text",        "Governor preference (shown)",  "[Text chosen by respondent]",
  "voted_rep_chosen",    "text",        "House vote choice (shown)",    "[Text chosen by respondent]",
  "voted_sen_chosen",    "text",        "Senate vote choice (shown)",   "[Text chosen by respondent]",
  "voted_gov_chosen",    "text",        "Governor vote choice (shown)", "[Text chosen by respondent]",
  "intent_rep_fec",      "text",        "ID for House preference",      "[FEC ID]",
  "intent_sen_fec",      "text",        "ID for Senate preference",     "[FEC ID]",
  "intent_gov_fec",      "text",        "ID for Governor preference",   "[FEC ID]",
  "voted_rep_fec",       "text",        "ID for House vote choice",     "[FEC ID]",
  "voted_sen_fec",       "text",        "ID for Senate vote choice",    "[FEC ID]",
  "voted_gov_fec",       "text",        "ID for Governor vote choice",  "[FEC ID]",
  "rep_shown",           "text",        "Current House Representative", "",
  "sen1_shown",          "text",        "Current Senator 1",            "",
  "sen2_shown",          "text",        "Current Senator 2",            "",
  "gov_shown",           "text",        "Current Governor",             "",
  "rep_icpsr",           "text",        "ID for current House Rep",     "[ICPSR / NOMINATE]",
  "sen1_icpsr",          "text",        "ID for current Senator 1",     "[ICPSR / NOMINATE]",
  "sen2_icpsr",          "text",        "ID for current Senator 2",     "[ICPSR / NOMINATE]",
  "gov_fec",             "text",        "ID for current Governor",      "[FEC ID]"
)

# need to be unique
stopifnot(n_distinct(ccc_meta$alias) == nrow(ccc_meta))
stopifnot(n_distinct(ccc_meta$name) == nrow(ccc_meta))


saveRDS(ccc_meta, "data/output/02_questions/cumulative_vartable.Rds")
