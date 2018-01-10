library(crunch)
library(dplyr)


# Start up crunch -------
login() # you need a login and password to complete this command


# connect to data---------
ds <- loadDataset("CCES Cumulative Common Dev")

# description for dataset
description(ds) <- "This is a working version -- formatting incomplete and may contain errors. Only a limited set of questions are included for this cumulative file. The cumulative file is a combination of each years common content and may contain minor errors. To ask a question or report a bug, file an issue at https://github.com/kuriwaki/cces_cumulative, where the build code can be seen."
startDate(ds) <- "2006-10-06"
endDate(ds) <- "2006-11-07"


# add metadata ---------

ccc_meta <- tribble(
  ~alias, ~type, ~name, ~description,
  "year",                "categorical", "Year", "[Year of CCES Common Content]",
  "starttime",           "datetime",    "Start Time", "[Pre-election wave start time]",
  "caseID",              "text",        "Identifier", "[Case (Respondent) Idenfitier. Unique within year]",
  "weight",              "numeric",     "Common content weight", "[weights from pre-survey of each year]",
  "weight_cumulative",   "numeric",     "Survey Weight (Cumulative)", "[weight variable with simple adjustment: mulitiplied a constant within year to make years comparable]",
  "CD",                  "categorical", "Congressional District", "[Current Congressional District (Imputed from input zipcode)]",
  "cdid",                "categorical", "Congressional District Number", "[Congressional District (Imputed from input zipcode)]",
  "state",               "categorical", "State", "[State (Imputed from input zipcode)]",
  "st",                  "categorical", "State Abbreviation", "[State (Imputed from input zipcode)]",
  "zipcode",             "text",        "Zipcode of Residence", "So that we can ask you about the news and events in your area, in what zip code do you currently reside?",
  "countyFIPS",          "text",        "County of Residence", "[]",
  "gender",              "categorical", "Gender", "Are you male or female?",
  "birthyr",             "numeric",     "Year of Birth", "In what year were you born?",
  "age",                 "numeric",     "Age", "[Approximate; computed from the year of survey minus Year of Birth]",
  "educ",                "categorical", "Education", "What is the highest level of education you have completed?",
  "race",                "categorical", "Race", "What racial or ethnic group best describes you?",
  "pid3",                "categorical", "Partisan Identity", "Generally speaking, do you think of yourself as a ...?",
  "pid7",                "categorical", "Partisan Identity (7 point)", "[based on branching from Partisan Identity question]",
  "approval_pres",       "categorical", "President Approval", "Do you approve of the way each is doing their job... [Pipe Incumbent President]",
  "approval_rep",   "categorical", "House Representative Approval", "Do you approve of the way each is doing their job... [Pipe Incumbent Representative's Name and Party]",
  "approval_sen1",  "categorical", "Senator 1 Approval", "Do you approve of the way each is doing their job... [Pipe Incumbent Senator's Name and Party]",
  "approval_sen2",  "categorical", "Senator 2 Approval", "Do you approve of the way each is doing their job... [Pipe Incumbent Senator's Name and Party]",
  "approval_gov",        "categorical", "Governor Approval", "Do you approve of the way each is doing their job... [Pipe Incumbent Governor's Name and Party]",
  "economy_retro",       "categorical", "Retrospective Economy", "OVER THE PAST YEAR the nation's economy has ...?",
  "vv_regstatus",        "categorical", "Validated Registration Status", "[Validation Results. Missing if validation was not conducted in the year. Categories are aggregated. Both Matched-not registered and unmatched are labelled as a no record.]",
  "vv_party_gen",        "categorical", "Validated Registered Party", "[Validation Results.]",
  "vv_party_prm",        "categorical", "Validated Registered Primary Party", "[Validation Results. All vote methods (polling, mail, early, unknown, etc..) are aggregated as a vote.]",
  "vv_turnout_gvm",      "categorical", "Validated Turnout General Election", "[Validation Results. All vote methods (polling, mail, early, unknown, etc..) are aggregated as a vote.]",
  "vv_turnout_pvm",      "categorical", "Validated Turnout Primary Election (Congressional)", "[Validation Results]",
  "voted_pres_16",  "categorical", "2016 President vote choice", "For whom did you vote for President of the United States? [post, with early voters in pre coalesced]",
  "intent_pres_16", "categorical", "2016 President preference",  "Which candidate did you prefer for President of the United States?",
  "voted_pres_12",  "categorical", "2012 President vote choice", "[2012 wording] For whom did you vote for President of the United States? [2016 wording]: In 2012, who did you vote for in the election for President? [see appendix for wording in all years]",
  "intent_pres_12", "categorical", "2012 President preference", "In the race for President of the United States, who do you prefer?",
  "voted_pres_08",  "categorical", "2008 President vote choice", "[2008 wording] For which candidate for President of the United States did you vote?  [see appendix for wording in all years]",
  "intent_pres_08", "categorical", "2008 President preference", "For which candidate for President of the United States would you vote?",
  "approval_rep_num",    "categorical", "approval_rep_num", "[Response values (may be specific to year)]",
  "approval_sen1_num",   "categorical", "approval_sen1_num", "[Response values (may be specific to year)]",
  "approval_sen2_num",   "categorical", "approval_sen2_num", "[Response values (may be specific to year)]",
  "voted_pres_16_num",   "categorical", "voted_pres_16_num", "[Response values (may be specific to year)]",
  "intent_pres_16_num",  "categorical", "intent_pres_16_num", "[Response values (may be specific to year)]",
  "voted_pres_12_num",   "categorical", "voted_pres_12_num", "[Response values (may be specific to year)]",
  "intent_pres_12_num",  "categorical", "intent_pres_12_num", "[Response values (may be specific to year)]",
  "voted_pres_08_num",   "categorical", "voted_pres_08_num", "[Response values (may be specific to year)]",
  "intent_pres_08_num",  "categorical", "intent_pres_08_num", "[Response values (may be specific to year)]",
  "intent_rep",          "categorical", "House preference", "In the general election for U.S. House of Representatives in your area, who do you prefer?",
  "intent_sen",          "categorical", "Senate preference", "In the race for U.S. Senator in your state, who do you prefer?",
  "intent_gov",          "categorical", "Governor preference", "In the race for Governor in your state, who do you prefer?",
  "voted_rep",           "categorical", "House vote choice", "For whom did you vote for U.S. House?",
  "voted_sen",           "categorical", "Senate vote choice", "For whom did you vote for U.S. Senator?",
  "voted_gov",           "categorical", "Governor vote choice", "For whom did you vote for Governor?",
  "intent_rep_chosen",   "text",        "House preference (shown)", "[Text chosen by respondent]",
  "intent_sen_chosen",   "text",        "Senate preference (shown)", "[Text chosen by respondent]",
  "intent_gov_chosen",   "text",        "Governor preference (shown)", "[Text chosen by respondent]",
  "voted_rep_chosen",    "text",        "House vote choice (shown)", "[Text chosen by respondent]",
  "voted_sen_chosen",    "text",        "Senate vote choice (shown)", "[Text chosen by respondent]",
  "voted_gov_chosen",    "text",        "Governor vote choice (shown)", "[Text chosen by respondent]",
  "intent_rep_fec",      "text",        "ID for House preference",     "[FEC ID]",
  "intent_sen_fec",      "text",        "ID for Senate preference",    "[FEC ID]",
  "intent_gov_fec",      "text",        "ID for Governor preference",  "[FEC ID]",
  "voted_rep_fec",       "text",        "ID for House vote choice",    "[FEC ID]",
  "voted_sen_fec",       "text",        "ID for Senate vote choice",   "[FEC ID]",
  "voted_gov_fec",       "text",        "ID for Governor vote choice", "[FEC ID]",
  "hou_inc",             "text",        "Current House Rep", "",
  "sen1_inc",            "text",        "Current Senator 1", "",
  "sen2_inc",            "text",        "Current Senator 2", "",
  "gov_inc",             "text",        "Current Governor", "",
  "hou_icpsr",           "text",        "ID for current House Rep", "[ICPSR / NOMINATE]",
  "sen1_icpsr",          "text",        "ID for current Senator 1", "[ICPSR / NOMINATE]",
  "sen2_icpsr",          "text",        "ID for current Senator 2", "[ICPSR / NOMINATE]",
  "gov_fec",             "text",        "ID for current Governor", "[FEC ID]"
  )

# need to be unique
stopifnot(n_distinct(ccc_meta$alias) == nrow(ccc_meta))
stopifnot(n_distinct(ccc_meta$name) == nrow(ccc_meta))

# appply the name and variable
lapply(ds, function(v){
  name(v) <-        ccc_meta$name[ccc_meta$alias == alias(v)]
  description(v) <- ccc_meta$description[ccc_meta$alias == alias(v)]
})

# at the same time apply the variable labels to the dta as well
dta_not_labelled <- FALSE 

if (dta_not_labelled) {
  ccc_factor <- readRDS("data/output/cumulative_2006_2016_preStata.Rds")
  
  for (v in colnames(ccc_factor)) {
    attributes(ccc_factor[[v]])$label <- ccc_meta$name[which(ccc_meta$alias == v)]
  }
  
  write_dta(ccc_factor, "data/release/cumulative_2006_2016.dta")
}

# apply weights ---
weight(ds) <- ds$weight_cumulative

# change look  -----
type(ds$year) <- "categorical"
names(categories(ds$year)) <- as.character(2006:2016)


# ordering of categories ----
st_order <- c(order(table(ds$state, useNA = "ifany"), decreasing = TRUE),
              which(ids(categories(ds$state)) < 0)) # missings
categories(ds$state) <- categories(ds$state)[c(st_order)]



# Variable Groups and ordering ------
vn <- names(ds)

ind_adm <- grep("(year|starttime)", vn)
ind_geo <- grep("(CD|state|zipcode|countyFIPS)", vn)
ind_wgt <- grep("(weight)", vn)
ind_dem <- grep("(gender|birthyr|race|educ|pid|age)", vn)

ind_app <- grep("(retro|approval_.*)", vn)

ind_pres <- grep("(intent|voted)_pres", vn)

ind_vv  <- grep("^vv_.*", vn)

ind_int <- grep("intent_(rep|sen|gov)$", vn)
ind_vtd <- grep("voted_(rep|sen|gov)$", vn)
ind_candID  <- grep("intent_rep_chosen", vn):grep("voted_gov_fec", vn) 
ind_incID  <- grep("^hou_inc$", vn):grep("^gov_fec$", vn) 


ind_other <- setdiff(
  1:length(vn),
  c(ind_adm, ind_geo, ind_wgt, ind_dem, ind_app, 
    ind_pres, ind_int, ind_vtd, ind_vv,
    ind_incID, ind_candID)
)

ordering(ds) <- VariableOrder(
  VariableGroup("Administration", ds[ind_adm]),
  VariableGroup("Geography", ds[ind_geo]),
  VariableGroup("Demographics", ds[ind_dem]),
  VariableGroup("Presidential Preference and Vote", ds[ind_pres]),
  VariableGroup("House, Senate, and Governor Preference and Vote", ds[c(ind_int, ind_vtd)]),
  VariableGroup("Validated Vote and Turnout", ds[ind_vv]),
  VariableGroup("Approval", ds[ind_app]),
  VariableGroup("Politician Names and Identifiers", ds[c(ind_candID, ind_incID)]),
  VariableGroup("Weights", ds[ind_wgt]),
  VariableGroup("Other", ds[ind_other])
)
ordering(ds)[["House, Senate, and Governor Preference and Vote"]] <- VariableOrder(
  VariableGroup("Preference", ds[ind_int]),
  VariableGroup("Vote Choice", ds[ind_vtd])
)

ordering(ds)[["Politician Names and Identifiers"]]  <- VariableOrder(
  VariableGroup("Candidates", ds[ind_candID]),
  VariableGroup("Current Representatives", ds[ind_incID])
)

# hide numeric variables for crunch
ind_nuisance_num <- grep("_num$", vn)
hideVariables(ds, ind_nuisance_num)

