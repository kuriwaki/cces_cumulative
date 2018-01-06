library(crunch)
library(dplyr)

# ccc <- readRDS("data/output/cumulative_2006_2016.Rds") # on disk
upload_again <- FALSE

if (upload_again) {
  newDataset(
    "data/output/cumulative_2006_2016.sav.zip",
    name = "CCES Common Content/cumulative_beta"
  )
}


# Start up crunch -------
login() # you need a login and password to complete this command


# connect to data---------
ds <- loadDataset("cumulative_2006_2016.sav", project = "CCES")


# description for dataset
description(ds) <- "This is a working version -- formatting incomplete and may contain errors."
startDate(ds) <- "2006-10-06"
endDate(ds) <- "2006-11-07"





# add metadata ---------

ccc_meta <- tribble(
  ~alias, ~type, ~name, ~description,
  "year",                "categorical",  "Year", "[Year of CCES Common Content]",
  "starttime",           "datetime",    "Start Time", "[Pre-election wave start time]",
  "caseID",              "text",        "Identifier", "[Case (Respondent) Idenfitier. Unique within year]",
  "weight",              "numeric",     "Common content weight", "[weights from pre-survey of each year]",
  "weight_cumulative",   "numeric",     "Survey Weight (Cumulative)", "[weight variable with simple adjustment: mulitiplied a constant within year to make years comparable]",
  "CD",                  "categorical", "Congressional District", "[Congressional District (Imputed from input zipcode)]",
  "state",               "categorical", "State", "[State (Imputed from input zipcode)]",
  "gender",              "categorical", "Gender", "Are you male or female?",
  "birthyr",             "numeric",     "Year of Birth", "In what year were you born?",
  "educ",                "categorical", "Education", "What is the highest level of education you have completed?",
  "race",                "categorical", "Race", "What racial or ethnic group best describes you?",
  "pid3",                "categorical", "Partisan Identity", "Generally speaking, do you think of yourself as a ...?",
  "pid7",                "categorical", "Partisan Identity (7 point)", "[based on branching from Partisan Identity question]",
  "approval_pres",       "categorical", "President Approval", "Do you approve of the way each is doing their job... [Pipe Incumbent President]",
  "approval_rep_char",   "categorical", "House Representative Approval", "Do you approve of the way each is doing their job... [Pipe Incumbent Representative's Name and Party]",
  "approval_sen1_char",  "categorical", "Senator 1 Approval", "Do you approve of the way each is doing their job... [Pipe Incumbent Senator's Name and Party]",
  "approval_sen2_char",  "categorical", "Senator 2 Approval", "Do you approve of the way each is doing their job... [Pipe Incumbent Senator's Name and Party]",
  "approval_gov",        "categorical", "Governor Approval", "Do you approve of the way each is doing their job... [Pipe Incumbent Governor's Name and Party]",
  "voted_pres_16_char",  "categorical", "2016 President vote choice", "For whom did you vote for President of the United States? [post, with early voters in pre coalesced]",
  "intent_pres_16_char", "categorical", "2016 President preference",  "Which candidate did you prefer for President of the United States?",
  "voted_pres_12_char",  "categorical", "2012 President vote choice", "[2012 wording] For whom did you vote for President of the United States? [2016 wording]: In 2012, who did you vote for in the election for President? [see appendix for wording in all years]",
  "intent_pres_12_char", "categorical", "2012 President preference", "In the race for President of the United States, who do you prefer?",
  "voted_pres_08_char",  "categorical", "2008 President vote choice", "[2008 wording] For which candidate for President of the United States did you vote?  [see appendix for wording in all years]",
  "intent_pres_08_char", "categorical", "2008 President preference", "For which candidate for President of the United States would you vote?",
  "intent_rep_char",     "categorical", "House preference", "In the general election for U.S. House of Representatives in your area, who do you prefer?",
  "voted_rep_char",      "categorical", "House vote choice", "For whom did you vote for U.S. House?",
  "intent_sen_char",     "categorical", "Senate preference", "In the race for U.S. Senator in your state, who do you prefer?",
  "voted_sen_char",      "categorical", "Senate vote choice", "For whom did you vote for U.S. Senator?",
  "intent_gov_char",     "categorical", "Governor preference", "In the race for Governor in your state, who do you prefer?",
  "voted_gov_char",      "categorical", "Governor vote choice", "For whom did you vote for Governor?"
  )

# appply

lapply(ds, function(v){
  name(v) <-        ccc_meta$name[ccc_meta$alias == alias(v)]
  description(v) <- ccc_meta$description[ccc_meta$alias == alias(v)]
})


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

ind_adm <- grep("year|starttime", vn)
ind_geo <- grep("CD|state", vn)
ind_wgt <- grep("weight", vn)
ind_dem <- grep("gender|birthyr|race|educ|pid", vn)

ind_app <- grep("approval_.*", vn)
ind_app <- setdiff(ind_app, grep("_num$", vn)) # don't need numerics

ind_pres <- setdiff(grep("pres.*char", vn), ind_app)

ind_int <- setdiff(grep("intent.*char", vn), ind_pres)
ind_vtd <- setdiff(grep("voted.*char", vn), ind_pres)

ind_other <- setdiff(
  1:length(vn),
  c(ind_adm, ind_geo, ind_wgt, ind_dem, ind_app, ind_pres, ind_int, ind_vtd)
)

ordering(ds) <- VariableOrder(
  VariableGroup("Administration", ds[ind_adm]),
  VariableGroup("Geography", ds[ind_geo]),
  VariableGroup("Demographics", ds[ind_dem]),
  VariableGroup("Presidential Intent and Vote", ds[ind_pres]),
  VariableGroup("Vote Intent (Other Offices)", ds[ind_int]),
  VariableGroup("Voted (Other Offices)", ds[ind_vtd]),
  VariableGroup("Approval", ds[ind_app]),
  VariableGroup("Weights", ds[ind_wgt]),
  VariableGroup("Other", ds[ind_other])
)