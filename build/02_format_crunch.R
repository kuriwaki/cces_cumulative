library(crunch)
library(ggplot2)
library(dplyr)
library(foreach)
library(readr)
library(xtable)


ccc <- haven::read_sav("data/output/cumulative_2006_2016.sav")

# Start up crunch -------
login() # you need a login and password to complete this command


# connect to data---------
proj <- projects()[["CCES Common Content"]]

ds <- loadDataset(datasets(proj)[[1]])


# add question wording?

description(ds$year) <- "[Year of CCES Common Content]"
description(ds$caseID) <- "[Case (Respondent) Idenfitier. Unique within year]"
description(ds$weight) <- "[weights from pre-survey of each year]"
description(ds$weight_cumulative) <- "[weights from pre-survey of each year, mulitiplied a constant within year to make years comparable]"
description(ds$CD) <- "[Congressional District (Imputed from input zipcode)]"
description(ds$state) <- "[State (Imputed from input zipcode)]"

description(ds$gender) <- "Are you male or female?"
description(ds$birthyr) <- "In what year were you born?"
description(ds$educ) <- "What is the highest level of education you have completed?"
description(ds$race) <- "What racial or ethnic group best describes you?"

description(ds$pid3) <- "Generally speaking, do you think of yourself as a ...?"
description(ds$pid7) <- "[branching based on pid3]"

description(ds$approval_rep_char) <- "Do you approve of the way each is doing their job... [Pipe Incumbent Representative's Name and Party]"
description(ds$approval_sen1_char) <- "Do you approve of the way each is doing their job... [Pipe Incumbent Senator's Name and Party]"
description(ds$approval_sen2_char) <- "Do you approve of the way each is doing their job... [Pipe Incumbent Senator's Name and Party]"
description(ds$approval_gov_char) <- "Do you approve of the way each is doing their job... [Pipe Incumbent Governor's Name and Party]"

description(ds$voted_pres_16_char) <- "For whom did you vote for President of the United States?"
description(ds$intent_pres_16_char) <- "Which candidate did you prefer for President of the United States?"

description(ds$voted_pres_12_char) <- "2012 wording: For whom did you vote for President of the United States? 2016 wording: Which candidate did you prefer for President of the United States? (see appendix for wording in all years)"
description(ds$intent_pres_12_char) <- "In the race for President of the United States, who do you prefer?"

description(ds$intent_rep_char) <- "In the general election for U.S. House of Representatives in your area, who do you prefer?"
description(ds$voted_rep_char) <- "For whom did you vote for U.S. House?"

description(ds$intent_sen_char) <- "In the race for U.S. Senator in your state, who do you prefer?"
description(ds$voted_sen_char) <- "For whom did you vote for U.S. Senator?"

description(ds$intent_gov_char) <- "In the race for Governor in your state, who do you prefer?"
description(ds$voted_gov_char) <- "For whom did you vote for Governor?"


# Variable Groups and ordering ------
vn <- names(ds)

ind_ids <- grep("year|caseID", vn)
ind_geo <- grep("CD|state", vn)
ind_wgt <- grep("weight", vn)
ind_dem <- grep("gender|birthyr|race|educ|pid", vn)

ind_app <- grep("approval_.*char", vn)
ind_pres <- setdiff(grep("pres.*char", vn), ind_app)

ind_int <- setdiff(grep("intent.*char", vn), ind_pres)
ind_vtd <- setdiff(grep("voted.*char", vn), ind_pres)

ind_other <- setdiff(1:length(vn),
                     c(ind_ids, ind_geo, ind_wgt, ind_dem, ind_app, ind_pres, ind_int, ind_vtd))

ordering(ds) <- VariableOrder(
  VariableGroup("Identifiers", ds[ind_ids]),
  VariableGroup("Geography", ds[ind_geo]),
  VariableGroup("Demographics", ds[ind_dem]),
  VariableGroup("Approval", ds[ind_app]),
  VariableGroup("Presidential Intent and Vote", ds[ind_pres]),
  VariableGroup("Vote Intent (Other Offices)", ds[ind_int]),
  VariableGroup("Voted (Other Offices)", ds[ind_vtd]),
  VariableGroup("Weights", ds[ind_wgt]),
  VariableGroup("Other", ds[ind_other])
)



logout()