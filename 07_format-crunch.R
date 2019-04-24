library(crunch)
library(dplyr)
library(haven)


ccc_meta <- readRDS("data/output/02_questions/cumulative_vartable.Rds")
iss_meta <- readRDS("data/output/02_questions/issuevars_vartable.Rds")

# Start up crunch -------
login() # you need a login and password to complete this command


# connect to data---------
ds <- loadDataset("CCES Cumulative Common", project = "CCES")

unlock(ds)

# description for dataset
description(ds) <- "Only a limited set of questions are included for this cumulative file. The cumulative file is a combination of each year's common content and modifies categories; see the codebook for details. Source code and bug reports: https://github.com/kuriwaki/cces_cumulative"
startDate(ds) <- as.Date("2006-10-06")
endDate(ds) <- as.Date("2017-12-12")


# add metadata ---------

for (j in 1:ncol(ds)) {
  if (!names(ds)[j] %in% ccc_meta$alias) next
  else  
    name(ds[[j]]) <- ccc_meta$name[which(ccc_meta$alias == names(ds)[j])]
    description(ds[[j]]) <- ccc_meta$description[which(ccc_meta$alias == names(ds)[j])]
}

# apply weights ---
weightVariables(ds) <- list(ds$weight, ds$weight_cumulative, ds$weight_post)
weight(ds) <- ds$weight_cumulative

# change look  -----
type(ds$year) <- "categorical"
names(categories(ds$year)) <- c(as.character(2006:2017))
type(ds$cong) <- type(ds$cong_up) <- "categorical"


# ordering of categories ----
st_order <- c(order(table(ds$state, useNA = "ifany"), decreasing = TRUE),
              which(ids(categories(ds$state)) < 0)) # missings
categories(ds$state) <- categories(ds$state)[c(st_order)]



# Variable Groups and ordering ------
vn <- names(ds)

ind_top <- grep("(year|state)", vn)
ind_dem <- grep("(gender|birthyr|race|hispanic|educ|age|faminc)", vn)

ind_geo <- grep("(cd|zipcode|county_fips)", vn)

ind_pid <- grep("(pid|ideo)", vn)
ind_app <- grep("(approval_.*)", vn)
ind_econ <- grep("(retro)", vn)

ind_pres_08 <- grep("(intent|voted)_pres_08", vn)
ind_pres_12 <- grep("(intent|voted)_pres_12", vn)
ind_pres_16 <- grep("(intent|voted)_pres_16", vn)

ind_vv  <- grep("^vv_.*", vn)

ind_rep <- str_which(vn, "(intent|voted)_rep(_party|$)")
ind_sen <- str_which(vn, "(intent|voted)_sen(_party|$)")
ind_gov <- str_which(vn, "(intent|voted)_gov(_party|$)")

ind_candID  <- str_which(vn, "(intent|voted)_(rep|gov|sen)_(chosen|fec)")
ind_incID  <- grep("^rep_current$", vn):grep("^gov_fec$", vn) 


ind_wgt <- grep("(weight)", vn)
ind_tim <- grep("(starttime|cong)", vn)

ind_other <- setdiff(
  1:length(vn),
  c(ind_top, 
    ind_geo, ind_wgt, 
    ind_dem, 
    ind_econ, ind_pid, ind_app, 
    c(ind_pres_08, ind_pres_12, ind_pres_16),  
    ind_rep, ind_sen, ind_gov, 
    ind_vv,
    ind_incID, ind_candID)
)

ordering(ds) <- VariableOrder(
  VariableGroup("Year and State", ds[ind_top]),
  VariableGroup("Demographics", ds[ind_dem]),
  VariableGroup("Geography", ds[ind_geo]),
  VariableGroup("Identity and Attitudes", ds[c(ind_pid, ind_econ, ind_app)]),
  VariableGroup("Validated Vote and Turnout", ds[ind_vv]),
  VariableGroup("Presidential Preference and Vote", ds[c(ind_pres_08, ind_pres_12, ind_pres_16)]),
  VariableGroup("House, Senate, and Governor Preference and Vote", ds[c(ind_rep, ind_sen, ind_gov)]),
  VariableGroup("Politician Names and Identifiers", ds[c(ind_candID, ind_incID)]),
  VariableGroup("Weights", ds[ind_wgt]),
  VariableGroup("Other", ds[ind_other])
)


ordering(ds)[["Presidential Preference and Vote"]] <- VariableOrder(
  VariableGroup("2008 Obama - McCain",  ds[ind_pres_08]),
  VariableGroup("2012 Obama - Romney",  ds[ind_pres_12]),
  VariableGroup("2016 Trump - Clinton", ds[ind_pres_16])
)

ordering(ds)[["House, Senate, and Governor Preference and Vote"]] <- VariableOrder(
  VariableGroup("House", ds[ind_rep]),
  VariableGroup("Senate", ds[ind_sen]),
  VariableGroup("Governor", ds[ind_gov])
)

ordering(ds)[["Identity and Attitudes"]] <- VariableOrder(
  VariableGroup("Partisan Identity", ds[ind_pid]),
  VariableGroup("Economy", ds[ind_econ]),
  VariableGroup("Approval", ds[ind_app])
)

ordering(ds)[["Politician Names and Identifiers"]]  <- VariableOrder(
  VariableGroup("Candidates", ds[ind_candID]),
  VariableGroup("Current Representatives", ds[ind_incID])
)

# hide numeric variables for crunch
ind_nuisance_num <- grep("_num$", vn)
hideVariables(ds, ind_nuisance_num)


lock(ds)

## issue var

login()

ds <- loadDataset("CCES Cumulative Issues")

for (j in 1:ncol(ds)) {
  if (!names(ds)[j] %in% iss_meta$alias) next
  else  
  {
    name(ds[[j]])        <- iss_meta$name[which(iss_meta$alias == names(ds)[j])]
    description(ds[[j]]) <- iss_meta$description[which(iss_meta$alias == names(ds)[j])]
  }
  
}

type(ds[["year"]]) <- "categorical"
ds$year_date5 <- as.Datetime(ds$year_date, format = "%Y-%m-%d", resolution = "D")
rollupResolution(ds[["year_date"]]) <- "Y"

mv(ds, matches("(banassault|repeal|resent|spend|legal|security|gay|cleanair|renewable)"), "Issues")
mv(ds, matches("ideo|party$"), "Perception")
mv(ds, matches("sign|meeting|candidate|donor"), "Participation")
mv(ds, matches("(church|bornagain|relig|church|prayer)"), "Demographics")

cat("Finished formatting Crunch dataset. ")
