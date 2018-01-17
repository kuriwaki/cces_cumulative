library(crunch)
library(dplyr)


ccc_meta <- readRDS("data/output/02_questions/cumulative_vartable.Rds")

# Start up crunch -------
login() # you need a login and password to complete this command


# connect to data---------
ds <- loadDataset("CCES Cumulative Common Dev")

# description for dataset
description(ds) <- "Only a limited set of questions are included for this cumulative file. The cumulative file is a combination of each year's common content and modifies categories; see the codebook for details. Source code and bug reports: https://github.com/kuriwaki/cces_cumulative"
startDate(ds) <- "2006-10-06"
endDate(ds) <- "2006-11-07"


# add metadata ---------

# appply the name and variable
lapply(ds, function(v){
  name(v) <-        ccc_meta$name[ccc_meta$alias == alias(v)]
  description(v) <- ccc_meta$description[ccc_meta$alias == alias(v)]
})

# at the same time apply the variable labels to the dta as well
dta_not_labelled <- TRUE

if (dta_not_labelled) {
  ccc_factor <- readRDS("data/output/cumulative_2006_2016_factor.Rds")
  
  for (v in colnames(ccc_factor)) {
    attributes(ccc_factor[[v]])$label <- ccc_meta$name[which(ccc_meta$alias == v)]
  }
  
  write_dta(ccc_factor, "data/release/cumulative_2006_2016.dta")
}

# apply weights ---
weightVariables(ds) <- list(ds$weight, ds$weight_cumulative, ds$weight_vv, ds$weight_vv_post)
weight(ds) <- ds$weight_cumulative

# change look  -----
type(ds$year) <- "categorical"
names(categories(ds$year)) <- as.character(2006:2016)

type(ds$cong) <- type(ds$cong_up) <- "categorical"
names(categories(ds$cong)) <- c(as.character(109:114), "No Data")
names(categories(ds$cong_up)) <- c(as.character(110:115), "No Data")


# ordering of categories ----
st_order <- c(order(table(ds$state, useNA = "ifany"), decreasing = TRUE),
              which(ids(categories(ds$state)) < 0)) # missings
categories(ds$state) <- categories(ds$state)[c(st_order)]



# Variable Groups and ordering ------
vn <- names(ds)

ind_top <- grep("(year|state)", vn)
ind_dem <- grep("(gender|birthyr|race|educ|age)", vn)

ind_geo <- grep("(cd|zipcode|county_fips)", vn)

ind_pid <- grep("(pid)", vn)
ind_app <- grep("(approval_.*)", vn)
ind_econ <- grep("(retro)", vn)

ind_pres <- grep("(intent|voted)_pres", vn)

ind_vv  <- grep("^vv_.*", vn)

ind_int <- grep("intent_(rep|sen|gov)$", vn)
ind_vtd <- grep("voted_(rep|sen|gov)$", vn)
ind_candID  <- grep("intent_rep_chosen", vn):grep("voted_gov_fec", vn) 
ind_incID  <- grep("^rep_current$", vn):grep("^gov_fec$", vn) 


ind_wgt <- grep("(weight)", vn)
ind_tim <- grep("(starttime|cong)", vn)

ind_other <- setdiff(
  1:length(vn),
  c(ind_top, 
    ind_geo, ind_wgt, 
    ind_dem, 
    ind_econ, ind_pid, ind_app, 
    ind_pres, ind_int, ind_vtd, ind_vv,
    ind_incID, ind_candID)
)

ordering(ds) <- VariableOrder(
  VariableGroup("Year and State", ds[ind_top]),
  VariableGroup("Demographics", ds[ind_dem]),
  VariableGroup("Geography", ds[ind_geo]),
  VariableGroup("Identity and Attitudes", ds[c(ind_pid, ind_econ, ind_app)]),
  VariableGroup("Validated Vote and Turnout", ds[ind_vv]),
  VariableGroup("Presidential Preference and Vote", ds[ind_pres]),
  VariableGroup("House, Senate, and Governor Preference and Vote", ds[c(ind_int, ind_vtd)]),
  VariableGroup("Politician Names and Identifiers", ds[c(ind_candID, ind_incID)]),
  VariableGroup("Weights", ds[ind_wgt]),
  VariableGroup("Other", ds[ind_other])
)

ordering(ds)[["House, Senate, and Governor Preference and Vote"]] <- VariableOrder(
  VariableGroup("Preference", ds[ind_int]),
  VariableGroup("Vote Choice", ds[ind_vtd])
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

