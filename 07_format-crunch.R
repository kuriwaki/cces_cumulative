library(crunch)
library(dplyr)
library(haven)


ccc_meta <- readRDS("data/output/02_questions/cumulative_vartable.Rds")
iss_meta <- readRDS("data/output/02_questions/issuevars_vartable.Rds")

# Start up crunch -------
login() # you need a login and password to complete this command


# connect to data---------
# ds <- loadDataset("Fork of CCES Cumulative Common")
ds <- loadDataset("CCES Cumulative Common", project = "CCES")

unlock(ds)

# description for dataset
description(ds) <- "Only a limited set of questions are included for this cumulative file, and this crunch datasets has a few more variables on issue questions notin the dataverse version. The cumulative file is a combination of each year's common content; see the dataverse codebook for details. Updated and overwritten on 2019-04-29 with 2018 data and more variables. Source code and bug reports: https://github.com/kuriwaki/cces_cumulative"
startDate(ds) <- as.Date("2006-10-06")
endDate(ds) <- as.Date("2018-11-05")


# add metadata ---------

mvars <- bind_rows(iss_meta, ccc_meta)
for (j in 1:ncol(ds)) {
  if (!names(ds)[j] %in% mvars$alias) next
  else  
    name(ds[[j]]) <- mvars$name[which(mvars$alias == names(ds)[j])]
    description(ds[[j]]) <- mvars$description[which(mvars$alias == names(ds)[j])]
}

# apply weights ---
weightVariables(ds) <- list(ds$weight, ds$weight_cumulative, ds$weight_post)
weight(ds) <- ds$weight_cumulative

# change look  -----
type(ds$year) <- "categorical"
ds <- dropRows(ds, is.na(ds$year) & is.na(ds$case_id))
names(categories(ds$year)) <- c(as.character(2006:2018)) # did some fixing by hand
rollupResolution(ds$year_date) <- "Y"
type(ds$cong) <- type(ds$cong_up) <- "categorical"


# create filters ------
newFilter("Male", ds$gender == "Male", is_public = TRUE)
newFilter("Female", ds$gender == "Female", is_public = TRUE)

newFilter("White", ds$race == "White", is_public = TRUE)
newFilter("Black", ds$race == "Black", is_public = TRUE)
newFilter("Hispanic", ds$race == "Hispanic", is_public = TRUE)
newFilter("Asian", ds$race == "Asian", is_public = TRUE)
newFilter("Mixed Race", ds$race == "Mixed", is_public = TRUE)
newFilter("Other Race", 
          !(ds$race %in% c("White", "Black", "Hispanic", "Asian", "Mixed")),
          is_public = TRUE)

newFilter("High school or less", 
          ds$educ %in% c("High School Graduate", "No HS"),
          is_public = TRUE)
newFilter("Some college/assoc. degree", 
          ds$educ %in% c("Some College", "2-Year"),
          is_public = TRUE)
newFilter("College Graduate", 
          ds$educ %in% c("4-Year"),
          is_public = TRUE)
newFilter("Postgraduate study", 
          ds$educ %in% c("Post-Grad"),
          is_public = TRUE)

newFilter("Under $30,000", ds$faminc %in% c("10k - 20k", "20k - 30k", "Less than 10k"), is_public = TRUE)
newFilter("$30,000-$49,999", ds$faminc %in% c("30k - 40k", "40k - 50k"), is_public = TRUE)
newFilter("$50,000-$99,999", ds$faminc %in% c("50k - 60k", "60k - 70k","70k - 80k", "80k - 100k"), is_public = TRUE)
newFilter("$100,000-$149,999", ds$faminc %in% c("100k - 120k", "120k - 150k"), is_public = TRUE)
newFilter("$150,000 or more", ds$faminc %in% c("150k+"), is_public = TRUE)


newFilter("White Non-College", ds$race == "White" & ds$educ %in% c("High School Graduate", "No HS"), is_public = TRUE)
newFilter("White Non-College Men", ds$race == "White" & ds$educ %in% c("High School Graduate", "No HS") & ds$gender == "Male", is_public = TRUE)
newFilter("White Non-College Women", ds$race == "White" & ds$educ %in% c("High School Graduate", "No HS") & ds$gender == "Female", is_public = TRUE)

newFilter("Democrat", ds$pid3 == "Democrat", is_public = TRUE)
newFilter("Independent", ds$pid3 == "Independent", is_public = TRUE)
newFilter("Republican", ds$pid3 == "Republican", is_public = TRUE)
newFilter("Democrat (Including Leaners)", ds$pid3 == "Democrat (Including Leaners)", is_public = TRUE)
newFilter("Republican (Including Leaners)", ds$pid3 == "Republican (Including Leaners)", is_public = TRUE)
newFilter("Independent (Excluding Leaners)", ds$pid3 == "Independent (Excluding Leaners)", is_public = TRUE)

newFilter("Liberal", ds$ideo5 %in% c("Liberal", "Very Liberal"), is_public = TRUE)
newFilter("Conservative", ds$ideo5 %in% c("Conservative", "Very Conservative"), is_public = TRUE)
newFilter("Moderate", ds$ideo5 %in% c("Moderate", "Not Sure"), is_public = TRUE)

newFilter("2012", ds$year == "2012", is_public = TRUE)
newFilter("2016", ds$year == "2016", is_public = TRUE)
newFilter("2018", ds$year == "2018", is_public = TRUE)





# ordering of categories ----
st_order <- c(order(table(ds$state, useNA = "ifany"), decreasing = TRUE),
              which(ids(categories(ds$state)) < 0)) # missings
categories(ds$state) <- categories(ds$state)[c(st_order)]



# Variable Groups and ordering ------
vn <- names(ds)

ind_top <- grep("(year|state)", vn)
ind_dem <- grep("(gender|birthyr|race|hispanic|educ|age|faminc|marstat|church|bornagain|relig|church|prayer)", vn)

ind_geo <- grep("(cd|zipcode|county_fips)", vn)

ind_pid <- grep("(pid|ideo|demparty|repparty)", vn)
ind_app <- grep("(approval_.*)", vn)
ind_econ <- grep("(retro)", vn)
ind_int <- grep("newsint", vn)
ind_iss <- grep("(banassault|repeal|resent|spend|legal|security|gay|cleanair|renewable)", vn)

ind_act <- grep("(sign|meeting|candidate|donor)", vn)

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
    ind_econ, ind_pid, ind_app, ind_iss, ind_act, ind_int,
    c(ind_pres_08, ind_pres_12, ind_pres_16),  
    ind_rep, ind_sen, ind_gov, 
    ind_vv,
    ind_incID, ind_candID)
)

ordering(ds) <- VariableOrder(
  VariableGroup("Year and State", ds[ind_top]),
  VariableGroup("Demographics", ds[ind_dem]),
  VariableGroup("Geography", ds[ind_geo]),
  VariableGroup("Identity and Attitudes", ds[c(ind_pid, ind_econ, ind_app, ind_iss, ind_int)]),
  VariableGroup("Political Actions", ds[ind_act]),
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
  VariableGroup("Approval", ds[ind_app]),
  VariableGroup("Issues", ds[ind_iss]),
  VariableGroup("News Interest", ds[ind_int])
)



ordering(ds)[["Politician Names and Identifiers"]]  <- VariableOrder(
  VariableGroup("Candidates", ds[ind_candID]),
  VariableGroup("Current Representatives", ds[ind_incID])
)

# hide numeric variables for crunch
ind_nuisance_num <- grep("_num$", vn)
hideVariables(ds, ind_nuisance_num)


# lock(ds)


cat("Finished formatting Crunch dataset. ")
