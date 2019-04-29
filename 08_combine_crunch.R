library(tidyverse)
library(crunch)
library(dplyr)
library(haven)
library(googlesheets)
library(lubridate)


login()

write_sav(bs_df, "data/release/issues_add-crunch.sav")

if (writeToCrunch) {
  login()
  deleteDataset("CCES Cumulative Issues")
  newDataset("https://www.dropbox.com/s/f9ngcutauoqgunb/issues_add-crunch.sav?dl=0", "CCES Cumulative Issues")
}

# append intent to vote party -----


ds_orig <- loadDataset("CCES Cumulative Common", project = "CCES")
forkDataset(ds_orig, "Fork of CCES Cumulative Common")

ds_fork <- loadDataset("Fork of CCES Cumulative Common")
ds_new <- loadDataset("CCES Cumulative Common Dev")

# identifier
ds_fork$year_caseid <- paste0(as.character(as.vector(ds_fork$year)), "_",  as.vector(ds_fork$case_id))
ds_new$year_caseid <- paste0(as.character(as.vector(ds_new$year)), "_",  as.vector(ds_new$case_id))

# compare
compareDatasets(ds_fork, ds_new)

vf <- names(ds_fork)
vf_drop <- setdiff(vf, c("year_caseid"))


# drop unnecessary from Dev
deleteVariables(ds_fork, vf_drop) # delete the vars to be replaced
saveVersion(ds_fork, "dropped all but year_caseid")
refresh(ds_fork)

# merge fork on new, dropping 2018 rows
extendDataset(ds_fork, ds_new, by = "year_caseid", all.x = TRUE, all.y = FALSE)
refresh(ds_fork)
saveVersion(ds_fork, description = "fork merged with new 2018")


# updte new dataset to be only 2018
ds_new <- dropRows(ds_new, ds_new$year != 2018)
refresh(ds_new)

# append
appendDataset(ds_fork, ds_new)

# un 07 format 


# merge new vars into fork
mergeFork(ds_orig, fork = ds_fork)






# Fix 2016 vote match ---------
ds <- loadDataset("CCES 2016 Common Vote Validated", project = "CCES")
crtabs(~ inputstate + CL_E2016GVM, ds, useNA = "ifany", weight = NULL) # check Northeastern states

login()
ds <- loadDataset("Fork of CCES 2016 Common Vote Validated")
crtabs(~ inputstate + CL_E2016GVM, ds, useNA = "ifany", weight = NULL) # check Northeastern states

if(FALSE){
  
  ds16 <- loadDataset("CCES Cumulative Common 2016") # old dataset, 2006 - 2016
  ds17 <- loadDataset("CCES Cumulative Common 2017") # new dataset, only 2017 
  
  ds16_17 <- appendDataset(ds16, ds17)
  compareDatasets(ds16, ds17)
  
  # something wrong with variable number 6?
  ds16[[6]]
  ds17[[6]]
  
}

# fix 2016 cumulative --------

# upload 2016
cc16 <- read_dta("~/Dropbox/CCES_SDA/2016/data/Common/CCES16_Common_OUTPUT_Feb2018_VV.dta") %>% 
  select(V101, matches("weight"), matches("^CL")) # vars to replace + ID

# insert it once

if (FALSE) { # don't run again
  insert <- loadDataset("CCES 2016 Jan 2018")
  old16_fork <- loadDataset("Fork of CCES 2016 Common Vote Validated") # old version (will get overwritten)
  
  # insert is the vars to replacement + ID
  vars_to_replace <- setdiff(names(insert), "V101")
  
  # delete the "wrong" variables
  deleteVariables(old16_fork, vars_to_replace)
  
  # immediately add back the "correct" variables in its place
  joined <- extendDataset(old16_fork, insert, by = "V101")
  
}


# add to weights 
login()
ds <- loadDataset("Fork of CCES 2016 Common Vote Validated")

# apply weights ---
weight_aliases <- str_subset(names(ds), "weight")
weightVariables(ds) <- weight_aliases
weight(ds) <- ds$commonweight_vv


# replace alias-based names with real names
ccvar <- gs_title("CCES_crunch_variables") 
v16 <- gs_read_csv(ccvar, ws = "CCES_2016_variables")
lookup <- v16 %>% select(alias = variable, name) %>% distinct()

# rename
for (cv in aliases(variables(ds))) {
  if (cv %in% lookup$alias) {
    replacement <- lookup$name[which(lookup$alias == cv)]
    if (!is.na(replacement)) {
      name(ds[[which(aliases(variables(ds)) == cv)]]) <- replacement
      print(cv)
    } else next
  }
}


# merge fork 
ds_original <- loadDataset("CCES 2016 Common Vote Validated", project = "CCES")
ds_fork <- loadDataset("Fork of CCES 2016 Common Vote Validated")

mergeFork(dataset = ds_original, fork = ds_fork)
crtabs(~ inputstate + CL_E2016GVM, ds_original, useNA = "ifany", weight = NULL) # check Northeastern states
