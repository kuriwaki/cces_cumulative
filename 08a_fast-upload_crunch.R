library(tidyverse)
library(haven)
library(crunch)


load("data/output/01_responses/common_all.RData")


cc18

write_sav(cc18, "data/output/01_responses/cc18_crunch.sav")



login()
newDataset("https://www.dropbox.com/s/h6s9q8vullqjn2x/cc18_crunch.sav?dl=0", 
           name = "CCES 2018 Preliminary",
           owner = self(projects()[["CCES"]]))


ds <- loadDataset("CCES 2018 Preliminary", project = "CCES")

weight(ds) <- ds$commonweight
description(ds) <- "CCES 2018 Common Content. All variables, variable names not named."
startDate(ds) <- as.Date("2018-09-27")
endDate(ds) <- as.Date("2018-11-05")
