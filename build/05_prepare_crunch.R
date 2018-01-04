library(tidyverse)
library(crunch)
library(haven)

# prepare for crunch upload

ccc_factor <- readRDS("data/output/cumulative_2006_2016.Rds")


# to factor

cf <- ccc_factor %>%
  mutate_if(is.labelled, as_factor)


# crunch-ready
cc <- prepareDataForCrunch(cf)



