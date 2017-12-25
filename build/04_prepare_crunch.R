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




# upload to Crunch
newDataset(cf, name = "temp_R-upload")
ds <- loadDataset("temp_R-upload")
variableMetadata(ds)



str(attributes(cc)$metadata$body$table$metadata$starttime)


foo <- tibble(date = as.POSIXct("2017-12-20"))
newDataset(foo)


# dataset
cc12_link <- curl::curl("www.shirokuriwaki.com/datasets/cces_sample.dta")
cc12 <- read_dta(cc12_link) %>% 
  mutate_if(is.labelled, as_factor) # switcht lablled to factor b/c crunch doesn't accept

# hypothetical, manually-constructed metadata
cc12_meta <- tibble::tribble(
  ~alias, ~type, ~name, ~description,
  "state",  "categorical", "State", "[State]",
  "gender", "categorical", "Gender", "What is your Gender?",
  "race",   "categorical", "Race", "What racial or ethnic group best describes you?",
  "employ", "categorical", "Employment Status", "Which of the following best describes your current employment status?",
  "family_income", "categorical", "Family Income", "Thinking back over the last year, what was your family's annual income?",
  "religion", "categorical", "Religion", "What is your present religion, if any?",
  "obama12", "numeric", "Obama Vote", "[vote for Obama]") 

# somehow incorporate cc12_meta when creating
newDataset(cc12, "sample_cces-2012")

# instead of assigning variable descriptions one-by-one, post-upload?

ccc12 <- loadDataset("sample_cces-2012")

