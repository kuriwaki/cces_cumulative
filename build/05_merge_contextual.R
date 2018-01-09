library(tidyverse)
library(haven)



# Data -------------
load("data/output/01_responses/vote_responses.RData")
load("data/output/01_responses/incumbents_key.RData")
load("data/output/01_responses/candidates_key.RData")
ccc <- readRDS("data/output/01_responses/cumulative_stacked.Rds")
rmaster <- readRDS("data/output/01_responses/repsondent_contextual.Rds")




# apppend the candidatename-candidate party variables to the vote choice qes
i_hou_name <- left_join(i_rep, hc_key, by = c("year", "caseID", "intent_rep_num" = "cand"))
i_sen_name <- left_join(i_sen, sc_key, by = c("year", "caseID", "intent_sen_num" = "cand"))
i_gov_name <- left_join(i_gov, gc_key, by = c("year", "caseID", "intent_gov_num" = "cand"))






# Format for output  --------
# make char variables a factor so crunch knows it's a categorical?
ccc_factor <- ccc %>%
  mutate(caseID = as.character(caseID)) %>% # better this than let crunch think its a numeric
  mutate(zipcode = as.character(zipcode)) %>%
  mutate(cdid = as.factor(cdid)) %>% # we don't want to take summary stats of this, so better a factor
  mutate(countyFIPS = str_pad(as.character(countyFIPS), width = 5, pad = "0")) %>%
  mutate_at(vars(matches("_char")), as.factor) %>%
  mutate_at(vars(matches("^CD$")), as.factor) %>%
  mutate_at(vars(matches("(state$|st$)")), as.factor)


write_sav(ccc_factor, "data/release/cumulative_2006_2016.sav")
write_dta(ccc_factor, "data/release/cumulative_2006_2016.dta")
