library(tidyverse)
library(haven)
library(rcces)

cc16 <- read_dta("data/source/cces/2016_cc.dta")


cc16_hou <- cc16 %>%
  select(inputstate, cdid113,  cdid115,  matches("CurrentHouseName"), matches("HouseCand")) %>% 
  select(-matches("post")) %>% 
  mutate(inputstate = as.character(as_factor(inputstate))) %>% 
  rename(state = inputstate) %>% 
  distinct() %>% 
  arrange(state, cdid113)


cc16_st <- cc16 %>% 
  select(inputstate, matches("CurrentGov"), matches("GovCand"), matches("CurrentSen"), matches("SenCand")) %>% 
  select(-matches("post")) %>% 
  mutate(inputstate = as.character(as_factor(inputstate))) %>% 
  rename(state = inputstate) %>% 
  distinct() %>% 
  arrange(state)


write_csv(cc16_hou, "~/Dropbox/temp/cces-2016_candidate_info_cd.csv", na = "")
write_csv(cc16_st, "~/Dropbox/temp/cces-2016_candidate_info_st.csv", na = "")
