library(tidyverse)
library(haven)
library(rcces)

load("data/output/01_responses/common_all.RData")

# birth year
age_16 <- cc16 %>% 
  transmute(case_id, 
            birthyr = as.integer(as.character(as_factor(birthyr))))
stopifnot(all(age_16$birthyr <= 1998))
write_sav(age_16, "data/output/01_responses/cc16_age.sav")

newDataset(age_16, name = "CCES 2016 birthyr")

library(crunch)
login()
ds16 <- loadDataset("CCES 2016 Common Vote Validated", project = "CCES")
# forkDataset(ds16, name = "Fork of CCES 2016")

f16 <- loadDataset("Fork of CCES 2016")
# deleteVariables(f16, "birthyr")

a16 <- loadDataset("CCES 2016 birthyr")
a16$V101 <- a16$case_id
name(a16$V101) <- "Case Identifier 2016"
type(a16$V101) <- "text"
name(a16$birthyr) <- "Year of Birth"
# GUI Edit for display
description(a16$birthyr) <- "In what year were you born?"

extendDataset(x = f16, y = a16, by = "V101")
f16 <- refresh(f16)

# manually edit groupings

# merge
saveVersion(f16, "merged in birthyr as integer variable")
mergeFork(ds16, fork = f16)







# export for SD
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
