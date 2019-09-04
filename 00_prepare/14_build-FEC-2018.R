library(tidyverse)


dime_raw <- read_csv("data/source/dime/dime_cong_elections_current.csv")


# dime fmt

dime <- dime_raw %>% 
  filter(cycle >= 2006) %>% 
  select(cycle, fec = recipient_candid, office_sought = seat, party, st = state, district, 
         name = Name) %>% 
  mutate(namelast = str_remove(str_extract(name, "(.*)(?<=,)"), ",")) %>% 
  mutate(dist = as.integer(str_extract(district, "[0-9]+")),
         dist = replace(dist, office_sought == "federal:senate", NA)) %>% 
  select(cycle, fec, office_sought, party, st, dist, name, namelast, everything())



saveRDS(dime, "data/output/03_contextual/dime_fmt.Rds")

