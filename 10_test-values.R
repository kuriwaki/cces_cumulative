ccc <- readRDS("data/release/cumulative_2006_2019.rds")
ccs <- read_rds("data/temp_cc-name-cleaned-list.rds")

ccc %>% 
  mutate(across(is.labelled, as_factor)) %>% 
  xtabs( ~ year + voted_gov_party, .) %>% 
  prop.table(1) %>% round(3)

ccs[["2006"]] %>% 
  count(milstat)


lookfor(cc06, "child")
lookfor(cc07, "parent")
count(cc06, v2129)

