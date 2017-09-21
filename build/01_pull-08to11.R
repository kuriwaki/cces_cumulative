
# pull out some variables of interest pre-aggregation into 2008-2012 cumulative

library(dplyr)
library(haven)


cc06 <- read_dta("data/source/cces/2006_cc.dta")
cc07 <- read_dta("data/source/cces/2007_cc.dta")
cc08 <- read_dta("data/source/cces/2008_cc.dta")
cc09 <- read_dta("data/source/cces/2009_cc.dta")
cc10 <- read_dta("data/source/cces/2010_cc.dta")
cc11 <- read_dta("data/source/cces/2011_cc.dta")

# variable list ----
varList <- function(df) {
  tibble(label = as.character(sapply(df, function(x) attr(x, "label"))),
         var = colnames(df))
}
v06 <- varList(cc06)
v08 <- varList(cc08)
v10 <- varList(cc10)


# validated vote ----


arrange(v10, desc(var))

vv10 <- cc10 %>% 
  rename(vv_regstatus = voter_status,
         vv_pmv = vote_pri10,
         vv_gmv = vote_gen10,
         caseID = V100) %>%
  mutate(year = 2010) %>%
  select(caseID, year, matches("^vv"))

vv08 <- cc08 %>% 
  rename(vv_gmv = vote_gen08,
         vv_pmv = vote_pri08,
         vv_regstatus = voter_status,
         caseID = V100) %>%
  mutate(year = 2008) %>% 
  select(caseID, year, matches("^vv_"))


vv06 <- cc06 %>% 
  rename(vv_gmv = vote_gen06,
         vv_st = matchState,
         vv_regstatus = matched,
         caseID = v1000) %>%
  mutate(year = 2006) %>% 
  mutate(vv_regstatus = as.character(as_factor(vv_regstatus))) %>%
  select(caseID, year, matches("^vv_"))


bind_rows(vv06, vv08, vv10)
