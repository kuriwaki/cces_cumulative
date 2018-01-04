library(tidyverse)


load("data/output/01_responses/common_all.RData")
feckey <- readRDS("data/output/03_contextual/fec_fmt.Rds")
inc_H <- readRDS("data/output/03_contextual/incumbents_H.Rds")
inc_S <- readRDS("data/output/03_contextual/incumbents_S.Rds")
statecode <- read_csv("data/source/statecode.csv")

fmt_geo <- function(tbl) {
  tbl %>%
  mutate(state = as.character(as_factor(state)),
         dist = as.integer(cdid)) %>%
  right_join(select(statecode, state, st), by = "state")
}


# 2008
ctx08 <-  
  cc08 %>% 
  mutate(cong_inc = 110L,
         cong_up = 111L) %>%
  select(year, 
         cong_inc,
         cong_up,
         caseID,
         state = V206,
         cdid = V250,
         reszip = V202, 
         regzip = V204,
         gov_inc = V508,
         gov_dem = V501,
         gov_rep = V502,
         hou_inc = V527,
         hou_dem = V518,
         hou_rep = V519,
         sen_inc1 = V551,
         sen_inc2 = V552,
         sen_dem = V553,
         sen_rep = V555) %>% 
  fmt_geo()



# 2010
ctx10 <- 
  cc10 %>% 
  mutate(cong_inc = 111L,
         cong_up = 112L) %>%
  select(year, 
         cong_inc,
         cong_up,
         caseID,
         state = V206,
         cdid = V276,
         reszip = V202, 
         regzip = V205,
         gov_inc = V529,
         gov_dem = V564,
         gov_rep = V567,
         hou_inc = V501,
         hou_dem = V533,
         hou_rep = V536,
         sen_inc1 = V513,
         sen_inc2 = V521,
         sen_dem = V548,
         sen_rep = V551) %>% 
  fmt_geo()


# 2012
ctx12 <- 
  cc12 %>% 
  mutate(cong_inc = 112L,
         cong_up = 113L) %>% 
  select(year, 
         cong_inc,
         cong_up,
         caseID,
         state = inputstate,
         cdid = cdid,
         cdid_up = cdid113,
         gov_inc = CurrentGovName,
         gov_cand1 = GovCand1Name,
         gov_cand2 = GovCand2Name,
         hou_inc = CurrentHouseName,
         hou_cand1 = HouseCand1Name,
         hou_cand2 = HouseCand2Name,
         sen_inc1 = CurrentSen1Name,
         sen_inc2 = CurrentSen2Name,
         sen_cand1 = SenCand1Name,
         sen_cand2 = SenCand2Name) %>% 
  fmt_geo()




# vars
inc_H_mv <- c("congress", "st", "dist", "icpsr", "fec", "namelast")



# create key ----
hou_inc <- key %>% 
  left_join(ctx08, 
          select(inc_H, !! inc_H_mv),
          by = c("st", "dist", "cong_inc" = "congress")) %>%
  select(year, caseID, icpsr, fec)
