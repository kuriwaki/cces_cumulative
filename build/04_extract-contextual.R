library(tidyverse)


load("data/output/01_responses/common_all.RData")
feckey <- readRDS("data/output/03_contextual/fec_fmt.Rds")
inc_H <- readRDS("data/output/03_contextual/incumbents_H.Rds")
inc_S <- readRDS("data/output/03_contextual/incumbents_S.Rds")
statecode <- read_csv("data/source/statecode.csv")

fmt_geo <- function(tbl) {
  tbl %>%
  mutate(state = as.character(as_factor(state)),
         cdid = as.integer(cdid),
         dist = as.integer(cdid)) %>%
  left_join(select(statecode, state, st), by = "state")
}


sel_common <- c("year", "cong_inc", "cong_up", "caseID")

# 2006
ctx06 <- 
  cc06 %>% 
  mutate(cong_inc = 109L,
         cong_up = 110L) %>%
  select(!!sel_common,
         st = v1002,
         cdid = v1003,
         gov_inc = v5019,
         gov_dem = v5009,
         gov_rep = v5011,
         hou_rep = v5013,
         hou_can1 = v5001,
         hou_can2 = v5003,
         sen1_inc = v5015,
         sen2_inc = v5017,
         sen_can1 = v5005,
         sen_can2 = v5007) %>% 
  left_join(select(statecode, state, st), by = "st") %>% 
  select(-st) %>%
  fmt_geo()

# 2007
ctx07 <-
  cc07 %>% 
  mutate(cong_inc = 110L,
         cong_up = 111L) %>%
  select(!!sel_common,
         state = inputstate,
         cdid = cdid_num,
         gov_inc = govname, # verify with Spitzer in NY 
         hou_inc = repname,
         sen_inc1 = sen1name,
         sen_inc2 = sen2name) %>% 
  fmt_geo()


# 2008
ctx08 <-  
  cc08 %>% 
  mutate(cong_inc = 110L,
         cong_up = 111L) %>%
  select(!!sel_common, 
         state = V206,
         cdid = V250,
         reszip = V202, 
         regzip = V204,
         gov_inc = V508,
         gov_can1 = V501,
         gov_can2 = V502,
         hou_inc = V527,
         hou_can1 = V518,
         hou_can2 = V519,
         sen1_inc = V551,
         sen2_inc = V552,
         sen_can1 = V553,
         sen_can2 = V555) %>% 
  fmt_geo()

# 2009
st_labs <- cc09 %>% 
  distinct(v259) %>% 
  mutate(value = zap_labels(v259),
         label = as.character(as_factor(v259)))

newlabs <- st_labs$value
names(newlabs) <- str_to_title(st_labs$label)


ctx09 <-
  cc09 %>% 
  mutate(cong_inc = 111L,
         cong_up = 112L) %>%
  select(!!sel_common,
         state = v259,
         cdid = v264,
         gov_inc = v608,
         hou_inc = v627,
         hou_can1 = v618, # labelled as R/D in 2009
         hou_can2 = v619,
         sen1_inc = v651,
         sen2_inc = v652,
         sen_can1 = v653,
         sen_can2 = v655)  %>% 
  mutate(state = labelled(zap_labels(state), newlabs)) %>%
  fmt_geo() 
  



# 2010
ctx10 <- 
  cc10 %>% 
  mutate(cong_inc = 111L,
         cong_up = 112L) %>%
  select(!!sel_common,
         state = V206,
         cdid = V276,
         reszip = V202, 
         regzip = V205,
         gov_inc = V529,
         gov_can1 = V564,
         gov_can2 = V567,
         hou_inc = V501,
         hou_can1 = V533,
         hou_can2 = V536,
         sen1_inc = V513,
         sen2_inc = V521,
         sen_can1 = V548,
         sen_can2 = V551) %>% 
  fmt_geo()

# 2011
ctx11 <- 
  cc11 %>% 
  mutate(cong_inc = 112L,
         cong_up = 113L) %>% 
  select(!!sel_common,
         state = V206,
         cdid = V276, 
         gov_inc = V529,
         hou_inc = V521,
         hou_can1 = V533, # 2010 vote, D/R
         hou_can2 = V536,
         sen1_inc = V501,
         sen2_inc = V513) %>% 
  fmt_geo()

# 2012
ctx12 <- 
  cc12 %>% 
  mutate(cong_inc = 112L,
         cong_up = 113L) %>% 
  select(!!sel_common,
         state = inputstate,
         cdid = cdid,
         cdid_up = cdid113,
         gov_inc = CurrentGovName,
         gov_can1 = GovCand1Name,
         gov_can2 = GovCand2Name,
         hou_inc = CurrentHouseName,
         hou_can1 = HouseCand1Name,
         hou_can2 = HouseCand2Name,
         sen_inc1 = CurrentSen1Name,
         sen_inc2 = CurrentSen2Name,
         sen_can1 = SenCand1Name,
         sen_can2 = SenCand2Name) %>% 
  fmt_geo()

# 2013 
ctx13 <- 
  cc13 %>% 
  mutate(cong_inc = 113L,
         cong_up = 114L) %>% 
  select(!!sel_common,
         state = inputstate,
         cdid = cdid113,
         gov_inc = CurrentGovName, # NJ and VA Gov
         hou_inc = CurrentHouseName,
         sen_inc1 = CurrentSen1Name,
         sen_inc2 = CurrentSen2Name) %>% 
  fmt_geo()


# 2014
ctx14 <- 
  cc14 %>% 
  mutate(cong_inc = 113L,
         cong_up = 114L) %>% 
  select(!!sel_common,
         state = inputstate,
         cdid,
         gov_inc = CurrentGovName,
         gov_can1 = GovCand1Name,
         gov_can2 = GovCand2Name,
         hou_inc = CurrentHouseName,
         hou_can1 = HouseCand1Name,
         hou_can2 = HouseCand2Name,
         sen_inc1 = CurrentSen1Name,
         sen_inc2 = CurrentSen2Name,
         sen_can1 = SenCand1Name,
         sen_can2 = SenCand2Name) %>% 
  fmt_geo()

# 2015 
ctx15 <- 
  cc15 %>% 
  mutate(cong_inc = 114L,
         cong_up = 115L) %>% 
  select(!!sel_common,
         state = inputstate,
         cdid = cdid,
         gov_inc = CurrentGovName, # KY, LA, MS Gov
         hou_inc = CurrentHouseName,
         sen_inc1 = CurrentSen1Name,
         sen_inc2 = CurrentSen2Name) %>% 
  fmt_geo()



# 2016 
ctx16 <- 
  cc16 %>% 
  mutate(cong_inc = 114L,
         cong_up = 115L) %>%
  select(!!sel_common,
         state = inputstate,
         cdid = cdid113,
         cdid_up = cdid115,
         gov_inc = CurrentGovName,
         gov_can1 = GovCand1Name,
         gov_can2 = GovCand2Name,
         hou_inc = CurrentHouseName,
         hou_can1 = HouseCand1Name,
         hou_can2 = HouseCand2Name,
         sen_inc1 = CurrentSen1Name,
         sen_inc2 = CurrentSen2Name,
         sen_can1 = SenCand1Name,
         sen_can2 = SenCand2Name) %>% 
  fmt_geo()


  
# bind 

key <- bind_rows(
  ctx06,
  ctx07,
  ctx08, 
  ctx09,
  ctx10, 
  ctx11,
  ctx12,
  ctx13,
  ctx14, 
  ctx15,
  ctx16
)


# do something like 

master <- 
  tribble(~name, ~`2006`, ~`2007`, ~`2016`,
          "state", "v1002", "inputstate", "inputstate")





# vars
inc_H_mv <- c("congress", "st", "dist", "icpsr", "fec", "namelast")






# create key ----
hou_inc <- left_join(key, 
          select(inc_H, !! inc_H_mv),
          by = c("st", "dist", "cong_inc" = "congress")) %>%
  select(year, caseID, icpsr, fec)
