library(tidyverse)


load("data/output/01_responses/common_all.RData")
feckey <- readRDS("data/output/03_contextual/fec_fmt.Rds")
inc_H <- readRDS("data/output/03_contextual/incumbents_H.Rds")
inc_S <- readRDS("data/output/03_contextual/incumbents_S.Rds")
statecode <- read_csv("data/source/statecode.csv")

fmt_geo <- function(tbl) {
  if (class(tbl$state) == "character") {
    fmt <- tbl %>% 
      mutate(cdid = as.integer(cdid),
             dist = as.integer(cdid))
  }
  
  if (class(tbl$state) == "labelled") {
    fmt <- tbl %>%
      mutate(state = as.character(as_factor(state)),
             cdid = as.integer(cdid),
             dist = as.integer(cdid))
  }
  
  left_join(fmt, select(statecode, state, st), by = "state") %>%
    mutate_at(vars(matches("_can")), function(x) replace(x, x == "__NA__" | x == "", NA)) %>% # make NA if empty or "_NA_"
    select(year, st, dist, everything())
}


sel_common <- c("year", "cong_inc", "cong_up", "caseID")

# do something like 
master <- 
  tribble(~name,      ~`2006`, ~`2007`,     ~`2008`, ~`2009`, ~`2010`, ~`2011`, ~`2012`,            ~`2013`,            ~`2014`,            ~`2015`,            ~`2016`,
          "state",    "v1002", "inputstate", "V206",  "v259",  "V206",  "V206", "inputstate",       "inputstate",       "inputstate",       "inputstate",       "inputstate",
          "cdid",     "v1003", "cdidnum",    "V250",  "v264",  "V276",  "V276", "cdid",             "cdid113",          "cdid",             "cdid113",          "cdid113",
          "gov_inc",  "v5019", "govname",    "V508",  "v608",  "V529",  "V529", "CurrentGovName",   "CurrentGovName",   "CurrentGovName",   "CurrentGovName",   "CurrentGovName",
          "gov_can1", "v5009", NA,           "V501",  NA,      "V564",  NA,     "GovCand1Name",     NA,                 "GovCand1Name",     NA,                 "GovCand1Name",
          "gov_can2", "v5011", NA,           "V502",  NA,      "V567",  NA,     "GovCand2Name",     NA,                 "GovCand2Name",     NA,                 "GovCand2Name",
          "hou_inc",  "v5013", "repname",    "V527",  "v627",  "V501",  "V521", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName",
          "hou_can1", "v5001", NA,           "V518",  "v618",  "V533",  "V533", "HouseCand1Name",   NA,                 "HouseCand1Name",   NA,                 "HouseCand1Name",
          "hou_can2", "v5003", NA,           "V519",  "v619",  "V536",  "V536", "HouseCand2Name",   NA,                 "HouseCand2Name",   NA,                 "HouseCand2Name",
          "sen1_inc", "v5015", "sen1name",   "V551",  "v651",  "V513",  "V501", "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",
          "sen2_inc", "v5017", "sen2name",   "V552",  "v652",  "V521",  "V513", "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",
          "sen_can1", "v5005", NA,           "V553",  "v653",  "V548",  NA,     "SenCand1Name",     NA,                 "SenCand1Name",     NA,                 "SenCand1Name",
          "sen_can2", "v5007", NA,           "V555",  "v655",  "V551",  NA,     "SenCand2Name",     NA,                 "SenCand2Name",     NA,                 "SenCand2Name"
          )





# 2006
ctx06 <- 
  cc06 %>% 
  mutate(cong_inc = 109L,
         cong_up = 110L) %>%
  select(!!sel_common,
         ) %>% 
  left_join(select = statecode, state, st), by = "st") %>% 
  select(-st) %>%
  fmt_geo()


# 2007
ctx07 <-
  cc07 %>% 
  mutate(cong_inc = 110L,
         cong_up = 111L) %>%
  select(!!sel_common) %>% 
  fmt_geo()


# 2008
ctx08 <-  
  cc08 %>% 
  mutate(cong_inc = 110L,
         cong_up = 111L) %>%
  select(!!sel_common, 
         reszip = V202, 
         regzip = V204,) %>% 
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
  mutate(state = labelled(zap_labels(state), newlabs)) %>%
  fmt_geo() 
  



# 2010
ctx10 <- 
  cc10 %>% 
  mutate(cong_inc = 111L,
         cong_up = 112L) %>%
  select(!!sel_common,
         reszip = V202, 
         regzip = V205) %>% 
  fmt_geo()

# 2011
ctx11 <- 
  cc11 %>% 
  mutate(cong_inc = 112L,
         cong_up = 113L) %>% 
  select(!!sel_common,
         hou_can1 = , # 2010 vote, D/R

# 2012
ctx12 <- 
  cc12 %>% 
  mutate(cong_inc = 112L,
         cong_up = 113L) %>% 
  select(!!sel_common,
         cdid_up = cdid113,
  fmt_geo()

# 2013 
ctx13 <- 
  cc13 %>% 
  mutate(cong_inc = 113L,
         cong_up = 114L) %>% 
  select(!!sel_common,
         state = inputstate,
         cdid = cdid113,
         gov_inc = CurrentGovName) # NJ and VA Gov
  fmt_geo()


# 2014
ctx14 <- 
  cc14 %>% 
  mutate(cong_inc = 113L,
         cong_up = 114L) %>% 
  select(!!sel_common,
  fmt_geo()

# 2015 
ctx15 <- 
  cc15 %>% 
  mutate(cong_inc = 114L,
         cong_up = 115L) %>% 
  select(!!sel_common,
         gov_inc = CurrentGovName, # KY, LA, MS Gov
  fmt_geo()



# 2016 
ctx16 <- 
  cc16 %>% 
  mutate(cong_inc = 114L,
         cong_up = 115L) %>%
  select(!!sel_common,
         cdid_up = cdid115,
         state = "inputstate",
         cdid = "cdid113",
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








# vars
inc_H_mv <- c("congress", "st", "dist", "icpsr", "fec", "namelast")






# create key ----
hou_inc <- left_join(key, 
          select(inc_H, !! inc_H_mv),
          by = c("st", "dist", "cong_inc" = "congress")) %>%
  select(year, caseID, icpsr, fec)
