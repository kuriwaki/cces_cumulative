library(tidyverse)

# Variable Key ---

# each row is a variable for the starndardized data, each column is for the cces year0
master <- 
  tribble(~name,      ~`2006`, ~`2007`,     ~`2008`, ~`2009`, ~`2010`, ~`2011`, ~`2012`,            ~`2013`,            ~`2014`,            ~`2015`,            ~`2016`,
          "gov_inc",  "v5019", "govname",    "V508",  "v608",  "V529",  "V529", "CurrentGovName",   "CurrentGovName",   "CurrentGovName",   "CurrentGovName",   "CurrentGovName",
          "hou_inc",  "v5013", "repname",    "V527",  "v627",  "V501",  "V521", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName", "CurrentHouseName",
          "sen1_inc", "v5015", "sen1name",   "V551",  "v651",  "V513",  "V501", "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",  "CurrentSen1Name",
          "sen2_inc", "v5017", "sen2name",   "V552",  "v652",  "V521",  "V513", "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",  "CurrentSen2Name",
          "gov_can1", "v5009", NA,           "V501",  NA,      "V564",  NA,     "GovCand1Name",     NA,                 "GovCand1Name",     NA,                 "GovCand1Name",
          "gov_pty1", "v5010", NA,            NA,     NA,       NA,     NA,     "GovCand1Party",    NA,                 "GovCand1Party",    NA,                 "GovCand1Party",
          "gov_can2", "v5011", NA,           "V502",  NA,      "V567",  NA,     "GovCand2Name",     NA,                 "GovCand2Name",     NA,                 "GovCand2Name",
          "gov_pty2", "v5012", NA,            NA,     NA,       NA,     NA,     "GovCand2Party",    NA,                 "GovCand2Party",    NA,                 "GovCand2Party",
          "hou_can1", "v5001", NA,           "V518",  "v618",  "V533",  "V533", "HouseCand1Name",   NA,                 "HouseCand1Name",   NA,                 "HouseCand1Name",
          "hou_pty1", "v5002", NA,            NA,     NA,       NA,     NA,     "HouseCand1Party",  NA,                 "HouseCand1Party",  NA,                 "HouseCand1Party",
          "hou_can2", "v5003", NA,           "V519",  "v619",  "V536",  "V536", "HouseCand2Name",   NA,                 "HouseCand2Name",   NA,                 "HouseCand2Name",
          "hou_pty2", "v5004", NA,            NA,     NA,       NA,     NA,     "HouseCand2Party",  NA,                 "HouseCand2Party",  NA,                 "HouseCand2Party",
          "sen_can1", "v5005", NA,           "V553",  "v653",  "V548",  NA,     "SenCand1Name",     NA,                 "SenCand1Name",     NA,                 "SenCand1Name",
          "sen_pty1", "v5006", NA,            NA,     NA,       NA,     NA,     "SenCand1Party",    NA,                 "SenCand1Party",    NA,                 "SenCand1Party",
          "sen_can2", "v5007", NA,           "V555",  "v655",  "V551",  NA,     "SenCand2Name",     NA,                 "SenCand2Name",     NA,                 "SenCand2Name",
          "sen_pty2", "v5008", NA,            NA,     NA,       NA,     NA,     "SenCand2Party",    NA,                 "SenCand2Party",    NA,                 "SenCand2Party"
  )


# 2008, 2009, 2010, 2011 takes D and R so no party column. but note there is an "other party candidate for 2008, 2010

# Data ----
load("data/output/01_responses/common_all.RData")
feckey <- readRDS("data/output/03_contextual/fec_fmt.Rds")
inc_H <- readRDS("data/output/03_contextual/incumbents_H.Rds")
inc_S <- readRDS("data/output/03_contextual/incumbents_S.Rds")
statecode <- read_csv("data/source/statecode.csv")


cclist <- list(`2006` = cc06, 
               `2007` = cc07, 
               `2008` = cc08, 
               `2009` = cc09, 
               `2010` = cc10, 
               `2011` = cc11, 
               `2012` = cc12, 
               `2013` = cc13,
               `2014` = cc14,
               `2015` = cc15, 
               `2016` = cc16)


# Rename variables ----

for (yr in 2006:2016) {
  for (var in master$name) {
    
    # lookup this var
    rename_from <- filter(master, name == var) %>% 
      pull(!!as.character(yr))
    
    # if it shouldn't exist, ensure it doesn't exist
    if (is.na(rename_from)) {
      cclist[[as.character(yr)]] <- cclist[[as.character(yr)]] %>% 
        mutate(!!var := NULL)
    }
    
    # if it should exist, rename
    if (!is.na(rename_from)) {
      cclist[[as.character(yr)]] <- cclist[[as.character(yr)]] %>% 
        rename(!!var := !!rename_from)
    }
  }
}


# bind ------

clean_out <- function(tbl) {
  tbl %>% 
    mutate_at(vars(matches("_can")), function(x) replace(x, x == "__NA__" | x == "", NA)) %>% # make NA if empty or "_NA_" 
    select(!!c("year", "caseID", "state", "st", "cdid", intersect(master$name, colnames(tbl)))) %>% 
    mutate_if(is.labelled, function(x) as.character(as_factor(x)))
}

# bind
dfcc <- map_dfr(cclist, clean_out)

         # hou_can1 = , # 2010 vote, D/R
         # gov_inc = CurrentGovName) # NJ and VA Gov
         # gov_inc = CurrentGovName, # KY, LA, MS Gov


# derive more vars -------
df <- dfcc %>% 
  mutate(cong_inc = as.integer(ceiling((year - 1788)/2)),
         cong_up = cong_inc + 1L)


# add D/R if in 2008, 2010 ----- 

df <- df %>% 
  mutate(gov_pty1 = replace(gov_pty1, year %in% c(2010, 2012), "D"),
         hou_pty1 = replace(hou_pty1, year %in% c(2010, 2012), "D"),
         sen_pty1 = replace(sen_pty1, year %in% c(2010, 2012), "D"),
         gov_pty2 = replace(gov_pty2, year %in% c(2010, 2012), "R"),
         hou_pty2 = replace(hou_pty2, year %in% c(2010, 2012), "R"),
         sen_pty2 = replace(sen_pty2, year %in% c(2010, 2012), "R")
         )

# standardize to D/R -----
std_pty <- function(vec) gsub("Democrat.*", "D", gsub("Repub.*", "R", vec))

df <- df %>%
  mutate_at(vars(matches("_pty")), std_pty)
  


# vars
inc_H_mv <- c("congress", "st", "dist", "icpsr", "fec", "namelast")




# create key ----
hou_inc <- left_join(df, 
          select(inc_H, !! inc_H_mv),
          by = c("st", "cdid" = "dist", "cong_inc" = "congress")) %>%
  select(year, caseID, icpsr, fec)




