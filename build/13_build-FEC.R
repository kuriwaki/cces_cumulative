library(tidyverse)


dime_raw <- read_csv("data/source/dime/dime_recipients_all_1979_2014.csv")

# filter only candidates
dime_cand <- dime_raw %>%
  filter(recipient.type == "cand")

dime_cols <- dime_cand %>%
  select(
    elec = fecyear, # target election, as recorded by FFEC
    cycle,
    fec = Cand.ID,
    icpsr = ICPSR2, # ICPSR has the year of donation attached to it
    office_sought = seat,
    party,
    st = state, 
    dist = district,
    incumb = Incum.Chall,
    name,
    namelast = lname,
    namefirst = fname,
    namefirstm = ffname
  ) %>% 
  mutate(
    name = toupper(name),
    namefirst = toupper(namefirst),
    namelast = toupper(namelast),
    namefirstm = toupper(namefirstm)
  )

dime_full <- dime_cols %>%
  filter(!is.na(fec))

df_distinct <- dime_full %>% distinct()
  

# keep ICPSR as ICPSR, drop FEC (in DIME data, candidates who have never been 
# incumbents and thus have no FEC are just given the FEC code).

# for those that remain, change to integer

df_fec <- df_distinct %>% 
  mutate(icpsr = replace(icpsr, icpsr == fec, "")) %>%
  mutate(icpsr_num = as.integer(icpsr)) # a real ICPSR (of incumbents) should have no letters in the first place 




# modify dist
# only keep dist for house and state leg

regex_dist_offices <- "(federal:house|state:lower|state:upper)"

df <-  df_fec %>% 
  mutate(dist = replace(dist, !grepl(regex_dist_offices, office_sought), "")) %>%
  mutate(dist = gsub("[A-Z-]+", "", dist)) %>% # can strip state for these offices
  mutate(dist = as.integer(dist)) %>%
  mutate(st = toupper(st))  # formatting corrections


# recode party 
df <- df %>% 
  mutate(party = dplyr::recode(party, `100` = "D", `200` = "R", `328` = "I", .default = "NA/Other"))



# save
saveRDS(df, "data/output/03_contextual/fec_fmt.Rds")
write_csv(filter(df, elec %in% 2004:2020),  "data/output/03_contextual/fec_fmt.csv")
