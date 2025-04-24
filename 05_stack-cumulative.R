library(tidyverse)
library(labelled)
library(haven)
suppressPackageStartupMessages(library(foreach))
library(glue)
library(lubridate)
library(cli)
library(arrow)

stopifnot(packageVersion("labelled") >= "2.4.0")


# functions -----
source("05_functions-stack.R")
cli_alert_success("Finished reading in functions")

# Read data ------
if (!exists("cc22") & !exists("cc18") & !exists("cc06")) {
  load("data/output/01_responses/common_all.RData")
}
cc06_time <- readRDS("data/output/01_responses/cc06_datetime.Rds")
cc09_time <- readRDS("data/output/01_responses/cc09_datetime.Rds")
cc10_pid3 <- readRDS("data/output/01_responses/cc10_pid3.Rds")
cc09_econ <- readRDS("data/output/01_responses/cc09_econ_retro.Rds")
cc17_county <- read_csv("data/source/cces/CCES17_Common_county.csv", show_col_types = FALSE) |>
  transmute(year = 2017, case_id = V101, countyfips)

# execute name standardization
# in list form
ccs <- list(
  "pettigrew" = std_name(filter(ccp, year != 2012)),
  # "2006mit" = std_name(mit06_add),
  # "2008hu" = std_name(hu08),
  "2009hu" = std_name(hu09),
  "2012" = std_name(cc12),
  "2012panel" = std_name(panel12, is_panel = TRUE),
  "2013" = std_name(cc13),
  "2014" = std_name(cc14),
  "2015" = std_name(cc15),
  "2016" = std_name(cc16),
  "2017" = std_name(cc17),
  "2018" = std_name(cc18),
  "2018comp" = std_name(mutate(cc18_cnew,
                               commonweight = NA,
                               commonpostweight = NA,
                               vvweight = NA,
                               vvweight_post = NA)),
  "2019" = std_name(cc19),
  "2020" = std_name(cc20),
  "2021" = std_name(cc21),
  "2022" = std_name(cc22),
  "2023" = std_name(cc23),
  "2024" = std_name(cc24)
)

cli_alert_success("Finished reading in data and standardizing names")



# Extract variable by variable  -----
cli_h1("Joining admin")

## admin ------
wgt        <- find_stack(ccs, weight, "numeric")
wgt_post   <- find_stack(ccs, weight_post, "numeric")

vwgt        <- find_stack(ccs, rvweight, "numeric")
vwgt_post <- find_stack(ccs, rvweight_post, "numeric")

tookpost <- find_stack(ccs, tookpost, make_labelled =  FALSE, new_reorder = FALSE) %>% 
  mutate(tookpost = labelled(
    as.integer(tookpost_num == 1 & year < 2018 | 
                 tookpost_num == 2 & year %in% c(2018, 2020, 2022, 2024)), # diff number in 2018
    labels = c("Took Post-Election Survey" = 1,
               "Did Not Take Post-Election Survey" = 0))) %>% 
  mutate(tookpost  = replace(tookpost, year %% 2 == 1, NA)) %>% 
  select(year, case_id, tookpost)

time <- find_stack(ccs, starttime, type = "datetime") %>%
  filter(year != 2006, year != 2009) %>% 
  bind_rows(cc06_time, cc09_time)

## pid -------
cli_h1("Joining partisanship and demographics")
pid3_labels <- c("Democrat" = 1,  "Republican" = 2, "Independent" = 3,
                 "Other" = 4, "Not Sure" = 5)

pid3 <- find_stack(ccs, pid3, make_labelled = FALSE, new_reorder = FALSE) %>%
  filter(year != 2010) %>% # fix the missing 2010
  bind_rows(cc10_pid3) %>%
  mutate(pid3_num = na_if(pid3_num, 8)) %>%
  mutate(pid3_num = na_if(pid3_num, 9)) %>%
  mutate(pid3 = labelled(as.integer(pid3_num), pid3_labels)) %>%
  select(year, case_id, pid3) # manually do only this one

pid7 <- find_stack(ccs, pid7, make_labelled = TRUE)

# put leaners into partisans
leaner_lbl_code <- c(`Democrat (Including Leaners)` = 1L,
                     `Republican (Including Leaners)` = 2L,
                     `Independent (Excluding Leaners)` = 3L,
                     `Not Sure` = 8L)
pid3_leaner <- pid7 %>%
  mutate(pid3_leaner = as_factor(pid7)) %>% 
  mutate(pid3_leaner = fct_collapse(pid3_leaner, 
                                    "Republican (Including Leaners)" = c("Strong Republican", "Not Very Strong Republican", "Lean Republican"),
                                    "Democrat (Including Leaners)" = c("Strong Democrat", "Not Very Strong Democrat", "Lean Democrat"),
                                    "Independent (Excluding Leaners)" = "Independent")) %>% 
  mutate(pid3_leaner_num = recode(pid3_leaner, !!!leaner_lbl_code)) %>% 
  mutate(pid3_leaner = labelled(pid3_leaner_num, leaner_lbl_code)) %>% 
  select(-pid3_leaner_num, -pid7)

ideo5 <- find_stack(ccs, ideo5)

## demographics ----

sex <- find_stack(ccs, sex, make_labelled = TRUE)
gend <- find_stack(ccs, gender, make_labelled = TRUE)
gend4 <- find_stack(ccs, gender4, make_labelled = TRUE)

educ <- find_stack(ccs, educ, make_labelled = TRUE)

race <- find_stack(ccs, race, make_labelled = TRUE)
hisp <- find_stack(ccs, hispanic, make_labelled = TRUE)
race_key <- ccesMRPprep::race_key %>% distinct(.data$race_cces_chr, .data$race)
race_anyh <- left_join(race, hisp, by = c("year", "case_id")) |> 
  mutate(race_chr = as.character(as_factor(race)),
         race_h = replace(race, race_chr != "Hispanic" & hispanic == 1, race_key$race[3])) |> 
  select(year, case_id, race_h)
hisp_origin <- find_stack(ccs, hisp_origin, make_labelled = FALSE)

bryr <- find_stack(ccs, birthyr, "integer")
age <- find_stack(ccs, age, "integer")

## income wrangling -----
cli_h1("Joining income and employmnet")
inc_old <- find_stack(ccs, family_income_old, "integer", make_labelled = FALSE) %>%
  mutate(faminc = recode(
    family_income_old,
    `1` = "Less than 10k",
    `2` = "10k - 20k",
    `3` = "10k - 20k",
    `4` = "20k - 30k",
    `5` = "20k - 30k",
    `6` = "30k - 40k",
    `7` = "40k - 50k",
    `8` = "50k - 60k",
    `9` = "60k - 70k",
    `10` = "70k - 80k",
    `11` = "80k - 100k",
    `12` = "100k - 120k",
    `13` = "120k - 150k",
    `14` = "150k+",
    `15` = "Prefer not to say"))

inc_new <- find_stack(ccs, family_income, "integer", make_labelled = FALSE) %>%
  mutate(faminc = recode(
    family_income,
    `1` = "Less than 10k",
    `2` = "10k - 20k",
    `3` = "20k - 30k",
    `4` = "30k - 40k",
    `5` = "40k - 50k",
    `6` = "50k - 60k",
    `7` = "60k - 70k",
    `8` = "70k - 80k",
    `9` = "80k - 100k",
    `10` = "100k - 120k",
    `11` = "120k - 150k",
    `12` = "150k+",
    `13` = "150k+",
    `14` = "150k+",
    `15` = "150k+",
    `16` = "150k+",
    `31` = "150k+",
    `32` = "150k+",
    `97` = "Prefer not to say",
    `98` = "Skipped",
    `99` = "Not Asked"))

faminc <- inner_join(inc_old, inc_new, by = c("year", "case_id")) %>% 
  mutate(faminc_char = coalesce(faminc.x, faminc.y),
         faminc_num = coalesce(family_income_old, family_income)) %>% 
  transmute(year, case_id, faminc = fct_reorder(faminc_char, faminc_num, .na_rm = FALSE))

## union, employment, health ----
union <- find_stack(ccs, union, make_labelled = TRUE) %>% 
  mutate(union = labelled(zap_label(union), 
                          c("Yes, Currently" = 1,
                            "Yes, Formerly" = 2, 
                            "No, Never" = 3)),
         union = na_if(union, 8))

union_hh <- find_stack(ccs, unionhh, make_labelled = FALSE) %>% 
  mutate(union_hh = fct_collapse(unionhh,
                                 `1` = c(
                                   "Current Member in Household",
                                   "Yes, a Member of My Household Is Currently a Union Member"),
                                 `2` = c(
                                   "A Member of My Household Was Formerly a Member of a Labor Union, But Is not Now",
                                   "Former Member in Household"),
                                 `3` = c(
                                   "No Union Members in Household",
                                   "No, No One in My Household Has Ever Been a Member of a Labor Union"),
                                 `4` = c("Not Sure")
  )) %>% 
  mutate(union_hh = labelled(as.integer(union_hh), 
                             c("Yes, Currently" = 1, 
                               "Yes, Formerly" = 2,
                               "No, Never" = 3, 
                               "Not Sure" = 4))) %>% 
  select(-unionhh)


employ <- find_stack(ccs, employ)
ownhome <- find_stack(ccs, ownhome)

child18 <- find_stack(ccs, child18) %>% 
  rename(has_child = child18)
milstat <- find_stack(ccs, milstat_5) %>% 
  rename(no_milstat = milstat_5) %>% 
  mutate(no_milstat = recode_factor(no_milstat,
                                    Yes = "Yes",
                                    Selected = "Yes",
                                    No = "No", 
                                    `Not Selected` = "No"))

hi_most <- find_stack(ccs, healthins_6) %>% 
  filter(year != "2018") %>% 
  rename(no_healthins = healthins_6)
hi_18 <- find_stack(ccs[c("2018", "2018comp")], healthins_7) %>% 
  rename(no_healthins = healthins_7)
healthins <- bind_rows(hi_most, hi_18) %>% 
  mutate(no_healthins = recode_factor(no_healthins,
                                      Yes = "Yes",
                                      Selected = "Yes",
                                      No = "No", 
                                      `Not Selected` = "No"))

## marriage status 
marstat <- find_stack(ccs, marstat, make_labelled = TRUE) %>% 
  remove_value_labels(marstat = 8) %>% 
  mutate(marstat = na_if(marstat, 8)) %>% 
  labelled::add_value_labels(marstat = c(`Single / Never Married` = 5))

# citizen - define by immstat
citizen <- find_stack(ccs, immstat) %>% 
  mutate(citizen = str_detect(immstat, regex("(Non-Citizen|Not A Citizen)", ignore_case = TRUE))) %>% 
  mutate(citizen = labelled(citizen + 1, labels = c(`Citizen` = 1, `Non-Citizen` = 2))) %>% 
  select(-immstat)

## religion -----
relig <- find_stack(ccs, religpew, make_labelled = TRUE) %>% 
  rename(religion = religpew)
religimp <- find_stack(ccs, pew_religimp, type = "factor") %>% 
  rename(relig_imp = pew_religimp)
bornagain <- find_stack(ccs, pew_bornagain, make_labelled = TRUE) %>%
  rename(relig_bornagain = pew_bornagain)
protestant <- find_stack(ccs, religpew_protestant, make_labelled = TRUE) |> 
  rename(relig_protestant = religpew_protestant)
churatd <- find_stack(ccs, pew_churatd, make_labelled = TRUE) |> 
  rename(relig_church = pew_churatd)

## pres, house, sen, gov -------
cli_h1("Joining vote choice")
i_pres08 <- find_stack(ccs, intent_pres_08)
i_pres12 <- find_stack(ccs, intent_pres_12)
i_pres16 <- find_stack(ccs, intent_pres_16)
i_pres20 <- find_stack(ccs, intent_pres_20)
i_pres24 <- find_stack(ccs, intent_pres_24)

v_pres08 <- find_stack(ccs, voted_pres_08)
v_pres12 <- find_stack(ccs, voted_pres_12)
v_pres16 <- find_stack(ccs, voted_pres_16)
v_pres20 <- find_stack(ccs, voted_pres_20)
v_pres24 <- find_stack(ccs, voted_pres_24)

# v_pres08
v_pres08_08_11 <- list(std_name(cc08), std_name(cc09), std_name(cc10), std_name(cc11)) |> 
  find_stack(voted_pres_08)

# quick consolidations for multiple years (Asked in the past)
v_pres08 <- v_pres08 |> 
  mutate(voted_pres_08 = replace(voted_pres_08, year < 2008, NA)) |> 
  left_join(v_pres08_08_11, by = c("year", "case_id"), suffix = c("", "_alt")) |> 
  mutate(voted_pres_08 = clps_pres08(voted_pres_08),
         voted_pres_08 = replace(voted_pres_08, is.na(voted_pres_08_alt) & year %in% 2008:2011,  NA),
         voted_pres_08 = replace(voted_pres_08, year %in% 2008:2011 & voted_pres_08 == "Did not Vote for this Office", NA)) |> 
  select(-voted_pres_08_alt)

v_pres12 <- v_pres12 %>% 
  mutate(voted_pres_12 = clps_pres12(voted_pres_12))
v_pres16 <- v_pres16 %>%
  mutate(voted_pres_16 = clps_pres16(voted_pres_16),
         voted_pres_16 = replace(voted_pres_16, year %in% 2019:2021 & voted_pres_16 == "Did not Vote for this Office", NA))
v_pres20 <- v_pres20 %>%
  mutate(voted_pres_20 = clps_pres20(voted_pres_20),
         voted_pres_20 = replace(voted_pres_20, voted_pres_20 == "Did not Vote for this Office", NA))
v_pres24 <- v_pres24 %>%
  mutate(voted_pres_24 = clps_pres24(voted_pres_24),
         voted_pres_24 = replace(voted_pres_24, voted_pres_24 == "Did not Vote for this Office", NA))

# coalesce
pres_party <- i_pres08 %>% 
  left_join(i_pres12, by = c("year", "case_id")) %>% 
  left_join(i_pres16, by = c("year", "case_id")) %>% 
  left_join(i_pres20, by = c("year", "case_id")) %>% 
  left_join(v_pres08, by = c("year", "case_id")) %>% 
  left_join(v_pres12, by = c("year", "case_id")) %>% 
  left_join(v_pres16, by = c("year", "case_id")) %>% 
  left_join(v_pres20, by = c("year", "case_id")) %>% 
  left_join(v_pres24, by = c("year", "case_id")) %>% 
  mutate_if(is.factor, as.character) %>% 
  # NA for previous election
  mutate(voted_pres_08 = replace(voted_pres_08, year == 2012, NA),
         voted_pres_12 = replace(voted_pres_12, year == 2016, NA),
         voted_pres_16 = replace(voted_pres_16, year == 2020, NA)) %>%  
  transmute(
    year, case_id,
    intent_pres_party = pres_names(
      coalesce(intent_pres_24, intent_pres_20, intent_pres_16, intent_pres_12, intent_pres_08)),
    voted_pres_party  = pres_names(
      coalesce(voted_pres_24, voted_pres_20, voted_pres_16, voted_pres_12, voted_pres_08))
  )

i_rep <- find_stack(ccs, intent_rep, new_reorder = FALSE)
i_sen <- find_stack(ccs, intent_sen, new_reorder = FALSE)
i_gov <- find_stack(ccs, intent_gov, new_reorder = FALSE)
v_rep <- find_stack(ccs, voted_rep, new_reorder = FALSE)
v_sen <- find_stack(ccs, voted_sen, new_reorder = FALSE)
v_gov <- find_stack(ccs, voted_gov, new_reorder = FALSE)


## approval -----
cli_h1("Joining opinion")
apvpres <- find_stack(ccs, approval_pres, make_labelled = TRUE)
apvrep  <- find_stack(ccs, approval_rep, make_labelled = FALSE)
apvsen1 <- find_stack(ccs, approval_sen1, make_labelled = FALSE)
apvsen2 <- find_stack(ccs, approval_sen2, make_labelled = FALSE)
apvgov  <- find_stack(ccs, approval_gov, make_labelled = TRUE) 

## economy -----
econ_char <- find_stack(ccs, economy_retro, make_labelled = FALSE, new_reorder = FALSE) %>% 
  mutate(economy_retro_char = recode(economy_retro_char,
                                     `Gotten Worse`           = "Gotten Worse / Somewhat Worse", 
                                     `Gotten Somewhat Worse`  = "Gotten Worse / Somewhat Worse", 
                                     `Gotten Better`          = "Gotten Better / Somewhat Better",
                                     `Gotten Somewhat Better` = "Gotten Better / Somewhat Better")) %>% 
  filter(year != 2009) %>% 
  bind_rows(cc09_econ) %>% 
  mutate(economy_retro_char = replace(economy_retro_char, economy_retro_num == 8, NA),
         economy_retro_num  = na_if(economy_retro_num, 8),
         economy_retro_char = str_to_lower(economy_retro_char),
         economy_retro_char = str_replace(economy_retro_char, "^g", "G"),
         economy_retro_char = str_replace(economy_retro_char, "^s", "S"),
         economy_retro_char = str_replace(economy_retro_char, "^n", "N"),
  )

# correct to labelled
econ_key <- deframe(distinct(select(econ_char, economy_retro_char, economy_retro_num)))
econ <-  econ_char %>% 
  mutate(economy_retro = labelled(economy_retro_num, labels = econ_key)) %>% 
  select(year, case_id, economy_retro)


## news interest 
newsint <- find_stack(ccs, newsint, make_labelled = TRUE) %>% 
  remove_value_labels(newsint = 8) %>% 
  mutate(newsint = na_if(newsint, 8))




## turnout ----
cli_h1("Joining turnout")
intent_trn <- find_stack(ccs, intent_trn, type = "factor") %>% 
  mutate(intent_turnout_self = recode(
    intent_trn, 
    `Yes, Definitely` = "Yes, definitely",
    `I Already Voted (Early or Absentee)` = "I already voted (early or absentee)",
    `I Plan to Vote Before November 3rd` = "Plan to vote early",
    `I Plan to Vote Before November 4th` = "Plan to vote early",
    `I Plan to Vote Before November 6th` = "Plan to vote early",
    `I Plan to Vote Before November 8th` = "Plan to vote early"))

voted_trn <- find_stack(ccs, voted_trn, type = "factor")  %>% 
  mutate(voted_turnout_self = case_when(
    str_detect(voted_trn, regex("Definitely Voted", ignore_case = TRUE)) ~ "Yes",
    str_detect(voted_trn, regex("yes", ignore_case = TRUE)) ~ "Yes",
    str_detect(voted_trn, regex("no", ignore_case = TRUE)) ~ "No",
    str_detect(voted_trn, regex("not sure", ignore_case = TRUE)) ~ "No",
    str_detect(voted_trn, regex("Did Not Vote", ignore_case = TRUE)) ~ "No",
    str_detect(voted_trn, regex("didn't Vote", ignore_case = TRUE)) ~ "No",
    str_detect(voted_trn, regex("But Didn't", ignore_case = TRUE)) ~ "No",
    str_detect(voted_trn, regex("But couldn't", ignore_case = TRUE)) ~ "No",
    str_detect(voted_trn, regex("But Did Not or Could Not", ignore_case = TRUE)) ~ "No",
    TRUE ~ NA_character_)
  ) %>% 
  mutate(voted_turnout_self = fct_relevel(voted_turnout_self, "Yes", "No"))

# checks before deleting
count(intent_trn, intent_turnout_self, intent_trn)
count(voted_trn, voted_turnout_self, voted_trn)
voted_trn$voted_trn <- NULL
intent_trn$intent_trn <- NULL

# validated vote turnout 
vv_regstatus   <- find_stack(ccs, vv_regstatus, new_reorder = FALSE) # will reorder by frequency later
vv_party_gen   <- find_stack(ccs, vv_party_gen, new_reorder = FALSE)
vv_party_prm   <- find_stack(ccs, vv_party_prm, new_reorder = FALSE)
vv_turnout_gvm <- find_stack(ccs, vv_turnout_gvm, new_reorder = FALSE)
vv_turnout_pvm <- find_stack(ccs, vv_turnout_pvm, new_reorder = FALSE)

# geography ----
cong       <- find_stack(ccs, cong, "integer")
cong_up    <- find_stack(ccs, cong_up, "integer")

cli_h1("Joining geography")
state      <- find_stack(ccs, state, "character")
state_post      <- find_stack(ccs, state_post, "character")
st      <- find_stack(ccs, st, "character")
st_post      <- find_stack(ccs, st_post, "character")

zipcode    <- find_stack(ccs, zipcode, "character") %>% 
  mutate(zipcode = str_pad(zipcode, width = 5, pad = "0"))

county_fips <- find_stack(ccs, county_fips, "numeric") %>% 
  left_join(cc17_county, by = c("year", "case_id")) %>% 
  mutate(county_fips = coalesce(county_fips, as.numeric(countyfips))) %>% 
  select(-countyfips) %>% 
  filter(year != 2007) %>% 
  bind_rows(select(cc07, year, case_id, county_fips = CC06_V1004) %>% 
              mutate_all(zap_labels))

dist       <- find_stack(ccs, dist, "integer")
dist_up    <- find_stack(ccs, dist_up, "integer")
cd         <- find_stack(ccs, cd, "character")
cd_up      <- find_stack(ccs, cd_up, "character")

dist_post     <- find_stack(ccs, dist_post, "integer")
dist_up_post  <- find_stack(ccs, dist_up_post, "integer")
cd_post       <- find_stack(ccs, cd_post, "character")
cd_up_post    <- find_stack(ccs, cd_up_post, "character")

cli_alert_success("Finished joining each variable. Now combining them")

## format state and CD, then zipcode and county ----
stcd <- left_join(state, st) %>%
  left_join(cong) %>%
  left_join(cong_up) %>%
  left_join(state_post) %>%
  left_join(st_post) %>%
  left_join(dist) %>%
  left_join(dist_up) %>%
  left_join(cd) %>%
  left_join(cd_up) %>%
  left_join(dist_post) %>%
  left_join(dist_up_post) %>%
  left_join(cd_post) %>%
  left_join(cd_up_post)

geo <- stcd %>%
  left_join(zipcode) %>%
  left_join(county_fips)

# Join all vars ----
ccc <- geo %>%
  left_join(tookpost) %>%
  left_join(wgt) %>%
  left_join(wgt_post) %>%
  left_join(vwgt) %>%
  left_join(vwgt_post) %>%
  left_join(time) %>%
  left_join(pid3) %>%
  left_join(pid3_leaner) %>%
  left_join(pid7) %>%
  left_join(ideo5) %>%
  left_join(gend) %>%
  left_join(sex) %>%
  left_join(gend4) %>%
  left_join(bryr) %>%
  left_join(age) %>%
  left_join(race) %>%
  left_join(hisp) %>%
  left_join(race_anyh) %>%
  left_join(hisp_origin) %>%
  left_join(citizen) %>%
  left_join(educ) %>%
  left_join(marstat) %>%
  left_join(faminc) %>%
  left_join(union) %>%
  left_join(union_hh) %>%
  left_join(employ) %>%
  left_join(healthins) %>%
  left_join(child18) %>%
  left_join(ownhome) %>%
  left_join(milstat) %>%
  left_join(relig) %>%
  left_join(religimp) %>%
  left_join(bornagain) %>%
  left_join(protestant) %>%
  left_join(churatd) %>%
  left_join(econ) %>%
  left_join(newsint) %>%
  left_join(apvpres) %>%
  left_join(apvrep) %>%
  left_join(apvsen1) %>%
  left_join(apvsen2) %>%
  left_join(apvgov) %>%
  left_join(i_pres08) %>%
  left_join(i_pres12) %>%
  left_join(i_pres16) %>%
  left_join(i_pres20) %>%
  left_join(v_pres08) %>%
  left_join(v_pres12) %>%
  left_join(v_pres16) %>%
  left_join(v_pres20) %>%
  left_join(pres_party) %>%
  left_join(intent_trn) %>%
  left_join(voted_trn) %>%
  left_join(vv_regstatus) %>%
  left_join(vv_party_gen) %>%
  left_join(vv_party_prm) %>%
  left_join(vv_turnout_gvm) %>%
  left_join(vv_turnout_pvm)


stopifnot(nrow(ccc) == nrow(pid3))

# Checks ---
# check no accidental duplicate id's within 2012 or 2009
foo_09 <- wgt %>% filter(year == 2009)
stopifnot(nrow(foo_09) == nrow(distinct(foo_09, year, case_id)))

foo_12 <- wgt %>% filter(year == 2012)
stopifnot(nrow(foo_12) == nrow(distinct(foo_12, year, case_id)))


# don't use panel rows for now
panel_id <- ccs[["2012panel"]] %>% select(year, case_id) %>% mutate(case_id = as.integer(case_id))
# mit06_id <- ccs[["2006mit"]] %>% select(year, case_id) %>% mutate(case_id = as.integer(case_id))
# hu08_id <- ccs[["2008hu"]] %>% select(year, case_id) %>% mutate(case_id = as.integer(case_id))
hu09_id <- ccs[["2009hu"]] %>% select(year, case_id) %>% mutate(case_id = as.integer(case_id))
comp_id <- ccs[["2018comp"]] %>% select(year, case_id) %>% mutate(case_id = as.integer(case_id))
addon_id <- bind_rows(hu09_id, panel_id, comp_id) # hu08_id, 


# Common manipulations ----
# Weight --
size_year <- ccc %>%
  anti_join(addon_id, by = c("year", "case_id")) %>% # don't count panel to get weights
  group_by(year) %>%
  summarize(size = n()) %>%
  mutate(size_factor = size / median(size)) # manageable constant -- divide by median

ccc_sort <- ccc %>%
  left_join(select(size_year, year, size_factor)) %>%
  mutate(weight_cumulative = weight / size_factor) %>%
  select(-size_factor) %>%
  relocate(year, case_id, weight, weight_cumulative)


# Write ----- 
cli_alert_success("Finished combining, now saving")
# write_rds(ccs, "data/temp_cc-name-cleaned-list.rds")
save(i_rep, i_sen, i_gov, v_rep, v_sen, v_gov, file = "data/output/01_responses/vote_responses.RData")
save(vv_party_gen, vv_party_prm, vv_regstatus, vv_turnout_gvm, vv_turnout_pvm, file = "data/output/01_responses/vv_responses.RData")
write_feather(ccc_sort, "data/output/01_responses/cumulative_stacked.feather")
saveRDS(addon_id, "data/output/01_responses/addon_ids.Rds")
write_csv(size_year, "data/output/03_contextual/weight_rescale_by-year.csv")

cli_alert_success("Finished stacking vars for cumulative")
