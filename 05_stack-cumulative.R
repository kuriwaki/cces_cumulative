library(tidyverse)
library(labelled)
library(haven)
suppressPackageStartupMessages(library(foreach))
library(glue)
library(lubridate)
library(cli)
library(arrow)
library(sjlabelled)
conflicted::conflict_prefer("labelled", "haven")
conflicted::conflict_prefer("as_factor", "haven")
conflicted::conflict_prefer("zap_labels", "haven")
conflicted::conflict_prefer("filter", "dplyr")

stopifnot(packageVersion("labelled") >= "2.4.0")


# functions -----
source("05_functions-stack.R")
cli_alert_success("Finished reading in functions")

left_join2 <- function(x, y) left_join(x, y, by = join_by(year, case_id), relationship = "one-to-one")

# Read data ------
if (!exists("cc24") & !exists("cc18") & !exists("cc06")) {
  load("data/output/01_responses/common_all.RData")
}
cc06_time <- readRDS("data/output/01_responses/cc06_datetime.Rds")
cc09_time <- readRDS("data/output/01_responses/cc09_datetime.Rds")
cc10_pid3 <- readRDS("data/output/01_responses/cc10_pid3.Rds")
cc09_econ <- readRDS("data/output/01_responses/cc09_econ_retro.Rds")
cc07_county <- readRDS("data/output/01_responses/cc07_county.Rds")
cc17_county <- readRDS("data/output/01_responses/cc17_county.Rds")

# Create ccs object -----
# in list form
ccs <- list(
  "pettigrew" = std_name(filter(ccp, year != 2012)),
  # "2006mit" = std_name(mit06_add),
  # "2008hu" = std_name(hu08),
  # "2009hu" = std_name(hu09), # caseid 189 duplicates with pettigrew data; maybe use _recontact
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
  "2024" = std_name(cc24),
  "2025" = std_name(cc25)
)

# free the raw per-year objects (~3 GB); everything downstream uses `ccs`.
# cc08-cc11 are kept until the voted_pres_08 consolidation below.
# NOTE: this defeats the exists() guard above for interactive re-runs -- the
# RData reloads each run.
rm(list = intersect(ls(), c("ccp", "panel12", "cc18_cnew", "hu09",
                            "cc06", "cc07", paste0("cc", 12:25))))
invisible(gc())

cli_alert_success("Finished reading in data and standardizing names")



# Extract variable by variable  -----
cli_h1("Joining admin")

## admin ------
wgt <- find_stack(ccs, weight, "numeric")
wgt_post <- find_stack(ccs, weight_post, "numeric")

vwgt <- find_stack(ccs, vvweight, "numeric")
vwgt_post <- find_stack(ccs, vvweight_post, "numeric")

tookpost_raw <- find_stack(ccs, tookpost, make_labelled = FALSE, new_reorder = FALSE)
tookpost <- finalize_tookpost(tookpost_raw)

time_raw <- find_stack(ccs, starttime, type = "datetime")
time_fixed <- replace_year(time_raw, c(2006, 2009), bind_rows(cc06_time, cc09_time))
time <- time_fixed

## pid -------
cli_h1("Joining partisanship and demographics")
pid3_raw <- find_stack(ccs, pid3, make_labelled = FALSE, new_reorder = FALSE)
pid3_fixed <- replace_year(pid3_raw, 2010, cc10_pid3)
pid3 <- finalize_pid3(pid3_fixed)

pid7 <- find_stack(ccs, pid7, make_labelled = TRUE)

# put leaners into partisans
pid3_leaner <- finalize_pid3_leaner(pid7)

ideo5 <- find_stack(ccs, ideo5)

## demographics ----

sex <- find_stack(ccs, sex, make_labelled = TRUE)
gend <- find_stack(ccs, gender, make_labelled = TRUE)
gend4 <- find_stack(ccs, gender4, make_labelled = TRUE)
sexor <- find_stack(ccs, sexuality)


educ <- find_stack(ccs, educ, make_labelled = TRUE)

race <- find_stack(ccs, race, make_labelled = TRUE)
hisp <- find_stack(ccs, hispanic, make_labelled = TRUE)
race_key <- ccesMRPprep::race_key |> distinct(.data$race_cces_chr, .data$race)
race_anyh <- left_join(race, hisp, by = c("year", "case_id")) |>
  mutate(race_chr = as.character(as_factor(race)),
         race_h = replace(race, race_chr != "Hispanic" & hispanic == 1, race_key$race[3])) |>
  select(year, case_id, race_h)
hisp_origin <- find_stack(ccs, hisp_origin, make_labelled = FALSE)

bryr <- find_stack(ccs, birthyr, "integer")
age <- find_stack(ccs, age, "integer")

## income wrangling -----
cli_h1("Joining income and employment")
inc_old_raw <- find_stack(ccs, family_income_old, "integer", make_labelled = FALSE)
inc_new_raw <- find_stack(ccs, family_income, "integer", make_labelled = FALSE)
faminc <- finalize_faminc(inc_old_raw, inc_new_raw)

## union, employment, health ----
union_raw <- find_stack(ccs, union, make_labelled = TRUE)
union <- finalize_union(union_raw)

union_hh_raw <- find_stack(ccs, unionhh, make_labelled = FALSE)
union_hh <- finalize_union_hh(union_hh_raw)


employ <- find_stack(ccs, employ)
ownhome <- find_stack(ccs, ownhome)
invst <- find_stack(ccs, investor)

child18 <- find_stack(ccs, child18) |>
  rename(has_child = child18)
milstat_raw <- find_stack(ccs, milstat_5)
milstat <- finalize_milstat(milstat_raw)

hi_most_raw <- find_stack(ccs, healthins_6)
hi_18_raw <- find_stack(ccs[c("2018", "2018comp")], healthins_7)
healthins <- finalize_healthins(hi_most_raw, hi_18_raw)

## marriage status
marstat_raw <- find_stack(ccs, marstat, make_labelled = TRUE)
marstat <- finalize_marstat(marstat_raw)

# citizen - define by immstat
citizen_raw <- find_stack(ccs, immstat)
citizen <- finalize_citizen(citizen_raw)


## religion -----
relig <- find_stack(ccs, religpew, make_labelled = TRUE) |>
  rename(religion = religpew)
religimp <- find_stack(ccs, pew_religimp, type = "factor") |>
  rename(relig_imp = pew_religimp)
bornagain <- find_stack(ccs, pew_bornagain, make_labelled = TRUE) |>
  rename(relig_bornagain = pew_bornagain)
protestant <- find_stack(ccs, religpew_protestant, make_labelled = TRUE) |>
  rename(relig_protestant = religpew_protestant)
churatd <- find_stack(ccs, pew_churatd, make_labelled = TRUE) |>
  rename(relig_church = pew_churatd)




## turnout ----
cli_h1("Joining turnout")
reg_self <- find_stack(ccs, reg_self)
intent_trn_raw <- find_stack(ccs, intent_trn, type = "factor")
intent_trn <- finalize_intent_turnout(intent_trn_raw)

voted_trn_raw <- find_stack(ccs, voted_trn, type = "factor")
voted_trn <- finalize_voted_turnout(voted_trn_raw)

## validated vote turnout -----
vv_regstatus <- find_stack(ccs, vv_regstatus, new_reorder = FALSE) # will reorder by frequency later
vv_party_gen <- find_stack(ccs, vv_party_gen, new_reorder = FALSE)
vv_party_prm <- find_stack(ccs, vv_party_prm, new_reorder = FALSE)
vv_turnout_gvm <- find_stack(ccs, vv_turnout_gvm, new_reorder = FALSE)
vv_turnout_pvm <- find_stack(ccs, vv_turnout_pvm, new_reorder = FALSE)
vv_state <- find_stack(ccs, vv_st, new_reorder = FALSE, type = "character") |>
  rename(vv_state = vv_st)


## pres, house, sen, gov -------
cli_h1("Joining vote choice")
i_pres08 <- find_stack(ccs, intent_pres_08)
i_pres12 <- find_stack(ccs, intent_pres_12)
i_pres16 <- find_stack(ccs, intent_pres_16)
i_pres20 <- find_stack(ccs, intent_pres_20)
i_pres24 <- i_pres24_orig <- find_stack(ccs, intent_pres_24)

v_pres08_orig <- find_stack(ccs, voted_pres_08)
v_pres12_orig <- find_stack(ccs, voted_pres_12)
v_pres16_orig <- find_stack(ccs, voted_pres_16)
v_pres20_orig <- find_stack(ccs, voted_pres_20)
v_pres24 <- v_pres24_orig <- find_stack(ccs, voted_pres_24)

# v_pres08
v_pres08_08_11 <- list(std_name(cc08), std_name(cc09), std_name(cc10), std_name(cc11)) |>
  find_stack(voted_pres_08)

# quick consolidations for multiple years (Asked in the past)
v_pres08 <- v_pres08_orig |>
  mutate(voted_pres_08 = replace(voted_pres_08, year < 2008, NA)) |>
  left_join(v_pres08_08_11, by = c("year", "case_id"), suffix = c("", "_alt")) |>
  mutate(voted_pres_08 = clps_pres08(voted_pres_08),
         voted_pres_08 = replace(voted_pres_08, is.na(voted_pres_08_alt) & year %in% 2008:2011, NA),
         voted_pres_08 = replace(voted_pres_08, year %in% 2008:2011 & voted_pres_08 == "Did not Vote for this Office", NA)) |>
  select(-voted_pres_08_alt)

v_pres12 <- v_pres12_orig |>
  mutate(voted_pres_12 = clps_pres12(voted_pres_12))
v_pres16 <- v_pres16_orig |>
  mutate(voted_pres_16 = clps_pres16(voted_pres_16),
         voted_pres_16 = replace(voted_pres_16, year %in% 2019:2021 & voted_pres_16 == "Did not Vote for this Office", NA))
v_pres20 <- v_pres20_orig |>
  mutate(voted_pres_20 = clps_pres20(voted_pres_20),
         voted_pres_20 = replace(voted_pres_20, voted_pres_20 == "Did not Vote for this Office", NA))
v_pres24 <- v_pres24_orig |>
  mutate(voted_pres_24 = clps_pres24(voted_pres_24),
         voted_pres_24 = replace(voted_pres_24, voted_pres_24 == "Did not Vote for this Office", NA))

# coalesce
pres_party <- i_pres08 |>
  left_join2(i_pres12) |>
  left_join2(i_pres16) |>
  left_join2(i_pres20) |>
  left_join2(i_pres24) |>
  left_join2(v_pres08) |>
  left_join2(v_pres12) |>
  left_join2(v_pres16) |>
  left_join2(v_pres20) |>
  left_join2(v_pres24) |>
  mutate_if(is.factor, as.character) |>
  # NA to anticipate later coalesce.
  # We will coalesce (24, 20, 16). In year = 2024, if voted24=NA, then we
  # don't want years 20 and 16 to get in to pres_party. So we zap those out beforehand
  # In year 2023, we only want to use the 2020 data, not 2016. So we need to zap voted16
  # In year 2016-2019, we only want to use 2016 variables, not 2012. So we need to zap votes12
  mutate(voted_pres_08 = replace(voted_pres_08, year %in% c(2012:2016), NA),
         voted_pres_12 = replace(voted_pres_12, year %in% c(2016:2024), NA),
         voted_pres_16 = replace(voted_pres_16, year %in% c(2020:2024), NA),
         voted_pres_20 = replace(voted_pres_20, year == 2024, NA),
         ) |>
  left_join2(voted_trn) |>
  mutate(across(starts_with("voted_pres_"),
                ~ if_else(year %% 4 == 0 & voted_turnout_self == "No", NA, .x))) |>
  transmute(
    year, case_id,
    intent_pres_party = pres_names(
      coalesce(intent_pres_24, intent_pres_20, intent_pres_16, intent_pres_12, intent_pres_08)),
    voted_pres_party = pres_names(
      coalesce(voted_pres_24, voted_pres_20, voted_pres_16, voted_pres_12, voted_pres_08))
  ) |>
  ## NA if not in post
  left_join2(tookpost) |>
  mutate(across(starts_with("voted_pres_"),
                ~ if_else(tookpost == 0 & year %% 2 == 0, NA, .x))) |>
  select(-tookpost)

i_rep <- find_stack(ccs, intent_rep, new_reorder = FALSE)
i_sen <- find_stack(ccs, intent_sen, new_reorder = FALSE)
i_gov <- find_stack(ccs, intent_gov, new_reorder = FALSE)
v_rep <- find_stack(ccs, voted_rep, new_reorder = FALSE)
v_sen <- find_stack(ccs, voted_sen, new_reorder = FALSE)
v_gov <- find_stack(ccs, voted_gov, new_reorder = FALSE)


## approval -----
cli_h1("Joining opinion")
apvpres <- find_stack(ccs, approval_pres, make_labelled = TRUE)
apvrep <- find_stack(ccs, approval_rep, make_labelled = FALSE)
apvsen1 <- find_stack(ccs, approval_sen1, make_labelled = FALSE)
apvsen2 <- find_stack(ccs, approval_sen2, make_labelled = FALSE)
apvgov <- find_stack(ccs, approval_gov, make_labelled = TRUE)

## economy -----
econ_raw <- find_stack(ccs, economy_retro, make_labelled = FALSE, new_reorder = FALSE)
econ_fixed <- econ_raw |>
  collapse_economy_retro() |>
  replace_year(2009, cc09_econ)
econ <- finalize_economy_retro(econ_fixed)


## news interest
newsint <- find_stack(ccs, newsint, make_labelled = TRUE) |>
  remove_value_labels(newsint = 8) |>
  mutate(newsint = na_if(newsint, 8))


# geography ----
cong <- find_stack(ccs, cong, "integer")
cong_up <- find_stack(ccs, cong_up, "integer")

cli_h1("Joining geography")
state <- find_stack(ccs, state, "character")
state_post <- find_stack(ccs, state_post, "character")
st <- find_stack(ccs, st, "character")
st_post <- find_stack(ccs, st_post, "character")

zipcode <- find_stack(ccs, zipcode, "character") |>
  mutate(zipcode = str_pad(zipcode, width = 5, pad = "0"))

county_fips_raw <- find_stack(ccs, county_fips, "numeric")
county_fips <- county_fips_raw |>
  left_join2(cc17_county) |>
  mutate(county_fips = coalesce(county_fips, as.numeric(countyfips))) |>
  select(-countyfips) |>
  replace_year(2007, cc07_county)

dist <- find_stack(ccs, dist, "integer")
dist_up <- find_stack(ccs, dist_up, "integer")
cd <- find_stack(ccs, cd, "character")
cd_up <- find_stack(ccs, cd_up, "character")

dist_post <- find_stack(ccs, dist_post, "integer")
dist_up_post <- find_stack(ccs, dist_up_post, "integer")
cd_post <- find_stack(ccs, cd_post, "character")
cd_up_post <- find_stack(ccs, cd_up_post, "character")

cli_alert_success("Finished joining each variable. Now combining them")

## format state and CD, then zipcode and county ----
stcd <- left_join2(state, st) |>
  left_join2(cong) |>
  left_join2(cong_up) |>
  left_join2(state_post) |>
  left_join2(st_post) |>
  left_join2(dist) |>
  left_join2(dist_up) |>
  left_join2(cd) |>
  left_join2(cd_up) |>
  left_join2(dist_post) |>
  left_join2(dist_up_post) |>
  left_join2(cd_post) |>
  left_join2(cd_up_post)

geo <- stcd |>
  left_join2(zipcode) |>
  left_join2(county_fips)

# Join all vars ----
ccc <- geo |>
  left_join2(tookpost) |>
  left_join2(wgt) |>
  left_join2(wgt_post) |>
  left_join2(vwgt) |>
  left_join2(vwgt_post) |>
  left_join2(time) |>
  left_join2(pid3) |>
  left_join2(pid3_leaner) |>
  left_join2(pid7) |>
  left_join2(ideo5) |>
  left_join2(gend) |>
  left_join2(sex) |>
  left_join2(gend4) |>
  left_join2(sexor) |>
  left_join2(bryr) |>
  left_join2(age) |>
  left_join2(race) |>
  left_join2(hisp) |>
  left_join2(race_anyh) |>
  left_join2(hisp_origin) |>
  left_join2(citizen) |>
  left_join2(educ) |>
  left_join2(marstat) |>
  left_join2(faminc) |>
  left_join2(union) |>
  left_join2(union_hh) |>
  left_join2(employ) |>
  left_join2(healthins) |>
  left_join2(invst) |>
  left_join2(child18) |>
  left_join2(ownhome) |>
  left_join2(milstat) |>
  left_join2(relig) |>
  left_join2(religimp) |>
  left_join2(bornagain) |>
  left_join2(protestant) |>
  left_join2(churatd) |>
  left_join2(econ) |>
  left_join2(newsint) |>
  left_join2(apvpres) |>
  left_join2(apvrep) |>
  left_join2(apvsen1) |>
  left_join2(apvsen2) |>
  left_join2(apvgov) |>
  left_join2(i_pres08) |>
  left_join2(i_pres12) |>
  left_join2(i_pres16) |>
  left_join2(i_pres20) |>
  left_join2(i_pres24) |>
  left_join2(v_pres08) |>
  left_join2(v_pres12) |>
  left_join2(v_pres16) |>
  left_join2(v_pres20) |>
  left_join2(v_pres24) |>
  left_join2(pres_party) |>
  left_join2(reg_self) |>
  left_join2(intent_trn) |>
  left_join2(voted_trn) |>
  left_join2(vv_regstatus) |>
  left_join2(vv_party_gen) |>
  left_join2(vv_party_prm) |>
  left_join2(vv_turnout_gvm) |>
  left_join2(vv_turnout_pvm) |>
  left_join2(vv_state)

# Checks ---
# check no accidental duplicate id's within 2012 or 2009
foo_09 <- wgt |> filter(year == 2009)
stopifnot(nrow(foo_09) == nrow(distinct(foo_09, year, case_id)))

foo_12 <- wgt |> filter(year == 2012)
stopifnot(nrow(foo_12) == nrow(distinct(foo_12, year, case_id)))


# don't use panel rows for now
panel_id <- ccs[["2012panel"]] |> select(year, case_id) |> mutate(case_id = as.integer(case_id))
# mit06_id <- ccs[["2006mit"]] |> select(year, case_id) |> mutate(case_id = as.integer(case_id))
# hu08_id <- ccs[["2008hu"]] |> select(year, case_id) |> mutate(case_id = as.integer(case_id))
# hu09_id <- ccs[["2009hu"]] |> select(year, case_id) |> mutate(case_id = as.integer(case_id))
comp_id <- ccs[["2018comp"]] |> select(year, case_id) |> mutate(case_id = as.integer(case_id))
addon_id <- bind_rows(panel_id, comp_id) # hu08_id, hu09_id,


# Common manipulations ----
# Weight --
size_year <- ccc |>
  anti_join(addon_id, by = c("year", "case_id")) |> # don't count panel to get weights
  summarize(size = n(), .by = year) |>
  mutate(size_factor = size / median(size)) # manageable constant -- divide by median

ccc_sort <- ccc |>
  left_join(select(size_year, year, size_factor), by = c("year"), relationship = "many-to-one") |>
  mutate(weight_cumulative = weight / size_factor, size_factor = NULL) |>
  relocate(year, case_id, weight, weight_cumulative)


# Write -----
cli_alert_success("Finished combining, now saving")
save(i_rep, i_sen, i_gov, v_rep, v_sen, v_gov, file = "data/output/01_responses/vote_responses.RData")
save(vv_party_gen, vv_party_prm, vv_regstatus, vv_turnout_gvm, vv_turnout_pvm, file = "data/output/01_responses/vv_responses.RData")
write_feather(ccc_sort, "data/output/01_responses/cumulative_stacked.feather")
saveRDS(addon_id, "data/output/01_responses/addon_ids.Rds")
write_csv(size_year, "data/output/03_contextual/weight_rescale_by-year.csv")

cli_alert_success("Finished stacking vars for cumulative")
