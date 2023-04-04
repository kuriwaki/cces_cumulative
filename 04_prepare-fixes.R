# subsets to fix in 02
library(tidyverse)
library(dplyr)
library(haven)
load("data/output/01_responses/common_all.RData")




# Retrieve 2007 County -----


# Retrieve 2010 PID  -------
pid3_cc10 <- cc10 %>%
  mutate(pid3 = V212a) %>%
  mutate(
    pid3_char = as.character(as_factor(pid3)),
    pid3_num = as.numeric(pid3)
  ) %>%
  mutate(
    pid3_char = replace(pid3_char, pid3_char == "NaN", NA),
    pid3_num = replace(pid3_num, is.nan(pid3_num), NA)
  ) %>%
  select(year, case_id, pid3_char, pid3_num)


# 2009 Economic retrospective recode

econ_recoded <- cc09 %>% 
  select(year, case_id, cc09_20) %>% 
  mutate(economy_retro_num = recode(as.integer(haven::zap_labels(cc09_20)),
                      `1` = 5L, `2` = 4L, `3` = 3L, `4` = 2L, `5` = 1L)) %>% 
  mutate(economy_retro_char = case_when(economy_retro_num == 5L ~ "Gotten Much Worse",
                                        economy_retro_num == 4L ~ "Gotten Worse / Somewhat Worse",
                                        economy_retro_num == 3L ~ "Stayed About The Same",
                                        economy_retro_num == 2L ~ "Gotten Better / Somewhat Better",
                                        economy_retro_num == 1L ~ "Gotten Much Better"
                                        )) %>% 
  select(-cc09_20)

# date time in 2006 and 2009 ----
fmt_date <- function(vec) {
  as.POSIXct(vec, format = "%a %b %e %T %Y")
}

cc06_time <- cc06 %>%
  mutate(starttime = fmt_date(starttime)) %>%
  select(year, case_id, starttime) %>% 
  bind_rows(select(mit06_add, year, case_id, starttime))

cc09_time <- cc09 %>%
  mutate(starttime = as.POSIXct(v401)) %>%
  select(year, case_id, starttime)


# recode newsinterest to fit with 2008 - 2018
cc06_interest <- cc06 %>% 
  transmute(year, 
            case_id, 
            interest = as.integer(v2042))

# save ---------
write_rds(pid3_cc10, "data/output/01_responses/cc10_pid3.Rds")
write_rds(econ_recoded, "data/output/01_responses/cc09_econ_retro.Rds")
write_rds(cc06_time, "data/output/01_responses/cc06_datetime.Rds")
write_rds(cc09_time, "data/output/01_responses/cc09_datetime.Rds")
write_rds(cc06_interest, "data/output/01_responses/cc06_newsintnum.Rds")
# fs::file_copy("data/output/01_responses/cc06_newsintnum.Rds",
#               "~/Dropbox/CCES_representation/data/output/intermediate", overwrite = TRUE)
