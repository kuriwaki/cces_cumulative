# subsets to fix in 02

library(dplyr)
load("data/output/01_responses/common_all.RData")


# Fix 2010 PID  -------
pid3_cc10 <- cc10 %>%
  mutate(pid3 = CC421a) %>%
  mutate(
    pid3_char = as.character(as_factor(pid3)),
    pid3_num = as.numeric(pid3)
  ) %>%
  mutate(
    pid3_char = replace(pid3_char, pid3_char == "NaN", NA),
    pid3_num = replace(pid3_num, is.nan(pid3_num), NA)
  ) %>%
  select(year, caseID, pid3_char, pid3_num)


# date time in 2006 and 2009 ----
fmt_date <- function(vec) {
  as.POSIXct(vec, format = "%a %b %e %T %Y")
}

cc06_time <- cc06 %>%
  mutate(starttime = fmt_date(starttime)) %>%
  select(year, caseID, starttime)

cc09_time <- cc09 %>%
  mutate(starttime = as.POSIXct(v401)) %>%
  select(year, caseID, starttime)


# save ---------
saveRDS(pid3_cc10, "data/output/01_responses/pid3_cc10.Rds")
saveRDS(cc06_time, "data/source/cces/cc06_datetime.Rds")
saveRDS(cc09_time, "data/source/cces/cc09_datetime.Rds")
