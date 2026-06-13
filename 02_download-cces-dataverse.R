library(ccesMRPprep)
stopifnot(packageVersion("ccesMRPprep") >= "0.1.15")
library(haven)
library(glue)
library(fs)
library(cli)
library(dataverse)
library(tidyverse)

dir_create("data/source/cces")
dir_create("data/output")
dir_create("data/release")

for (yr in 2006:2025) {
  filedir <- "data/source/cces"
  
  filename <- glue("{yr}_cc.dta")
  
  if (file_exists(path(filedir, filename))) next
  
  cli_alert_info("Will download and write {.file {filename}}.")
  dataverse_dl <- get_cces_dataverse(name = yr)
  write_dta(dataverse_dl, path(filedir, filename))
}

# Old CCES
ccp <- get_dataframe_by_name(
  filename = "cces_common_cumulative_4.dta", 
  "10.7910/DVN/26451", 
  version = "5.0",
  .f = haven::read_dta, 
  original = TRUE
  )
write_dta(ccp, "data/source/cces/2006_2012_cumulative.dta")

# Modules and panels ----------

# TODO: This differs from the version on Dropbox
cc18_comp <- get_dataframe_by_name(
  filename = "CCES18_CD_vv.dta", 
  "10.7910/DVN/KDAWBM", 
  version = "1",
  .f = haven::read_dta, 
  original = TRUE
  )
write_dta(cc18_comp, "data/source/cces/2018_cc_competitive.dta")

# TODO: This differs from the version on Dropbox
# Alt: "CCES_Panel_Full3waves_VV_V4.dta", "10.7910/DVN/TOE8I1", version = "11",
panel12_orig <- get_dataframe_by_name(
  filename = "CCES12_Panel_OUTPUT_10Oct2013_with2010_vv_V2.tab", 
  "10.7910/DVN/24416", 
  version = "4.0",
  .f = haven::read_dta, 
  original = TRUE
)
# Relocate a _pre / _post tag ahead of the year suffix, so the year (_10/_12)
# stays last. Pulled out of rename_with() so there's no pipe inside the pipe.
move_tag_before_year <- function(x, tag) {
  moved <- str_replace(x, "_(?=1\\d_)", str_c("_", tag, "_"))
  str_remove(moved, str_c("(?<=1\\d)_", tag))
}

# 2012-specific columns — caseid added as the join key
panel12_2012 <- panel12_orig |>
  select(caseid, contains("_12") & !starts_with("CC")) |>
  rename_with(\(x) move_tag_before_year(x, "pre"),  ends_with("_pre")) |>
  rename_with(\(x) move_tag_before_year(x, "post"), ends_with("_post")) |>
  mutate(
    across(starts_with("cdid112"), as.numeric),
    across(starts_with("regzip"), as.character),
    across(where(is.character), \(x) na_if(x, "__NA__")),
    year = 2012
  ) |>
  rename_with(\(x) str_remove(x, "_12$"), contains("_12"))

# Shared / time-invariant columns (unchanged)
panel12_shared <- panel12_orig |>
  select(!starts_with("CC10") & !contains("_10") & !contains("_12"))

panel12 <- panel12_2012 |>
  inner_join(
    panel12_shared,
    by = "caseid",
    relationship = "one-to-one",
    unmatched = "error"
  ) |>
  rename(
    cdid      = cdid112,
    cdid_post = cdid112_post
  )
write_dta(panel12, "data/source/cces/2012_panel_h.dta")

hum09 <- get_dataframe_by_name(
  filename = "cces09_harvard_output.tab", 
  "10.7910/DVN/NLCNWR", 
  version = "1.0",
  .f = haven::read_dta, 
  original = TRUE
)
write_dta(hum09, "data/source/cces/2009_hum.dta")

# County lookups ----------
county17 <- get_dataframe_by_name(
  filename = "CCES17_Common_county.tab",
  "10.7910/DVN/3STEZY",
  version = "2.0",
  .f = \(path) readr::read_csv(path, show_col_types = FALSE),
  original = TRUE
)
readr::write_csv(county17, "data/source/cces/CCES17_Common_county.csv")

# Possibly no longer used in pipeline
hum08 <- get_dataframe_by_name(
  filename = "cces08_harvard_output.dta", 
  "10.7910/DVN/WXXXJO", 
  version = "2.0",
  .f = haven::read_dta, 
  original = TRUE
) |> 
  rename_all(str_to_upper) |>
  select(-HUM302, -HUM304) # decimal labelled
write_dta(hum08, "data/source/cces/2008_hum.dta")

mit06 <- get_dataframe_by_name(
  filename = "mit_final_withcommon_validated_new.tab", 
  "10.7910/DVN/EK9MGR", 
  version = "2.0",
  .f = \(x) haven::read_dta(x, encoding = "latin1"), 
  original = TRUE
)
write_dta(mit06, "data/source/cces/2006_mit_final_withcommon_validated_new.dta")

hua18 <- get_dataframe_by_name(
  filename = "CCES18_HUA_OUTPUT_vv.tab", 
  "10.7910/DVN/ZLNSYN", 
  version = "2.0",
  .f = haven::read_sav,
  original = TRUE
)
write_sav(hua18, "data/source/cces/2018_hua.sav")

hub18 <- get_dataframe_by_name(
  filename = "CCES18_HUB_OUTPUT_vv.tab", 
  "10.7910/DVN/ZLNSYN", 
  version = "2.0",
  .f = haven::read_sav, 
  original = TRUE
)
write_sav(hub18, "data/source/cces/2018_hub.sav")
