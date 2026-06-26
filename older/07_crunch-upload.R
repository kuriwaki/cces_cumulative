# Written by Claude Code
# Archived from 07_merge-contextual_upload.R (2026-06-26).
# Crunch.io upload step. This pushed the SPSS-formatted cumulative file (plus the
# Schaffner issue variables) to Crunch.io. It is NOT needed for the Dataverse
# release and is disabled in the main build. Kept here for reference only.
#
# Dependencies (created in 07_merge-contextual_upload.R before this block ran):
#   - ccc_common : the panel-removed, factor-formatted cumulative data
# Plus the Schaffner issues source read below.

library(tidyverse)
library(haven)

writeToCrunch <- FALSE # set TRUE to (re)build the Crunch dataset

# only needed for Crunch upload
bs_stata <- read_dta("data/source/cces/schaffner_issues.dta")

if (writeToCrunch) {
  cli::cli_h1("Writing to crunch")
  library(crunch)

  # crunch var
  bs_df <- bs_stata |>
    select(-religion) |> # already in
    mutate(case_id = as.character(case_id),
           year = as.integer(year)) |>
    select(year, case_id, everything())

  ccc_crunch <- ccc_common |>
    left_join(bs_df, by = c("year", "case_id")) |>
    mutate(year_date = as.Date(str_c(as.character(year), "-11-01"), "%Y-%m-%d")) |>
    select(year, year_date, everything())

  write_sav(ccc_crunch, "data/release/cumulative_2006-2021_crunch.sav")

  if (file.exists("data/release/cumulative_2006-2021_crunch.sav.gz")) {
    file.remove("data/release/cumulative_2006-2021_crunch.sav.gz")
    R.utils::gzip("data/release/cumulative_2006-2021_crunch.sav")
  }

  # write to crunch
  login()
  deleteDataset("CCES Cumulative Common Dev")
  newDataset("https://www.dropbox.com/s/p8cx49h82coqfcs/cumulative_2006_2018_crunch.sav?dl=0",
             "CCES Cumulative Common Dev")
  logout()
}
