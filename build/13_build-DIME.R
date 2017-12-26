library(tidyverse)

# dime_raw is original long list of all dime recipients from 1979-2014
# dime_cand is dime_raw with only candidates
# dime_cols is dime_cand with only relevant columns
# dime_full is dime_cols without blank rows
# df is minigoodchart 2 without duplicate rows

dime_raw <- read_csv("data/source/dime/dime_recipients_all_1979_2014.csv")

dime_cand <- dime_raw %>%
  filter(!grepl("comm", bonica.rid))

dime_cols <- dime_cand %>%
  select(
    FEC = FEC.ID,
    ICPSR = ICPSR2,
    seat = seat
  )

dime_full <- dime_cols %>%
  filter(!is.na(FEC))

df <- dime_full %>%
  distinct(FEC, ICPSR, seat)


# strip away letters from icpsr
df <- df %>%
  mutate(ICPSR = gsub("[A-z]+", "", ICPSR)) %>%
  mutate(ICPSR = as.integer(ICPSR)) %>%
  rename(icpsr = ICPSR)


# save
saveRDS(df, "data/output/03_contextual/dime_fmt.Rds")
