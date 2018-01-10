library(dplyr)
library(readr)
library(foreach)
library(Rvoteview)


path <- list.files("data/source/voteview", full.names = TRUE)
paths_read <- grep("/HS.*members.csv$", path, value = TRUE)

# read and bind --
vv_raw <- foreach(p = paths_read, .combine = "bind_rows") %do% {
  read_csv(p, col_types = cols())
}


# pick last name --
vv_namelast <- vv_raw %>%
  mutate(namelast = toupper(gsub("^([A-z-]+),.*", "\\1", bioname)))


# formats ---
vv <- vv_namelast %>%
  select(congress, chamber, icpsr, namelast, everything()) %>%
  rename(
    st = state_abbrev,
    dist = district_code
  ) %>%
  mutate(
    chamber = replace(chamber, chamber == "House", "H"),
    chamber = replace(chamber, chamber == "Senate", "S")
  )


## H and S
vvH <- vv %>%
  filter(chamber == "H") %>%
  mutate(chamber = "H")

vvS <- vv %>%
  filter(chamber == "S") %>%
  arrange(icpsr) %>%
  select(-dist)



## sort for now ---
vvH_min <- vvH %>%
  select(congress, chamber, icpsr, st, dist, namelast) %>%
  mutate(CD = paste0(st, "-", dist))

vvS_min <- vvS %>%
  select(congress, chamber, icpsr, st, namelast)

## save ---
saveRDS(vv, "data/output/03_contextual/voteview_mcs.Rds")
saveRDS(vvH_min, "data/output/03_contextual/voteview_H_key.Rds")
saveRDS(vvS_min, "data/output/03_contextual/voteview_S_key.Rds")
