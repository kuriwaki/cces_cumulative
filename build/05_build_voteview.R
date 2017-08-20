library(dplyr)
library(readr)
library(foreach)


path <- list.files("data/source/voteview", full.names = TRUE)
paths_read <- grep("/HS.*members.csv$", path, value = TRUE)

vv <- foreach(p = paths_read, .combine = "bind_rows") %do% {
  read_csv(p)
}


# pick last name
vv <- mutate(vv,lastname = gsub("^([A-Z]+),.*", "\\1", bioname)) %>%
  select(congress, chamber, icpsr, lastname, everything())


saveRDS(vv, "data/output/03_contextual/voteview_mcs.Rds")
