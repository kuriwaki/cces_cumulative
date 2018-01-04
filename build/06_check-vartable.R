library(tidyverse)
library(googlesheets)

# sweeps through Crunch variable meta-table

gs_main <- gs_title("CCES_crunch_variables")


cc <- gs_read(gs_main, ws = "CCES_216_variables")


# duplicates ---
cc %>%
  group_by(name) %>%
  tally(sort = TRUE)

ds <- loadDataset("cumulative_2006_2016.sav", project = "CCES Common Content")