library(tidyverse)
library(googlesheets)

# sweeps through Crunch variable meta-table

gs_main <- gs_title("CCES_crunch_variables")


cc <- gs_read(gs_main, ws = "CCES_2008_variables")


# duplicates ---
cc %>% 
  group_by(name) %>% 
  tally(sort = TRUE)
