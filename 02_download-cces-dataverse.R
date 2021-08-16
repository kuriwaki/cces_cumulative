library(ccesMRPprep)
library(tidyverse)
library(glue)
library(fs)

# this will not recreate all the datasets on my local project, but it gives a start.
# Contact me if you need any data used in subsequent code but not available in Dataverse.

dir_create("data/source/cces")
dir_create("data/output")
dir_create("data/release")

for (yr in 2006:2020) {
  filedir <- "data/source/cces"
  
  filename <- glue("{yr}_cc.dta")
  if (yr == 2007)
    filename <- glue("{yr}_cc.sav")
  
  cat(filename, "\n")
  
  if (file_exists(path(filedir, filename)))
    next
  
  dataverse_dl <- get_cces_dv(name = yr)
  read_dta(dataverse_dl, path(filedir, filename))
}
