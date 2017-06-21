rm(list = ls())
# Test crunch.io

# devtools::install_github("Crunch-io/rcrunch", build_vignettes=TRUE)
library(crunch)
library(ggplot2)
library(dplyr)
library(foreach)
library(readr)
library(xtable)



# Start up crunch -------
login() # you need a login and password to complete this command


# connect to data---------
ds <- loadDataset("CCES 2016 Common")



# metadata --------
vars <- variables(ds)
meta <- variableMetadata(ds)

length(vars)
length(meta)


# take a peek
vars[[1]]

# one at a time
str(meta[[30]])


# get q wording -------
qwording <- foreach(i = 1:length(meta), .combine = "bind_rows") %do% {
  
  vm <- meta[[i]]
  
  wording <- vm@body$description
  
  id <- vm@body$id
  
  alias <- vm@body$alias
  
  name <- vm@body$name
  
  type <- vm@body$type
  
  nChoices <- length(vm@body$categories)
  
  nSubQuestions <- length(vm@body$subreferences)
  
  
  # break up the grid here
  
  
  # make this comprehensive
  tibble(id = id,
         alias = alias, 
         name = name, 
         type = type,
         nChoices = nChoices,
         nSubQuestions = nSubQuestions,
         wording = wording)
}


saveRDS(meta,  "data/output/meta/raw_metadata_cc16.Rds")
saveRDS(qwording, "data/output/meta/fmt_metadata_cc16.Rds")
write_csv(qwording, "data/output/meta/fmt_metadata_cc16.csv")
write_csv(qwording, "~/Dropbox/CCES_SDA/2016/Guide/fmt_metadata_cc16.csv")






