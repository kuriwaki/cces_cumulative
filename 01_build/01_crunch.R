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


# get q wording -------
meta_objs <- list()

for (i in 1:length(vars)) {
  name <- vars[[i]]$name
  vm <- meta[[name]]
  meta_objs[[i]] <- vm
}


# collect meta data -------
metadata <- foreach(i = 1:length(meta_objs), .combine = "bind_rows") %do% {
  
  vm <- meta_objs[[i]]
  
  wording <- vm@body$description
  
  id <- vm@body$id
  
  alias <- vm@body$alias
  
  name <- vm@body$name
  
  
  type <- vm@body$type
  
  
  nChoices <- length(vm@body$categories)
  
  nSubQuestions <- length(vm@body$subreferences)
  
  # if a grid question more than zero. 1 means only one question
  nQs <- ifelse(nSubQuestions == 0, 1, nSubQuestions)
  
  # counts
  var.tab <- crtabs(paste0("~ ", alias), ds, useNA = "ifany")
  var.arr <- var.tab@arrays$.unweighted_counts
  if (class(var.arr) == "array") var.arr <- matrix(var.arr, nrow = 1)

  # choices
  cat.obj <- vm@body$categories
  choicenames.vec <- sapply(cat.obj, "[", "name") %>% unlist() %>% as.character()
  choiceno.vec <- sapply(cat.obj, "[", "id") %>% unlist() %>% as.integer()


  # if a grid, get alias for each question
  if (nQs > 1) {
    subcat.obj <- vm@body$subreferences
    q.aliases <- sapply(subcat.obj, "[", "alias") %>% unlist() %>% as.character()
    q.names <- sapply(subcat.obj, "[", "name") %>% unlist() %>% as.character()
  } else {
    q.aliases <- alias
    q.names <- name
  }
  
  
  
  if(i %% 50 == 0) cat(paste0(i, " out of ", length(meta_objs), " done ...\n"))
  
  # duplicate id for each grid row. store counts and choice labels as c
  tibble(id = id,
         alias = q.aliases,
         wording = wording,
         name = q.names,
         count = as.list(data.frame(t(var.arr))), # https://stackoverflow.com/a/6819883/5525412
         level = rep(list(choiceno.vec), nrow(var.arr)),
         labels = rep(list(choicenames.vec), nrow(var.arr)),
         type = type,
         nChoices = nChoices,
         nSubQuestions = nSubQuestions)
}
metadata


saveRDS(metadata, "data/output/meta/fmt_metadata_cc16.Rds")
