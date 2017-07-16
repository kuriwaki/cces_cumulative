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

# don't need weights ?
weight(ds) <- NULL


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
  wording <- gsub("^\\$", "", wording) # sometimes wording starts with weird dollarsign
  wording <- gsub("\\$", "\\\\$", wording) # for real $, escape
  
  
  id <- vm@body$id
  
  alias <- vm@body$alias
  
  name <- vm@body$name
  name <- gsub("^\\$","", name) # same as wording
  name <- gsub("\\$", "\\\\$", name) # for real $, escape
  
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

  
  # "check all that apply" questions are not in grid-form, but count total yeses.
  # so make that in some grid form again...
  if (type == "multiple_response") {
    missing <- var.arr[var.tab@dims[[1]]$missing]
    totalnonmissing <- var.tab@.Data[[3]]$n - missing # apparently slot 3...
    
    # get the yeses
    checks.ind <- !(var.tab@dims[[1]]$any.or.none | var.tab@dims[[1]]$missing)
    checks <- var.arr[checks.ind]
    
    # get the nos
    notchecked <- totalnonmissing - checks
    
    # how many more columns (extra questions) do we have to add?
    # sub 2 for check and no check. then 1 for missing
    extracols <- length(choiceno.vec) - 2 - 1 
    zeros_mat <- matrix(0, nrow = length(checks), ncol = extracols)
    
    counts <- cbind(checks, notchecked, missing, zeros_mat)
    
    
    var.arr <- counts
  }
  

  # if a grid, get alias for each question
  if (nQs > 1) {
    subcat.obj <- var.tab@.Data[[3]]$dimensions[[1]]$references$subreferences
    q.aliases <- sapply(subcat.obj, "[", "alias") %>% unlist() %>% as.character()
    q.names <- sapply(subcat.obj, "[", "name") %>% unlist() %>% as.character()
    q.names <- gsub("^\\$","", q.names) # same as wording
    q.names <- gsub("\\$", "\\\\$", q.names) # for real $, escape
    
    
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

logout()

saveRDS(metadata, "data/output/meta/fmt_metadata_cc16.Rds")
