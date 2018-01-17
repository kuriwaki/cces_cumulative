rm(list = ls())
# Test crunch.io

# devtools::install_github("Crunch-io/rcrunch", build_vignettes=TRUE)
library(crunch)
library(ggplot2)
library(dplyr)
library(foreach)
library(readr)
library(readxl)
library(xtable)


# variable list ----
# to make a good guess about which is pre/post
vars_edited <- read_csv("data/source/cces_meta/2016_guidebook_variables_orderedby2014.csv") %>%
  select(section14, code16)


# Start up crunch -------
login() # you need a login and password to complete this command


# connect to data---------
ds <- loadDataset("CCES 2016 Common")

# don't need weights ?
weight(ds) <- NULL


# metadata --------
vars <- variables(ds)
meta <- variableMetadata(ds)


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
  name <- gsub("^\\$", "", name) # same as wording
  name <- gsub("\\$", "\\\\$", name) # for real $, escape

  type <- vm@body$type

  nChoices <- length(vm@body$categories)

  nSubQuestions <- length(vm@body$subreferences)


  # if a grid question more than zero. 1 means only one question
  nQs <- ifelse(nSubQuestions == 0, 1, nSubQuestions)


  # guess pre or post (will affect subset)
  section_guess <- (vars_edited %>% filter(code16 == alias) %>% pull(section14))
  is_post <- grepl("Post-Election", section_guess)
  if (length(section_guess) != 1) is_post <- FALSE
  if (grepl("(CC|_)4[0-9][0-9]", alias)) is_post <- TRUE
  if (grepl("post", alias, ignore.case = TRUE)) is_post <- TRUE
  if (grepl("edloan", alias, ignore.case = TRUE)) is_post <- TRUE

  # counts
  if (!is_post) {
    var.tab <- crtabs(paste0("~ ", alias), ds, useNA = "ifany")
  }
  if (is_post) {
    var.tab <- crtabs(paste0("~ ", alias), ds[ds$tookpost == "Took post"], useNA = "ifany")
  }

  # get counts
  var.arr <- var.tab@arrays$count
  if (class(var.arr) == "array") var.arr <- matrix(var.arr, nrow = 1)

  # choices
  cat.obj <- vm@body$categories
  choicenames.vec <- sapply(cat.obj, "[", "name") %>% unlist() %>% as.character()
  choiceno.vec <- sapply(cat.obj, "[", "id") %>% unlist() %>% as.integer()


  # "check all that apply" questions are not in grid-form, but need to be divided into subvariables
  # the choice names will be the same, and they will be in columns.
  # just need to populate one row of var.arr to be one subvariable, syntax alias[[1]]
  if (type == "multiple_response") {
    rm(var.arr)

    # we don't have a good way to distinguish between missings (not asked vs. skipped, etc..)
    # so, combine these "by hand"
    subvar.tab <- foreach(j = 1:nQs) %do% {
      # repeat what we had above, replacing values
      if (!is_post) {
        subvar.tab <- crtabs(paste0("~", alias, "[[", j, "]]"), ds, useNA = "ifany")
      }
      if (is_post) {
        subvar.tab <- crtabs(paste0("~", alias, "[[", j, "]]"), ds[ds$tookpost == "Took post"], useNA = "ifany")
      }

      subvar.tab
    }

    # flatten out to get the numbers
    var.arr <- t(sapply(subvar.tab, function(x) matrix(x@arrays$count, nrow = 1)))
  }


  # if a grid, get alias for each question
  if (nQs > 1) {
    subcat.obj <- var.tab@.Data[[3]]$dimensions[[1]]$references$subreferences
    q.aliases <- sapply(subcat.obj, "[", "alias") %>% unlist() %>% as.character()
    q.names <- sapply(subcat.obj, "[", "name") %>% unlist() %>% as.character()
    q.names <- gsub("^\\$", "", q.names) # same as wording
    q.names <- gsub("\\$", "\\\\$", q.names) # for real $, escape
  } else {
    q.aliases <- alias
    q.names <- name
  }

  # get length
  nr <- nrow(var.arr)

  if (i %% 50 == 0) cat(paste0(i, " out of ", length(meta_objs), " done ...\n"))

  # finally, if numeric, ignore everything and get the whole vector
  if (type == "numeric") {
    var.arr <- t(as.vector(ds[[alias]]))
    nr <- 1
  }

  # duplicate id for each grid row. store counts and choice labels as c
  tibble(
    id = id,
    alias = q.aliases,
    meta_obj_id = i,
    wording = wording,
    name = q.names,
    count = as.list(data.frame(t(var.arr))), # https://stackoverflow.com/a/6819883/5525412
    level = rep(list(choiceno.vec), nr),
    labels = rep(list(choicenames.vec), nr),
    type = type,
    is_post_gues = is_post,
    nChoices = nChoices,
    nSubQuestions = nSubQuestions
  )
}

# duh???
# crtabs(~CC16_312[[1]], ds, useNA = "ifany")


logout()




# auxilary tabulations from flat file --- these were not in crunch

vars <- read_excel(
  "data/source/cces_meta/Pre_Post_Contextual_Variables.xlsx",
  col_names = c("section16", "alias", "label")
)
cc16 <- read_dta("data/source/cces/2016_cc.dta")


vars_in_cc16 <- intersect(vars$alias, colnames(cc16))

cx <- foreach(nam = vars_in_cc16, .combine = "bind_rows") %do% {
  lab <- filter(vars, alias == nam) %>% pull(label)

  table_nam <- table(cc16[[nam]])
  var.arr <- matrix(as.numeric(table_nam), nrow = 1)
  choicenames.vec <- names(table_nam)

  choiceno.vec <- names(table(zap_labels(cc16[[nam]])))
  # but if it's text, don't bother..
  if (identical(choiceno.vec, choicenames.vec)) choiceno.vec <- rep(NA, length(choiceno.vec))

  nChoices <- length(choicenames.vec)

  nr <- nrow(var.arr)


  tibble(
    alias = nam,
    wording = "",
    name = lab,
    count = as.list(data.frame(t(var.arr))),
    level = rep(list(choiceno.vec), nr),
    labels = rep(list(choicenames.vec), nr),
    type = "categorical", # looks like
    nChoices = nChoices,
    nSubQuestions = 1
  )
}


metadata <- bind_rows(metadata, cx)

saveRDS(metadata, "data/output/meta/fmt_metadata_cc16.Rds")
