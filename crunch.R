rm(list = ls())
# Test crunch.io

# devtools::install_github("Crunch-io/rcrunch", build_vignettes=TRUE)
library(crunch)
library(ggplot2)
library(dplyr)
library(foreach)
library(readr)
library(xtable)

# Directory ------
wd <- "~/Dropbox/cces_cumulative/"
setwd(wd)



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
  
  nChoices <- length(vm@body$categories)
  
  nSubQuestions <- length(vm@body$subreferences)
  
  
  tibble(id = id,
         alias = alias, 
         name = name, 
         nChoices = nChoices,
         nSubQuestions = nSubQuestions,
         wording = wording)
}


saveRDS(meta,  "data/output/meta/raw_metadata_cc16.Rds")
saveRDS(qwording, "data/output/meta/fmt_metadata_cc16.Rds")
write_csv(qwording, "data/output/meta/fmt_metadata_cc16.csv")


# Tabulations -----

choiceqs.rownum <- which(qwording$nChoices != 0 & qwording$nSubQuestions == 1)

for (i in choiceqs.rownum) {
  alias <- qwording$alias[i]
  name <- qwording$name[i]

  var.tab <- crtabs(paste0("~ ", alias), ds)
  var.arr <- var.tab@arrays
  
  cat.obj <- meta[[name]]@body$categories
  names.vec <- sapply(cat.obj, "[", "name") %>% unlist() %>% as.character()
  no.vec <- sapply(cat.obj, "[", "id") %>% unlist() %>% as.integer()
  
  print(var.arr)
  
  simp.tab <- tibble(uw.count = var.arr$.unweighted_counts,
                     w.count = round(var.arr$count),
                     choice.num = no.vec,
                     choice.name = names.vec)
  
  simp.xtab <- xtable(simp.tab)
  
  print(simp.xtab, 
        file.path(wd, "data/output/meta/tabs/", paste0(alias, ".tex")))
}




# cross tabs and getting data sets--------
crtabs(~ pid3 + presvote, ds)
pp <- ds[ds$inputstate == "Kentucky", c("pid3", "presvote")]
variables(pp)


# downloading
pp.df <- as.data.frame(pp, force = T)
str(pp.df)
?as.data.frame.CrunchDataFrame


# viewing
ds$presvote


# applying models
ols1 <- lm(I(presvote == "Donald Trump (Republican)") ~ pid3 + gender + age,
           data = ds)
summary(ols1)

