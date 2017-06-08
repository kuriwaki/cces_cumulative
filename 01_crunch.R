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

choiceqs.rownum <- which(qwording$nChoices != 0 & qwording$nSubQuestions == 0)

for (i in choiceqs.rownum) {
  alias <- qwording$alias[i]
  name <- qwording$name[i]

  var.tab <- crtabs(paste0("~ ", alias), ds)
  var.arr <- var.tab@arrays
  
  cat.obj <- meta[[name]]@body$categories
  names.vec <- sapply(cat.obj, "[", "name") %>% unlist() %>% as.character()
  no.vec <- sapply(cat.obj, "[", "id") %>% unlist() %>% as.integer()
  
  
  simp.tab <- tibble(`Unweighted N` = var.arr$.unweighted_counts,
                     `Weighted N` = round(var.arr$count),
                     `num` = no.vec,
                     `Choice Text` = names.vec)

  
  addtorow <- list()
  addtorow$pos <- list(-1)
  addtorow$command <- c(paste0(paste0('\\multicolumn{4}{l}{', alias, '}'), "\\\\"))
  
  simp.xtab <- xtable(simp.tab, display = c("d", "d", "d", "d", "s"))
  
  
  filename <- paste0(formatC(i, width = 3, format = "d", flag = "0"), "_",  alias, ".tex")
  
  print(simp.xtab, 
        include.rownames = FALSE,
        add.to.row =  addtorow,
        file = file.path(wd, "data/output/meta/tabs/", filename))
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

