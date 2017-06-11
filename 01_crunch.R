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
  
  type <- vm@body$type
  
  nChoices <- length(vm@body$categories)
  
  nSubQuestions <- length(vm@body$subreferences)
  
  
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


# Tabulations -----

choiceqs.rownum <- which(qwording$nChoices != 0 & qwording$type != "multiple_response")
tabs.list <- list()

# get xtable, then print

writeToFile <- TRUE

for (i in choiceqs.rownum) {
  
  # if a grid question more than zero. 1 means only one question
  nQs <- ifelse(qwording$nSubQuestions[i] == 0, 1, qwording$nSubQuestions[i])
  
  # alias and name
  alias <- qwording$alias[i]
  name <- qwording$name[i]

  # counts
  var.tab <- crtabs(paste0("~ ", alias), ds, useNA = "ifany")
  var.arr <- var.tab@arrays$.unweighted_counts
  if (class(var.arr) == "array") var.arr <- matrix(var.arr, nrow = 1)
  
  # choices
  cat.obj <- meta[[name]]@body$categories
  choicenames.vec <- sapply(cat.obj, "[", "name") %>% unlist() %>% as.character()
  choiceno.vec <- sapply(cat.obj, "[", "id") %>% unlist() %>% as.integer()
  
  
  # if a grid, get alias for each question
  if (nQs > 1) {
    subcat.obj <- meta[[name]]@body$subreferences
    q.aliases <- sapply(subcat.obj, "[", "alias") %>% unlist() %>% as.character()
    q.names <- sapply(subcat.obj, "[", "name") %>% unlist() %>% as.character()
  } else {
    q.aliases <- alias
    q.names <- name
  }
  
  
  
  # for each question
  for (j in 1:nQs) {
    cat(paste("Question i = ", i, "; SubQ j = ", j, "\n"))
    
    # the tabs
    simp.tab <- tibble(`Unweighted N` = var.arr[j, ],
                       `num` = choiceno.vec,
                       `Choice Text` = choicenames.vec)
    
    # the quesiton
    addtorow <- list()
    addtorow$pos <- list(-1, -1)
    
    qcodetext <- paste0('\\multicolumn{3}{l}{', gsub("\\_", "\\\\_", q.aliases[j]), '}')
    qwordtext <- paste0('\\multicolumn{3}{l}{', qwording$wording[i], " ", q.names[j], '}')
    
    addtorow$command <- c(paste0(qcodetext, "\\\\"),
                          paste0(qwordtext, "\\\\"))
    
    
    # format
    simp.xtab <- xtable(simp.tab, display = c("d", "d", "d", "s"))
    filename <- paste0(formatC(i, width = 3, format = "d", flag = "0"), "_",  q.aliases[j], ".tex")
    tabs.list[[filename]] <- simp.xtab
    
    
    # write
    if (writeToFile) {
      print(simp.xtab, 
            include.rownames = FALSE,
            add.to.row =  addtorow,
            timestamp = NULL,
            file = file.path(wd, "data/output/meta/tabs/", filename))
    }    
  }
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

