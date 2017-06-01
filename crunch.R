rm(list = ls())
# Test crunch.io

library(ggplot2)
library(dplyr)




wd <- "~/Dropbox/cces_cumulative/"
setwd(wd)

# devtools::install_github("Crunch-io/rcrunch", build_vignettes=TRUE)


library(crunch)
login()
ds <- loadDataset("CCES 2016 Common")


crtabs(~ pid3 + presvote, ds)

variables(ds)


pp <- ds[ds$inputstate == "Kentucky", c("pid3", "presvote")]
variables(pp)

pp.df <- as.data.frame(pp, force = T)

str(pp.df)


?as.data.frame.CrunchDataFrame


ds$presvote

ols1 <- lm(I(presvote == "Donald Trump (Republican)") ~ pid3 + gender + age,
           data = ds)
summary(ols1)


