library(crunch)
library(ggplot2)
library(dplyr)
library(foreach)
library(readr)
library(xtable)

ccc <- read_sav("data/output/cumulative_2006_2016.sav")

# Start up crunch -------
login() # you need a login and password to complete this command


# connect to data---------
proj <- projects()[["CCES Common Content"]]

ds <- loadDataset(datasets(proj)[[1]])


# Manipulate data 
# apply weights
weight(ds) <- ds$weight_cumulative





logout()