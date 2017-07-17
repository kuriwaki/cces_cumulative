rm(list = ls())
# load necessary package 
library(readr)
library(dplyr)
library(haven)
library(stringr)
library(ggplot2)
library(scales)

setwd("~/Dropbox/cces_cumulative")

help(package = readr)
help(package = haven)


path <-"data/source/censusCD_demographics/ACS_5yr_2006_10/R11430181.dta"
acs_06_10 <- read_dta(path)

path1 <- "data/source/censusCD_demographics/ACS_5yr_2011_15/R11430939.dta"
acs_11_15 <- read_dta(path1)

path2 <- "data/source/censusCD_demographics/census_2010/R11430177_SL500.dta"
cen_10 <- read_dta(path2)


# table of variable and their labels

# function to extract
makeVlist <- function(dta) {
  labels <- sapply(dta, function(x) attr(x, "label"))
  
  tibble(colname = names(labels),
         description = labels)
}


# apply the function
v_acs_06 <- makeVlist(acs_06_10)
v_acs_11 <- makeVlist(acs_11_15)
v_cen_10 <- makeVlist(cen_10)


# see for example 
View(v_cen_10)