rm(list = ls())
# load necessary package 
library(readr)
library(dplyr)
library(haven)
library(stringr)
library(ggplot2)
library(scales)
library(DT)

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

# function to take a dta from read_dta and return a variable - description key
makeVlist <- function(dta) {
  labels <- sapply(dta, function(x) attr(x, "label"))
  
  tibble(name = names(labels),
         label = labels)
}


# apply the function
v_acs_06 <- makeVlist(acs_06_10)
v_acs_11 <- makeVlist(acs_11_15)
v_cen_10 <- makeVlist(cen_10)


Variable06 <- "source"
v_acs_06[, Variable06] <- "ACS06"
#View(v_acs_06)

Variable11 <- "source"
v_acs_11[, Variable11] <- "ACS111"
#View(v_acs_11)

Variable10 <- "source"
v_cen_10[, Variable10] <- "CEN10"
#View(v_cen_10)

combined_dataset <- bind_rows(v_acs_06, v_acs_11, v_cen_10)

help("write_dta")

# write ------
write_dta(combined_dataset,"data/output/CD_dem/CD_dem.dta")
write_csv(combined_dataset, "data/output/CD_dem/CD_dem.csv")

datatable(combined_dataset,
          filter = list(position = 'top', clear = FALSE),
          options = list(searchCols = list(NULL, NULL, NULL))
)

