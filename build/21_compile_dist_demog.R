rm(list = ls())
library(readr)
library(dplyr)
library(haven)
library(stringr)


path <- "data/source/census_demog/ACS_5yr_2006_10/R11430181.dta"
acs_06_10 <- read_dta(path)

path1 <- "data/source/census_demog/ACS_5yr_2011_15/R11430939.dta"
acs_11_15 <- read_dta(path1)

path2 <- "data/source/census_demog/census_2010/R11430177_SL500.dta"
cen_10 <- read_dta(path2)


# table of variable and their labels

# function to take a dta from read_dta and return a variable - description key
makeVlist <- function(dta) {
  labels <- sapply(dta, function(x) attr(x, "label"))

  tibble(
    name = names(labels),
    label = labels
  )
}


# apply the function
v_acs_06 <- makeVlist(acs_06_10)
v_acs_11 <- makeVlist(acs_11_15)
v_cen_10 <- makeVlist(cen_10)


Variable06 <- "source"
v_acs_06[, Variable06] <- "ACS06"

Variable11 <- "source"
v_acs_11[, Variable11] <- "ACS111"

Variable10 <- "source"
v_cen_10[, Variable10] <- "CEN10"

# one row per variable
combined_dataset <- bind_rows(v_acs_06, v_acs_11, v_cen_10)
