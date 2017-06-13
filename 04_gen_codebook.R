library(stargazer)
library(dplyr)
library(haven)
library(readr)
library(data.table)


# Read data ----
person <- readRDS("data/source/person.Rds") # from SK's compilation in different project



person



stargazer(as.data.frame(person))
