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
demcode111 <- read_tsv("data/source/censusCD_demographics/111/R11430177_SL500.txt")

demcode_06_10 <- read_tsv("data/source/censusCD_demographics/ASC(5yr_2006_10)/R11430181_SL500.txt")

demcode_11_15 <- read_tsv("data/source/censusCD_demographics/ASC(5yr_2011_15)/R11430939_SL500.txt")