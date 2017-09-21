
# pull out some variables of interest pre-aggregation into 2008-2012 cumulative

library(dplyr)
library(haven)


cc06 <- read_dta("data/source/cces/2006_cc.dta")
cc07 <- read_dta("data/source/cces/2007_cc.dta")
load("data/source/cces/2008_cc.rdata"); cc08 <- tbl_df(x)
cc09 <- read_dta("data/source/cces/2009_cc.dta")
cc10 <- read_dta("data/source/cces/2010_cc.dta")
cc11 <- read_dta("data/source/cces/2011_cc.dta")