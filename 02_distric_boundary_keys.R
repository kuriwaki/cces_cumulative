library(dplyr)
library(haven)
library(readr)
library(data.table)


# Read data ----
person <- readRDS("data/source/person.Rds") # from SK's compilation in different project

# this is a person-level dataset that stacks all CCES common contents from 2006- 2016. 
# This is the strucutre of what we want to eventually make. But, it needs more variables.
person



# get to keys
zy_key <- person %>%
  filter(zipcode != "." & zipcode != "") %>%
  distinct(year, cong, zipcode, StateAbbr, CD) %>%
  arrange(StateAbbr, zipcode, year) %>%
  mutate(CDnum = as.numeric(gsub("[A-Z\\-]+", "", CD))) %>%
  select(zipcode, year, cong, StateAbbr, CDnum, CD, everything())


# long to wide

# thi is long
zy_key


# use this command to see how dcast works
?dcast



# do these two have the same row count
zy_key

zy_key %>%
  distinct(zipcode, year) 



# let's turn this into wide
zy.wide <- dcast(data = as.data.table(zy_key),
                 formula = zipcode ~ year,
                 value.var = "CD",
                 fun.aggregate = function(cds) cds[1] 
                 )

# fun.aggregate = function(cds) cds[1]  this is a very subpar compromise -- when
# there are multiple CDs in a zipcode-year, we're taking the first one


# easier to see
zy.wide <- tbl_df(zy.wide)


# checking duplicates. How many CDs are in a zipcode-year? Preferably one. 
ZYCdupes <-  zy_key %>%
  group_by(zipcode, year) %>%
  summarize(nZYCs = n()) %>%
  arrange(-nZYCs) %>%
  ungroup()


