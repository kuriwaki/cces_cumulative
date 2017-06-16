library(stargazer)
library(dplyr)
library(haven)
library(readr)
library(readxl)
library(data.table)


# Read metadata ----
cq <- readRDS("data/output/meta/fmt_metadata_cc16.Rds") %>%
  mutate(aliasAbbrv = gsub("grid", "", alias))

sq <- read_excel("data/output/meta/Labels_2016.xlsx") %>% 
  select(Name, Label) %>%
  mutate(sOrder = 1:n())

# nathan and liz
nl <- read_csv("data/source/2016_guidebook_variables_orderedby2014.csv")


# Join  crunch and stata
cs <- left_join(cq, sq, by = c("alias" = "Name"))



# compare with 2014  ------
nl_key <- select(nl, rowID, code14, code16)


# how much of code16 acutally matches to crunch?
anti_join(nl_key, sq, by = c("code16" = "Name"))

# anti_join(cq, nl_key, by = c("alias"  = "code16"))



saveRDS(cs, "data/output/meta/fmt_metadata_cc16_stata.Rds")
write_csv(cs, "data/output/meta/fmt_metadata_cc16_withstata.csv",na = "")
