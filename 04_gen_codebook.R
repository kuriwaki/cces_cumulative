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
  rename(stataName = Name) %>%
  mutate(sOrder = 1:n())

# nathan and liz
nl <- read_csv("data/source/2016_guidebook_variables_orderedby2014.csv")


# inner join from stata to nl to get order
sq_ordered <- inner_join(sq, nl, by = c("Name" = "code16")) %>% 
  arrange(rowID)

sq_ordered


# what are the stuff in sq that are not in crunch?

anti_join(sq_ordered, cq, by = c("Name" = "alias")) %>%
   select(section14, Name, Label, sOrder, rowID, everything()) %>% 
  filter(!grepl("Contextual", section14))





saveRDS(cs, "data/output/meta/fmt_metadata_cc16_stata.Rds")
write_csv(cs, "data/output/meta/fmt_metadata_cc16_withstata.csv",na = "")
