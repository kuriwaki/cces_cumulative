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



# Join 
cs <- left_join(cq, sq, by = c("alias" = "Name"))


saveRDS(cs, "data/output/meta/fmt_metadata_cc16_stata.Rds")
write_csv(cs, "data/output/meta/fmt_metadata_cc16_withstata.csv",na = "")
