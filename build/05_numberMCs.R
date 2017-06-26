library(readxl)
library(dplyr)

vv <- read_excel("C:/Users/Emma/Desktop/All the Things/Harvard/CCES/Member ID.xlsx")
cq <- read_excel("C:/Users/Emma/Desktop/All the Things/Harvard/CCES/Members of Congress.xlsx")

cq_house <- filter(cq, Position == "U.S. Representative")
vv_house <- filter(vv, chamber == "House")

cq_house <- mutate(cq_house, District = replace(District, District == "AL", 0))
cq_house<- mutate(cq_house, District = as.numeric(District))

cq_vv_house <- inner_join(cq_house, vv_house, by = c("State" = "state_abbrev", "District" = "district_code"))
