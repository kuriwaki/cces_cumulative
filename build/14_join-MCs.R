library(plyr)
library(dplyr)

#join Voteview and FEC list 
VVwithFEC <- left_join(voteviewlisttotal, minigoodchartfinal, by= c("icpsr"="ICPSR"))

#adjust cq data to make for a clean join with VVwithFEC
df109to115 <- mutate(df109to115, Last = toupper(Last))
df109to115 <- mutate(df109to115,Position=replace(Position,Position=="U.S. Representative","House"))
df109to115 <- mutate(df109to115, Position=replace(Position,Position=="U.S. Senator", "Senate"))

#final join
VVwithFECandCQMETA <- left_join(df109to115,VVwithFEC,by=c("Last"="lastname","Position"="chamber","State" = "state_abbrev","Congress"="congress"))



# Reference from sK
# library(readxl)
# library(dplyr)
# 
# vv <- read_excel("C:/Users/Emma/Desktop/All the Things/Harvard/CCES/Member ID.xlsx")
# cq <- read_excel("C:/Users/Emma/Desktop/All the Things/Harvard/CCES/Members of Congress.xlsx")
# 
# cq_house <- filter(cq, Position == "U.S. Representative")
# vv_house <- filter(vv, chamber == "House")
# 
# cq_house <- mutate(cq_house, District = replace(District, District == "AL", 0))
# cq_house<- mutate(cq_house, District = as.numeric(District))
# 
# cq_vv_house <- inner_join(cq_house, vv_house, by = c("State" = "state_abbrev", "District" = "district_code"))
