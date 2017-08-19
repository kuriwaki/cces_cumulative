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
