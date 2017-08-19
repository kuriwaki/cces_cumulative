#badchart is original long list of all dime recipients from 1979-2014
#goodchart is badchart with only candidates
#minigoodchart is goodchart with only relevant columns
#minigoodchart 2 is minigoodchart without blank rows
#minigoodchartfinal is minigoodchart 2 without duplicate rows
badchart <- read.csv("~/Dropbox/cces_cumulative/data/source/dime/dime_recipients_all_1979_2014.csv")
badchart[!grepl("comm",badchart$bonica.rid),] -> goodchart
minigoodchart <- data_frame(FEC = goodchart$FEC.ID, ICPSR = goodchart$ICPSR2, seat = goodchart$seat)
minigoodchart2 <- minigoodchart[!is.na(minigoodchart$FEC),]
minigoodchartfinal <- distinct(minigoodchart2,FEC,ICPSR, seat)
#strip away letters from icpsr
minigoodchartfinal$ICPSR <- gsub("[A-z]+","",minigoodchartfinal$ICPSR)
minigoodchartfinal$ICPSR <- as.numeric(minigoodchartfinal$ICPSR)