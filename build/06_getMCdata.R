library(plyr)
setwd("~/Dropbox/cces_cumulative/documents_CQ/congress 114 metadata")
file_names = list.files(getwd())
files = lapply(file_names,read_xlsx)
colnames = c("Last","First","Middle","Suffix","Nickname","Born","Death","Sex","Position","Party","State","District","Start","End","Religion","Race","JobType1","JobType2","JobType3","JobType4","JobType5","Mil1","Mil2","Mil3")
files <- lapply(files, setNames, colnames)
df <- ldply(files, data.frame)
df<-df[!(df$Last=="Last"),]
df <- df[!is.na(df$First),]