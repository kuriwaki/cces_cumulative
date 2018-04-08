library(crunch)
library(dplyr)
library(haven)


login()


ds16 <- loadDataset("CCES Cumulative Common 2016") # old dataset, 2006 - 2016
ds17 <- loadDataset("CCES Cumulative Common 2017") # new dataset, only 2017 

ds16_17 <- appendDataset(ds16, ds17)
compareDatasets(ds16, ds17)

# something wrong with variable number 6?
ds16[[6]]
ds17[[6]]
