library(tidyverse)
library(crunch)
library(dplyr)
library(haven)


login()


if(FALSE){
  
  ds16 <- loadDataset("CCES Cumulative Common 2016") # old dataset, 2006 - 2016
  ds17 <- loadDataset("CCES Cumulative Common 2017") # new dataset, only 2017 
  
  ds16_17 <- appendDataset(ds16, ds17)
  compareDatasets(ds16, ds17)
  
  # something wrong with variable number 6?
  ds16[[6]]
  ds17[[6]]
  
}

# fix 2016 cumulative

# upload 2016
cc16 <- read_dta("~/Dropbox/CCES_SDA/2016/data/Common/CCES16_Common_OUTPUT_Feb2018_VV.dta") %>% 
  select(V101, matches("weight"), matches("^CL"))
write_sav(cc16, "data/output/01_responses/cc16_temp.sav")
newDataset("https://www.dropbox.com/s/fnv175o976rzxil/cc16_temp.sav?dl=0", "CCES 2016 Jan 2018")

insert <- loadDataset("CCES 2016 Jan 2018")



old16 <- loadDataset("CCES 2016 Common Vote Validated", project = "CCES")
old16_fork <- forkDataset(old16)
