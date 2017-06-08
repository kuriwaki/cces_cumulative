
# load necessary package 
library(readr)
library(dplyr)
library(haven)

# test right
AL109 <-  read_delim("data/source/census/109/zcta_c9_01.txt",
           delim = " ",
           skip = 3)

AK109 <-  read_delim("data/source/census/109/zcta_c9_05.txt",
                     delim = " ",
                     skip = 3)


AZ109 <-  read_delim("data/source/census/109/zcta_c9_04.txt",
                     delim = " ",
                     skip = 3)

national109 <-read_delim("data/source/census/109/zcta_cd109_natl.txt" ,
           delim = "," ,
           skip = 2)

national109


national110 <-read_delim("data/source/census/109/zcta_cd110_natl.txt" , 
          delim = "," ,
          skip = 2)

national110


national113 <-read_delim("data/source/census/109/zcta_cd113_natl.txt" , 
            delim = "," ,
            skip = 2)

national113

national115 <-read_delim("data/source/census/109/zcta_cd115_natl.txt" , 
           delim = "," ,
           skip = 2)

national115













# automate the reading in 
filenames <- list.files("data/source/census/109",
                        full.names = TRUE)

# write a loop to go through each file in `filenames`

# create a place to store states
states.list <- list() # empty list

for (file.i in 1:length(filenames)) {
  states.list[[file.i]] <- read_delim(filenames[file.i],
                                      delim = " ",
                                      skip = 3)
}

states.list

library(plyr)

moststates <- rbind.fill.matrix(states.list)


moststates

#there only seems to be 43 states on the census page for the 109th congress
#check moststates cells 21262-21264 and cells 30473-30489




AL113  <- read.delim("data/source/census/113/zc_cd_01.txt" ,
                     sep = "",
                     skip = 3,
                     header = FALSE)
AL113





# stack the state files

# states109 <- bind_rows(AL109, AL109, AZ109)
# states109
# 
# 
# 
# 
# # export 
# write_dta(states109, "data/output/states109.dta")
# write_csv(state109, "data/output/states109.csv")
# saveRDS(states109, "data/output/states110.Rds")
