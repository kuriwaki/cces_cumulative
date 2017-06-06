
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

names(AK109) <- names(AL109)
names(AZ109) <- names(AL109)

   ALAKAZ <-rbind(AL109, AK109, AZ109)

ALAKAZ

states.list



# AL113  <- read_delim("data/source/census/113/zc_cd_01_fmt.txt" ,
#                      delim = "\t",
#                      col_names = FALSE)
# AL113




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
