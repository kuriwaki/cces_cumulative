rm(list = ls())
# load necessary package 
library(readr)
library(dplyr)
library(haven)
library(data.table)


# REad in data -----------------
national109 <-read_delim("data/source/census/109/zcta_cd109_natl.txt" ,
           delim = "," ,
           skip = 2, col_names = c("state", "zipcode", "distnum"))

national109


national110 <-read_delim("data/source/census/110/zcta_cd110_natl.txt" , 
          delim = "," ,
          skip = 2, col_names = c("state", "zipcode", "distnum"))

national110


national113 <-read_delim("data/source/census/113/zcta_cd113_natl.txt" , 
            delim = "," ,
            skip = 2, col_names = FALSE)

national113

national115 <-read_delim("data/source/census/115/zcta_cd115_natl.txt" , 
           delim = "," ,
           skip = 2, col_names = FALSE)

national115



# sometimes zipcodes straddle districts! This will be a pain to account for by just merging, let's somehow manipulate the dataset so that each row is a unique zipcode.

# group the dataset into state-zipcodes. Then, let's just list up the distnums that are _within each group_
n109_byzip <- national109 %>%  
  group_by(state, zipcode) %>%  # these arrows combined with a percent sign (%>%) are "pipes". It says, take the output on the left and pass it down to the next function (in this case, the thing in the next row)
  summarize(distnums = paste0(distnum, collapse = ",")) # let's summarize each group. How? We're going to collapse the 1 or more distnums into one character. That's the function paste().


# run these examples to get a more concrete sense of what we are doing
dist <- c("01", "02", "03", "04")
paste0(dist)
paste0(dist, collapse = ",")

national109 %>% group_by(zipcode)
national109 %>% group_by(state, zipcode)



##### trying to figure out how to remove NA cells from the data set (top)

x <- na.omit(n109.wide)
x

x<- n109.wide[complete.cases(n109.wide), ]
x <- na.omit(n109.wide)

                   
complete.cases(n109.wide)



new_dataframe = as.data.frame(lapply(n109.wide, na.omit))


func<-function(i){
  x<-as.numeric(as.character(n109.wide[,i][!is.na(n109.wide[,i])]))
  xna<-as.numeric(as.character(n109.wide[,i][is.na(n109.wide[,i])]))
  newx<-c(x,xna)
}

do.call(cbind,lapply(1:length(df[1,]),func))

                 
##### trying to figure out how to remove NA cells from the data set (bottom)
                   
                  






c109110<-left_join(national109, national110, by = c("zipcode", "state"))

c109110

help ("left_join")


View(datasetcombined)

left_join(national109, national110, national113, national115, by = c("zip"))







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



# temporary example -- a toy example with fake data, but essentially the same thing
dataset1 <- tibble(key = c("A", "B", "C", "D"),
                   code1 = c(1, 2, 3, 4))

dataset2 <- tibble(key = c("C", "B", "A"),
                   code2 = c(103, 102, 101))

# what does dataset1 look like?
dataset1

# what does dataset2 look like?
dataset2

# NOW, how do we combine these two things so that we can summarize our info into one table?
datasetcombined <- left_join(dataset1, dataset2, by = c("key"))

# what does datasetcombined look like?
datasetcombined






