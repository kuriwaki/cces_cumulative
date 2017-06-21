rm(list = ls())
# load necessary package 
library(readr)
library(dplyr)
library(haven)
library(data.table)

setwd("~/Dropbox/cces_cumulative") # set your directory to Dropbox/cces_cumulative

## code that associates FIPS codes with state names
statecode <- read.csv("data/source/statecode.csv", as.is = T)


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
            skip = 2, col_names = c("state", "zipcode", "distnum"))

national113

national115 <-read_delim("data/source/census/115/zcta_cd115_natl.txt" , 
           delim = "," ,
           skip = 2, col_names = c("state", "zipcode", "distnum"))

national115



# sometimes zipcodes straddle districts! This will be a pain to account for by just merging, let's somehow manipulate the dataset so that each row is a unique zipcode.

# group the dataset into state-zipcodes. Then, let's just list up the distnums that are _within each group_
n109_byzip <- national109 %>%  
  group_by(state, zipcode) %>%  # these arrows combined with a percent sign (%>%) are "pipes". It says, take the output on the left and pass it down to the next function (in this case, the thing in the next row)
  summarize(distnums = paste0(distnum, collapse = ",")) # let's summarize each group. How? We're going to collapse the 1 or more distnums into one character. That's the function paste().


n110_byzip <- national110 %>%
  group_by(state, zipcode) %>%
  summarize(distnums = paste0(distnum, collapse = ","))

n113_byzip <- national113 %>%
  group_by(state, zipcode) %>%
  summarize(distnums = paste0(distnum, collapse = ","))

n115_byzip <- national115 %>%
  group_by(state, zipcode) %>%
  summarize(distnums = paste0(distnum, collapse = ","))

n109110_byzip <- left_join(n109_byzip, n110_byzip, by = c("zipcode", "state"))

n113115_byzip <- left_join(n113_byzip, n115_byzip, by = c("zipcode", "state"))

n109_115_byzip <- left_join(n109110_byzip, n113115_byzip, by = c("zipcode", "state")) %>%
  ungroup()
 
colnames(n109_115_byzip) <- c("state", "zipcode", "distnum109", "distnum110", "distnum113", "distnum115")

n109_115_byzip

# add state name
n109_115_byzip <- left_join(mutate(n109_115_byzip, state = as.integer(state)),
                            dplyr::select(statecode, StateAbbr, fips),
                            by = c("state" = "fips")) %>%
  dplyr::select(StateAbbr, zipcode, contains("distnum"))


## Make a key of districts ------------

all_CDs <- bind_rows(national109, national110,
                     national113, national115) %>% 
  distinct(state, distnum) %>% 
  mutate(state = as.integer(state)) %>%
  left_join(statecode, by = c("state" = "fips")) %>%
  mutate(CD = paste0(StateAbbr, "-", distnum)) %>% 
  distinct(CD) %>%
  pull(CD)


all_CDs


# build container (empty),
container <- tibble(CD = all_CDs,
                    zips109 = NA,
                    zips110 = NA,
                    zips113 = NA,
                    zips115 = NA)



## Loop through and store zipcodes for a given district ----


# pseudocode. stuff in capital letters is not real code and should be coded up into proper functions

d <- "CA-24"
state_of_d <- gsub(pattern = "-[0-9]+", replacement = "",  x = d)

d <- "AL-04"
state_of_d <- gsub(pattern = "-[0-9]+", replacement = "",  x = d)

d <- 04
distnum_of_d <- gsub(pattern = "-[A-Z]+", replacement = "",  x = d)

d <- 24
distnum_of_d <- gsub(pattern = "-[A-Z]+", replacement = "",  x = d)



for (d in all_CDs) {
  for (c in 109:115) {
   
    # what is the state of district d? extract from d
    state_of_d  <- AL
    
    # what is the distnum of district d? extract from d?
    distnum_of_d <- 01
    
    # pull out all the zipcodes associated with district d in congress c
    zips_in_d_at_c <- n109_115_byzip %>% 
      filter(StateAbbr == state_of_d &  109distnumC %in% distnum_of_d01) %>%
      pull(zipcode)
     
    # store those districts -- put them in a container
    STORE zips_ind_at_c INTO container[ROW d, COLUMN c] 
  }
}




