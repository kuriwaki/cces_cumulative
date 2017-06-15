rm(list = ls())
# load necessary package 
library(readr)
library(dplyr)
library(haven)
library(data.table)

## code that associates FIPS codes with state names
statecode <- read.csv("~/Dropbox/cces_rollcall/data/source/statecode.csv", as.is = T)


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

n109_115_byzip <- left_join(n109110_byzip, n113115_byzip, by = c("zipcode", "state"))
 
colnames(n109_115_byzip) <- c("state", "zipcode", "distnum109", "distnum110", "distnum113", "distnum115")

n109_115_byzip




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


## Loop through and store zipcodes for a given district ----


for (d in all_CDs) {
  for (c in 109:115) {
   
    zips_in_d_at_c <- n109_115_byzip %>% 
      filter(state == state(d) &  distnumC %in% distnum(d)) %>%
      pull(zipcode)
     
    store zips_ind_at_c INTO container[row d, column c] 
  }
}




