rm(list = ls())
# load necessary package 
library(readr)
library(dplyr)
library(haven)
library(stringr)


setwd("~/Dropbox/cces_cumulative") # set your directory to Dropbox/cces_cumulative

## code that associates FIPS codes with state names
statecode <- read.csv("data/source/statecode.csv", as.is = T)


# Read in data -----------------
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
  arrange(CD) %>%
  pull(CD)


all_CDs


# build container (empty),
container <- tibble(CD = all_CDs,
                    zips109 = NA,
                    zips110 = NA,
                    zips113 = NA,
                    zips115 = NA)



## Loop through and store zipcodes for a given district ----




##loop##

for (d in all_CDs) {
  for (c in c(109, 110, 113, 115)) {
    

  
    column_to_search <- paste(c("zips", some form of distnum that can dynamically pick c) , collapse = "")
    
    state_of_d <- gsub(pattern = "-[0-9]+", replacement = "",  x = d)  

    distnum_of_d <- gsub(pattern = "[-A-Z]+", replacement = "", x = d)
    
    # pull out the zips
    zips_in_d_at_c <- n109_115_byzip %>% 
      filter(StateAbbr == state_of_d & grepl(distnum_of_d, .data[[column_to_search]])) %>%
      pull(zipcode)

   
    # figure out which column to place our zip codes in
    col_name_in_container <- paste(c("zips", c) , collapse = "")
    
   # figure out which row corresponds to district d
    row_number_in_container <- which(d == container$CD)
    
  
    container[row_number_in_container,col_name_in_container] <- paste(zips_in_d_at_c, collapse = ",")
   

  }
  
  #cat(paste0(d, "\n"))
}

#build new container
containerz <- tibble(CD = all_CDs,
                    zips_calculation109110 = NA,
                    zips_calculation110113 = NA,
                    zips_calculation113115 = NA)





library(dplyr)


for (d in all_CDs) {
  for (c in c(109110, 110113, 113115)) {

  row_number_in_container <- which(d == container$CD)
  

  #### notes
  c_start <- substr(c, start = 1, stop = 3)
    
  c_end <- substr(c, start = 4, stop = 6)
  
  
  #### notes
  
  
  column_name_container_pre <- paste(c("zips", c_start) , collapse = "")
  column_name_container_post <- paste(c("zips", c_end) , collapse = "")
  

  zips_of_d_pre <- as.character(container[row_number_in_container, column_name_container_pre])
  zips_of_d_post <- as.character(container[row_number_in_container, column_name_container_post])

  zips_of_d_presplit <- str_split(zips_of_d_pre, ",")[[1]]


  zips_of_d_postsplit <- str_split(zips_of_d_post, ",")[[1]]

  inzips_of_d_pre_but_not_zips_of_d_post <- setdiff(x = zips_of_d_presplit, y = zips_of_d_postsplit)

  inzips_of_d_post_but_not_zips_of_d_pre <- setdiff(x = zips_of_d_postsplit, y = zips_of_d_presplit)

  inzips_of_d_pre_and_inzips_of_d_post <- intersect(zips_of_d_presplit, zips_of_d_postsplit)


  count_of_zips_of_d_pre <- length(zips_of_d_presplit)
  count_of_zips_of_d_post <- length(zips_of_d_postsplit)
  count_in_zips_of_d_pre_but_not_zips_of_d_post <- length(inzips_of_d_pre_but_not_zips_of_d_post)


  count_in_zips_of_d_post_but_not_zips_of_d_pre <- length(inzips_of_d_post_but_not_zips_of_d_pre)


  count_ofbothprepost <- length(inzips_of_d_pre_and_inzips_of_d_post)

  (count_ofbothprepost / count_of_zips_of_d_pre) %>% cat(paste0(.,"\n"))
  col_name_in_containerz <- paste(c("zips_calculation", c) , collapse = "")
  
  # figure out which row corresponds to district d
  row_number_in_containerz <- which(d == container$CD)
  
  containerz[row_number_in_containerz,col_name_in_containerz] <- paste(count_ofbothprepost / count_of_zips_of_d_pre, collapse = ",")
  
  }
}



























(count_inAL06_109_but_not_AL06_110 + count_inAL06_110_but_not_AL06_109) / count_ofAL06_109











###example

library(stringr)


# we start with this
zA <- "02138, 02139, 02140"
zB <- "02138, 02139, 02141, 02142"

# split up into a vector where each item is a zipcode -- opposite of paste(..., collapse = ",")
zAsplit <- str_split(zA, ",")[[1]]
zBsplit <- str_split(zB, ",")[[1]]


# compare the two -- find that zips that are different
inA_but_notB <- setdiff(x = zAsplit, y = zBsplit)
inB_but_notA <- setdiff(x = zBsplit, y = zAsplit)

# what about zips that are the same?
inA_and_inB <- intersect(zAsplit, zBsplit)


# count the number of zips that satisfy a certain condition
count_ofA <- length(zAsplit)
count_ofB <- length(zBsplit)
count_inA_but_notB <- length(inA_but_notB)
count_inB_but_notA <- length(inB_but_notA)
count_ofboth <- length(inA_and_inB)


# what are some ratios that would be useful?
count_ofboth / count_ofA
(count_inA_but_notB + count_inB_but_notA) / count_ofA

###example

container %>% pull(zips109)







