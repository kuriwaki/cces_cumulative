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


# pseudocode. stuff in capital letters is not real code and should be coded up into proper functions

d <- "CA-24"
state_of_d <- gsub(pattern = "-[0-9]+", replacement = "",  x = d)

d <- "AL-04"
state_of_d <- gsub(pattern = "-[0-9]+", replacement = "",  x = d)

d <- 04
distnum_of_d <- gsub(pattern = "-[A-Z]+", replacement = "",  x = d)

d <- "CA-24"
distnum_of_d <- gsub(pattern = "[-A-Z]+", replacement = "",  x = d)

d <- "AL-04"
distnum_of_d <- gsub(pattern = "[-A-Z]+", replacement = "", x = d)


# in the target table, what is the column we care about? it depends on c
column_to_search <- paste0("distnum", 109)


zips_in_d_at_c <- n109_115_byzip %>% 
  filter(StateAbbr == state_of_d & grepl(distnum_of_d, .data[[column_to_search]])) %>%
  pull(zipcode)





c <- 109
col_name_in_container <- paste(c("zips", c) , collapse = "")

d <- "AL-04"
row_number_in_container <- which(d == container$CD)




container[row_number_in_container,col_name_in_container] <- paste(zips_in_d_at_c, collapse = ",")


##loop##

for (d in all_CDs) {
  for (c in c(109, 110, 113, 115)) {
    
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
  
  cat(paste0(d, "\n"))
}

library(dplyr)

row_number_in_container <- which(d == container$CD)

zips_of_d_109 <- as.character(container[row_number_in_container, "zips109"])
zips_of_d_110 <- as.character(container[row_number_in_container, "zips110"])

zips_of_d_109split <- str_split(zips_of_d_109, ",")[[1]]


zips_of_d_110split <- str_split(zips_of_d_110, ",")[[1]]

inzips_of_d_109_but_not_zips_of_d_110 <- setdiff(x = zips_of_d_109split, y = zips_of_d_110split)
inzips_of_d_110_but_not_zips_of_d_109 <- setdiff(x = zips_of_d_110split, y = zips_of_d_109split)

inAL06_109_and_inAL06_110 <- intersect(AL06_109split, AL06_110split)


count_ofAL06_109 <- length(AL06_109split)
count_ofAL06_110 <- length(AL06_110split)
count_inAL06_109_but_not_AL06_110 <- length(inAL06_109_but_not_AL06_110)


count_inAL06_110_but_not_AL06_109 <- length(inAL06_110_but_not_AL06_109)


count_ofboth <- length(inAL06_109_and_inAL06_110)

count_ofboth / count_ofAL06_109
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







