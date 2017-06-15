rm(list = ls())

library(haven)
library(dplyr)
library(readr)
library(foreach)


# rudimentary standardization for data that comes out of dataverse
std_dv <- function(path, guess_year = TRUE) {
  
  if (guess_year) guessed_yr <- as.numeric(gsub(".*/([0-9]+)_cc.*", "\\1", path))
  
  ## then
  tbl <- haven::read_dta(path)
  
  
  ## guess ID
  if ("caseid" %in% colnames(tbl)) orig_key <- "caseid"
  if ("V101" %in% colnames(tbl)) orig_key <- "V101"
  

  # add year
  if (!"year" %in% colnames(tbl)) tbl <- mutate(tbl, year = guessed_yr)
    
  
  # then rename id
  tbl %>%
    rename(caseID = !!orig_key) %>%
    dplyr::select(year, caseID, everything())
  
}

# takes all datasets available, and given a var, pulls it out of each and stacks
findStack <- function(dflist = list(), var) {
  var <- enquo(var)
  
  chr_var_name <- paste0(quo_name(var), "_char")
  num_var_name <- paste0(quo_name(var), "_num")
  
  foreach(y = 1:length(dflist), .combine = "bind_rows") %do% {
    cat(paste0("df ", y, ", "))
    
    dplyr::select(dflist[[y]], year, caseID, !!var) %>%
      mutate(!!chr_var_name := as.character(as_factor(!!var)),
             !!num_var_name := as.numeric(!!var)) %>% 
      dplyr::select(-!!var)
  }
}




# READ ------

# 2012 and before (compiled by Stephen Pettigrew and others)
ccp <- std_dv("data/source/cces/2006_2012_cumulative.dta", 
                    guess_year = FALSE)



# take the needed columns for 2013- 2016
cc13 <- std_dv("data/source/cces/2013_cc.dta")
cc14 <- std_dv("data/source/cces/2014_cc.dta")
cc15 <- std_dv("data/source/cces/2015_cc.dta")
cc16 <- std_dv("data/source/cces/2016_cc.dta")




# Start extracting variables -----
# in list form
ccs <- list(ccp, cc13, cc14, cc15, cc16)


# first same name vars -----
pid3 <- findStack(ccs, pid3)
pid7 <- findStack(ccs, pid7)
gend <- findStack(ccs, gender)
educ <- findStack(ccs, educ)
race <- findStack(ccs, race)
bryr <- findStack(ccs, birthyr)



# bind together ----
ccc <- left_join(pid3, pid7) %>% 
  left_join(gend) %>%
  left_join(bryr) %>%
  left_join(race) %>%
  left_join(educ)
  



# Write dta 
write_dta(ccc, "data/output/cumulative_2006_2012.dta")
write_csv(ccc, "data/output/cumulative_2006_2012.csv")
saveRDS(ccc, "data/output/cumulative_2006_2012.Rds")
