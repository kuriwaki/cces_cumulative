library(tidyverse)
library(haven)
library(glue)



# apppend the candidatename-candidate party variables to the vote choice qs

#' append labels to numeric responses
#' 
#' @param numdf the slim df with choices as numbers
#' @param canddf the df with all the candidate info
#' 
#' @return A dataset with nrow(numdf) rows that has all the candidate info appended
#' for the candidate the respondent chose

num_cand_match <- function(numdf, canddf) {
  type <- grep("_num", colnames(numdf), value = TRUE)
  chosen_varname <- glue("{gsub('_num', '', type)}_chosen")
  abstract_varname <- as.character(glue("{gsub('_num', '', type)}_char"))
  
  canddf <- rename(canddf, !!type := cand)
  
  joined <- left_join(numdf, canddf, by = c("year", "caseID", type))
  
  joined %>% 
    mutate(!!abstract_varname := std_voteopts(.data[[abstract_varname]])) %>%
    mutate(!!chosen_varname := str_c(name, " (", party, ")", sep = "")) %>% 
    select(!!c(colnames(numdf), chosen_varname), everything())
}

#' Standardized the votechoice options for crunch
#' 
#' @param vec the vector to recode
#' @param chr1 text to show for first option
#' @param chr2 text to show for second option
#' 
#' @return A recoded vector
std_voteopts <- function(vec, 
                         chr1 = "[Democrat / Candidate 1]",
                         chr2 = "[Republican / Candidate 2]") {
  recode(vec,
         `$Housecand1name ($Housecand1party)` = chr1,
         `Democratic Candidate` = chr1,
         `$Housecand2name ($Housecand2party)` = chr2,
         `Republican Candidate` = chr2)
  
}
#' Slim out the data for crunch, and preparing for crunch match
#' 
#' @tbl The dataset to slim out
slim <- function(tbl, varmarker = '_chosen', id = "fec") {
  
  chosen_var <- grep(varmarker, colnames(tbl), value = TRUE)
  type <- gsub(varmarker, '', chosen_var)
  id_rename <- as.character(glue("{type}_{id}"))
  
  tbl %>% 
    select(!!c("year", "caseID"),
           matches(as.character(glue("{varmarker}$"))),  
           !!id_rename := !!id)
}

# Data -------------
load("data/output/01_responses/vote_responses.RData")
load("data/output/01_responses/incumbents_key.RData")
load("data/output/01_responses/candidates_key.RData")
ccc <- readRDS("data/output/01_responses/cumulative_stacked.Rds")
rmaster <- readRDS("data/output/01_responses/repsondent_contextual.Rds")



# add on name and fec, standardized option labels -----
i_hou_who <- num_cand_match(i_rep, hc_fec_match)
i_sen_who <- num_cand_match(i_sen, sc_fec_match)
i_gov_who <- num_cand_match(i_gov, gc_fec_match)

v_hou_who <- num_cand_match(v_rep, hc_fec_match)
v_sen_who <- num_cand_match(v_sen, sc_fec_match)
v_gov_who <- num_cand_match(v_gov, gc_fec_match)


# create a separate dataset for chosen vars ------ 
ids <- c("year", "caseID")

chosen_with_fec <-  slim(i_hou_who) %>% 
  left_join(slim(i_sen_who), ids) %>% 
  left_join(slim(i_gov_who), ids) %>% 
  left_join(slim(v_hou_who), ids) %>% 
  left_join(slim(v_sen_who), ids) %>% 
  left_join(slim(v_gov_who), ids)

# now we can wrap up the abstract labels 

#' combined char to number

bind_label <- function(tbl) {
  
  
}


# nice dataset for incumbents ? ----

incumbents_with_ID <-  slim(hi_mc_match, "_inc", "icpsr") %>% 
  left_join(slim(s1i_mc_match, "_inc", "icpsr"), ids) %>% 
  left_join(slim(s2i_mc_match, "_inc", "icpsr"), ids) %>% 
  left_join(slim(gov_inc_match, "_inc"), ids)
  


# merge in. 

select(i_hou_who, 1:7)




# Format for output  --------
# make char variables a factor so crunch knows it's a categorical?
ccc_factor <- ccc %>%
  mutate(caseID = as.character(caseID)) %>% # better this than let crunch think its a numeric
  mutate(zipcode = as.character(zipcode)) %>%
  mutate(cdid = as.factor(cdid)) %>% # we don't want to take summary stats of this, so better a factor
  mutate(countyFIPS = str_pad(as.character(countyFIPS), width = 5, pad = "0")) %>%
  mutate_at(vars(matches("_char")), as.factor) %>%
  mutate_at(vars(matches("^CD$")), as.factor) %>%
  mutate_at(vars(matches("(state$|st$)")), as.factor)


write_sav(ccc_factor, "data/release/cumulative_2006_2016.sav")
write_dta(ccc_factor, "data/release/cumulative_2006_2016.dta")
