library(tidyverse)
library(haven)
library(glue)



# Data -------------
load("data/output/01_responses/vote_responses.RData")
load("data/output/01_responses/incumbents_key.RData")
load("data/output/01_responses/candidates_key.RData")
ccc <- readRDS("data/output/01_responses/cumulative_stacked.Rds")
rmaster <- readRDS("data/output/01_responses/repsondent_contextual.Rds")




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
  
  show_varname <- glue("{gsub('_num', '', type)}_shown")
  abstract_varname <- as.character(glue("{gsub('_num', '', type)}_char"))
  
  canddf <- rename(canddf, !!type := cand)
  
  joined <- left_join(numdf, canddf, by = c("year", "caseID", type))
  
  joined %>% 
    mutate(!!abstract_varname := std_voteopts(.data[[abstract_varname]])) %>%
    mutate(!!show_varname := str_c(name, " (", party, ")", sep = "")) %>% 
    select(!!c(colnames(numdf), show_varname), everything())
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


i_hou_who <- num_cand_match(i_rep, hc_fec_match)
i_sen_who <- num_cand_match(i_sen, sc_fec_match)
i_gov_who <- num_cand_match(i_gov, gc_fec_match)

v_hou_who <- num_cand_match(v_rep, hc_fec_match)
v_sen_who <- num_cand_match(v_sen, sc_fec_match)
v_gov_who <- num_cand_match(v_gov, gc_fec_match)


i_hou_who %>% select(1:10) %>% mutate(intent_rep_char = std_voteopts(intent_rep_char))



keep_from_fec <- c("year", "caseID", "fec", "icpsr_num")



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
