library(tidyverse)
library(haven)
library(glue)
library(crunch)

writeToCrunch <- FALSE # to change the crunch dataset

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
  
  joined <- left_join(numdf, canddf, by = c("year", "case_id", type))
  
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
                         chr2 = "[Republican / Candidate 2]",
                         chr3 = "[Other / Candidate 3]") {
  recode(vec,
         `$Housecand1name ($Housecand1party)` = chr1,
         `$Sencand1name ($Sencand1party)` = chr1,
         `$Govcand1name ($Govcand1party)` = chr1,
         `$Govcand1name (Democrat)` = chr1,
         `$Govcand1name (Democratic)` = chr1,
         `Democratic Candidate` = chr1,
         `$Housecand2name ($Housecand2party)` = chr2,
         `$Sencand2name ($Sencand2party)` = chr2,
         `$Govcand2name ($Govcand2party)` = chr2,
         `$Govcand2name (Republican)` = chr2,
         `Republican Candidate` = chr2,
         `$Housecand3name ($Housecand3party)` = chr3,
         `$Sencand3name ($Sencand3party)` = chr3,
         `$Govcand3name ($Govcand3party)` = chr3,
         `Other, Third-Party Candidate` = chr3) %>% 
    gsub("cand", "Cand", .) %>%  # capitalize
    gsub("name", "Name", .) %>%
    gsub("party", "Party", .)
}



#' combined char to number
#' change character by re-defining label ordering ordered by the original number and breaking ties with year
#' @param tbl A table with columns _char and _num for the labels
bind_label <- function(tbl) {
  charname <- grep("_char$", colnames(tbl), value = TRUE)
  numname <- grep("(intent|voted).*_num$", colnames(tbl), value = TRUE)
  varname <- gsub("_char", "", charname)
  

  tbl_clps <- int_vot_manual(tbl, vn = varname, cn = charname, nn = numname)
  
  # order x by y
  median2 <- function(x, y) {
    median(x[order(y, na.last = FALSE)])
  }
  
  tbl_clps  %>%
    mutate(!! varname := fct_reorder2(.data[[charname]], 
                                      x = .data[[numname]], 
                                      y = .data[["year"]], 
                                      fun = median2,
                                      .desc = FALSE)) %>%
    select(year, case_id, !! varname)
}

#' Manual edit vote/intent vars to collapse
#' 
#' 
#' 
int_vot_manual <- function(tbl, vn, cn, nn) {
  if (grepl("intent", vn)) {
    tbl_fmt <- tbl %>% 
      mutate(!!cn := replace(.data[[cn]], .data[[nn]] >= 98, NA),
             !!nn := replace(.data[[nn]], .data[[nn]] >= 98, NA))
  }
  
  if (grepl("voted", vn)) {
    text_nv <- "I Did Not Vote In This Race"
    if (grepl("sen", vn)) values_nv <- c(5, 6, 9)
    if (grepl("rep|gov", vn)) values_nv <- c(8, 9)
    
    tbl_fmt <- tbl %>% 
      mutate(!!cn := replace(.data[[cn]], .data[[nn]] >= 98, NA), # 2016 gives these a number but coerce to NA
             !!nn := replace(.data[[nn]], .data[[nn]] >= 98, NA)) %>%
      mutate(!!cn := replace(.data[[cn]], .data[[nn]] %in% values_nv, text_nv),  # set "no vote to a 9"
             !!nn := replace(.data[[nn]],  .data[[nn]] %in% values_nv, 9)) %>% 
      mutate(!!cn := replace(.data[[cn]], .data[[nn]] == 90, "Not Sure")) %>% 
      mutate(!!nn := replace(.data[[nn]],  .data[[nn]] == 90, 10)) # set not sure to 10, match up to others
    
    if (grepl("sen", vn)) {
      tbl_fmt <- tbl_fmt %>%
        mutate(!!cn := replace(.data[[cn]], .data[[nn]] %in% values_nv & .data[["year"]] == 2012, NA), # only in 2012 senate voted does NotAsked get this label
               !!nn := replace(.data[[nn]], .data[[nn]] %in% values_nv & .data[["year"]] == 2012, NA))
    }
  }
  tbl_fmt
}

#' Slim out the data for crunch, and preparing for crunch match
#' 
#' @tbl The dataset to slim out
slim <- function(tbl, varmarker = '_chosen', id = "fec") {
  
  chosen_var <- grep(varmarker, colnames(tbl), value = TRUE)
  type <- gsub(varmarker, '', chosen_var)
  id_rename <- as.character(glue("{type}_{id}"))
  
  tbl %>% 
    select(!!c("year", "case_id"),
           matches(as.character(glue("{varmarker}$"))),  
           !!id_rename := !!id)
}

# Data -------------
load("data/output/01_responses/vote_responses.RData")
load("data/output/01_responses/incumbents_key.RData")
load("data/output/01_responses/candidates_key.RData")
ccc <- readRDS("data/output/01_responses/cumulative_stacked.Rds")


# add on name and fec, standardized option labels -----
i_rep_who <- num_cand_match(i_rep, rc_fec_match)
i_sen_who <- num_cand_match(i_sen, sc_fec_match)
i_gov_who <- num_cand_match(i_gov, gc_fec_match)

v_rep_who <- num_cand_match(v_rep, rc_fec_match)
v_sen_who <- num_cand_match(v_sen, sc_fec_match)
v_gov_who <- num_cand_match(v_gov, gc_fec_match)


# create a separate dataset for chosen vars ------ 
ids <- c("year", "case_id")

chosen_with_fec <-  slim(i_rep_who) %>% 
  left_join(slim(i_sen_who), ids) %>% 
  left_join(slim(i_gov_who), ids) %>% 
  left_join(slim(v_rep_who), ids) %>% 
  left_join(slim(v_sen_who), ids) %>% 
  left_join(slim(v_gov_who), ids)

# now we can wrap up the abstract labels 
abstract_lbl <- bind_label(i_rep_who) %>% 
  left_join(bind_label(i_sen_who), ids) %>%
  left_join(bind_label(i_gov_who), ids) %>%
  left_join(bind_label(v_rep_who), ids) %>%
  left_join(bind_label(v_sen_who), ids) %>%
  left_join(bind_label(v_gov_who), ids)

# nice dataset for incumbents ? ----
incumbents_with_ID <-  slim(ri_mc_match, "_shown", "icpsr") %>% 
  left_join(slim(s1i_mc_match, "_shown", "icpsr"), ids) %>% 
  left_join(slim(s2i_mc_match, "_shown", "icpsr"), ids) %>% 
  left_join(slim(gov_inc_match, "_shown"), ids)

# merge in the candidate vars ----
ccc_cand <- ccc %>% 
  left_join(abstract_lbl, ids) %>% 
  left_join(chosen_with_fec, ids) %>% 
  left_join(incumbents_with_ID, ids)
  

# Format for output  --------
# for ambiguous categories, where one number cancorrespond to different lables (intent_rep), use fct_reorder
ccc_df <- ccc_cand %>%
  mutate(zipcode = as.character(zipcode)) %>%
  mutate(countyFIPS = str_pad(as.character(countyFIPS), width = 5, pad = "0"))


# make char variables for IDs and numerous categories a factor so crunch knows it's a categorical
ccc_factor <- ccc_df %>% 
  mutate(case_id = as.character(case_id)) %>% # better this than let crunch think its a numeric
  mutate_at(vars(matches("_icpsr$")), as.character) %>%
  mutate_at(vars(matches("_fec$")), as.character) %>%
  mutate_at(vars(matches("(^dist|^CD$|^cong|^state$|^st$)")), as.factor)

# Save ---------
# write sav first for crunch. save RDS and write to dta after applying variable labels in 05
saveRDS(ccc_df, "data/release/cumulative_2006_2016.Rds")
saveRDS(ccc_factor, "data/output/cumulative_2006_2016_factor.Rds")

write_sav(ccc_factor, "data/release/cumulative_2006_2016.sav")

if (writeToCrunch) {
  login()
  newDataset("https://www.dropbox.com/s/jy59lc87plnq6zw/cumulative_2006_2016.sav?dl=0", "CCES Cumulative Common Dev")
  logout()  
}

