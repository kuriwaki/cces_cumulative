library(tidyverse)
library(haven)
library(glue)
library(tigris)
library(cli)

writeToCrunch <- FALSE # to change the crunch dataset

drop_post <- function(x) filter(x, !str_detect(dataset, "_post"))

# apppend the candidatename-candidate party variables to the vote choice qs
#' append labels to numeric responses
#' 
#' @param numdf the slim df with choices as numbers
#' @param canddf the df with all the candidate info
#' 
#' @return A dataset with nrow(numdf) rows that has all the candidate info appended
#' for the candidate the respondent chose

num_cand_match <- function(numdf, canddf) {
  type <- str_subset(colnames(numdf), "_num") # columns with 
  no_num <- str_replace(type, '_num', '')
  chosen_varname <- glue("{no_num}_chosen") # e.g. intent_rep_chosen = Maxine Waters
  party_varname <-  glue("{no_num}_party") # e.g. intent_rep_party = Democrat
  abstract_varname <- as.character(glue("{str_replace(type, '_num', '')}_char")) # e.g. intent_rep_char = Democract /Cand 1
  
  canddf <- rename(canddf, !!type := cand) |> 
    drop_post()
  
  joined <- left_join(numdf, canddf, by = c("year", "case_id", type))
  
  joined |> 
    mutate(!!abstract_varname := std_voteopts(.data[[abstract_varname]])) |>
    mutate(!!chosen_varname := str_c(name, " (", party, ")", sep = ""),
           !!party_varname := spell_out_party_abbrv(party)) |> 
    select(!!c(colnames(numdf), chosen_varname, party_varname), everything())
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
         `Democrat` = chr1,
         `$Housecand2name ($Housecand2party)` = chr2,
         `$Sencand2name ($Sencand2party)` = chr2,
         `$Govcand2name ($Govcand2party)` = chr2,
         `$Govcand2name (Republican)` = chr2,
         `Republican Candidate` = chr2,
         `Republican` = chr2,
         `$Housecand3name ($Housecand3party)` = chr3,
         `$Sencand3name ($Sencand3party)` = chr3,
         `$Govcand3name ($Govcand3party)` = chr3,
         `Other, Third-Party Candidate` = chr3) |> 
    str_replace_all("cand", "Cand") |>
    str_replace_all("name", "Name") |>
    str_replace_all("party", "Party")
}


#' Spell out D/Rs
#' @vec vector of party names that may include abbreviations like 
#' @examples spell_out_party_abbrv(c("D", "R", "Constitutional"))
spell_out_party_abbrv <- function (vec) {
  recode(vec,
         `D` = "Democratic",
         `R` = "Republican",                      
         `I` = "Independent", 
         `L` = "Libertarian",
         `G` =  "Green") |> 
    fct_infreq() |>  # generally in order
    fct_relevel(c("Democratic", "Republican")) # set this 
} 

#' combined char to number
#' change character by re-defining label ordering ordered by the original number and breaking ties with year
#' @param tbl A table with columns _char and _num for the labels
bind_label <- function(tbl, carry_vars = ids) {
  charname <- str_subset(colnames(tbl), "_char$")
  numname <-  str_subset(colnames(tbl), "(intent|voted).*_num$")
  varname <-  str_replace(charname, "_char", "")
  

  tbl_clps <- int_vot_manual(tbl, vn = varname, cn = charname, nn = numname)
  
  # order x by y
  median2 <- function(x, y) {
    median(x[order(y, na.last = FALSE)])
  }
  
  tbl_clps  |>
    mutate(!! varname := fct_reorder2(.data[[charname]], 
                                      .x = .data[[numname]], 
                                      .y = .data[["year"]], 
                                      .fun = median2,
                                      .na_rm = FALSE,
                                      .desc = FALSE)) |>
    select(!!carry_vars, !!varname)
}

#' Manual edit vote/intent vars to collapse
#' 
#' 
#' 
int_vot_manual <- function(tbl, vn, cn, nn) {
  if (grepl("intent", vn)) {
    tbl_fmt <- tbl
  }
  
  if (grepl("voted", vn)) {
    text_nv <- "I Did Not Vote In This Race"
    if (grepl("sen", vn)) values_nv <- c(5, 6, 9)
    if (grepl("rep|gov", vn)) values_nv <- c(8, 9)
    
    tbl_fmt <- tbl |> 
      mutate(!!cn := replace(.data[[cn]], .data[[nn]] %in% values_nv, text_nv),  # set "no vote to a 9"
             !!nn := replace(.data[[nn]],  .data[[nn]] %in% values_nv, 9)) |> 
      mutate(!!cn := replace(.data[[cn]], .data[[nn]] == 90, "Not Sure")) |> 
      mutate(!!nn := replace(.data[[nn]],  .data[[nn]] == 90, 10)) # set not sure to 10, match up to others
    
    if (grepl("sen", vn)) {
      tbl_fmt <- tbl_fmt |>
        mutate(!!cn := replace(.data[[cn]], .data[[nn]] %in% values_nv & .data[["year"]] == 2012, NA), # only in 2012 senate voted does NotAsked get this label
               !!nn := replace(.data[[nn]], .data[[nn]] %in% values_nv & .data[["year"]] == 2012, NA))
    }
  }
  tbl_fmt
}

#' Slim out the data for crunch, and preparing for crunch match
#' 
#' @tbl The dataset to slim out
#' @varmaker regexp to capture var of interest
#' @id  suffix for identifier variable
slim <- function(tbl, varmarker = '(_chosen|_party)', id = NULL, carry_vars = ids) {
  
  chosen_var <- str_subset(colnames(tbl), varmarker)
  type <- str_replace(chosen_var, varmarker, '')
  
  if (!is.null(id)) {
    id_rename <- unique(glue("{type}_{id}"))
    
    tbl_fmt <- select(tbl, 
                      !!carry_vars, 
                      matches(as.character(glue("{varmarker}$"))),
                      !!id_rename := !!id)
    
    return(tbl_fmt)
  }
  
  if (is.null(id))  {
                      
    select(tbl, 
           !!carry_vars, 
           matches(as.character(glue("{varmarker}$"))))
  }
}



# Data -------------
cli_h1("Load datasets")
load("data/output/01_responses/vote_responses.RData")
load("data/output/01_responses/incumbents_key.RData")
load("data/output/01_responses/candidates_key.RData")
ccc <- readRDS("data/output/01_responses/cumulative_stacked.Rds")
ccc_meta <- readRDS("data/output/02_questions/cumulative_vartable.Rds")
panel_ids <- readRDS("data/output/01_responses/addon_ids.Rds")
bs_stata <- read_dta("data/source/cces/schaffner_issues.dta")



# add on name and icpsr, standardized option labels -----
cli_h1("Link chosen candidate")
i_rep_who <- num_cand_match(i_rep, rc_key)
i_sen_who <- num_cand_match(i_sen, sc_key)
i_gov_who <- num_cand_match(i_gov, gc_key)

v_rep_who <- num_cand_match(v_rep, rc_key)
v_sen_who <- num_cand_match(v_sen, sc_key)
v_gov_who <- num_cand_match(v_gov, gc_key)


# create a separate dataset for chosen vars ------ 
ids <- c("year", "case_id")

chosen_with_party <-  slim(i_rep_who) |> 
  left_join(slim(i_sen_who), ids) |> 
  left_join(slim(i_gov_who), ids) |> 
  left_join(slim(v_rep_who), ids) |> 
  left_join(slim(v_sen_who), ids) |> 
  left_join(slim(v_gov_who), ids)

# now we can wrap up the abstract labels 
abstract_lbl <- bind_label(i_rep_who) |> 
  left_join(bind_label(i_sen_who)) |>
  left_join(bind_label(i_gov_who)) |>
  left_join(bind_label(v_rep_who)) |>
  left_join(bind_label(v_sen_who)) |>
  left_join(bind_label(v_gov_who))

# pre-merge and order vars 
lbl_party_name <- 
  left_join(abstract_lbl, chosen_with_party, by = ids) |> 
  select(year, case_id, 
         matches("intent_rep(_party|$)"),
         matches("voted_rep(_party|$)"),
         matches("intent_gov(_party|$)"),
         matches("voted_gov(_party|$)"),
         matches("intent_sen(_party|$)"),
         matches("voted_sen(_party|$)"),
         everything())

# incumbents
cli_h1("Add incumbent info")
incumbents_with_ID <-  slim(drop_post(ri_mc_match), "_current", "icpsr") |> 
  left_join(slim(drop_post(s1i_mc_match), "_current", "icpsr"), ids) |> 
  left_join(slim(drop_post(s2i_mc_match), "_current", "icpsr"), ids) |> 
  left_join(slim(drop_post(gov_inc_match), "_current"), ids)

# merge in the candidate vars ----
ccc_cand <- ccc |> 
  left_join(lbl_party_name, ids) |> 
  left_join(incumbents_with_ID, ids)

stopifnot(nrow(ccc) == nrow(ccc_cand))

# Format for output  --------
cli_h1("Format variable class")
# for ambiguous categories, where one number can correspond to different lables (intent_rep), use fct_reorder
ccc_df <- ccc_cand |>
  mutate(zipcode = as.character(zipcode)) |>
  mutate(county_fips = str_pad(as.character(county_fips), width = 5, pad = "0")) |> 
  mutate_at(vars(year, case_id), as.integer) |> 
  mutate_if(is.factor, fct_drop) # drop unused values

# make char variables for IDs for Stata
# change a few categories a factor so crunch knows it's a categorical
ccc_fac <- ccc_df |> 
  mutate(case_id = as.character(case_id)) |> # better this than let crunch think its a numeric
  mutate_at(vars(matches("(_icpsr$|hisp_origin)")), as.character) |>
  mutate_at(vars(matches("(^cong)")), as.factor)


# FIPS-state key 
fips_key <-  tigris::fips_codes |> 
  as_tibble() |> 
  transmute(st = state, state = state_name, st_fips = as.integer(state_code)) |> 
  distinct()

fips_key_post <- rename(fips_key, st_post = st, state_post = state)

ccc_factor <-   ccc_fac |> 
  left_join(fips_key) |>
  mutate(state = labelled(st_fips, deframe(select(fips_key, state, st_fips))),
         st = labelled(st_fips, deframe(select(fips_key, st, st_fips)))) |> 
  select(-st_fips) |> 
  left_join(fips_key_post) |>
  mutate(state_post = labelled(st_fips, deframe(select(fips_key_post, state_post, st_fips))),
         st_post = labelled(st_fips, deframe(select(fips_key_post, st_post, st_fips)))) |> 
  select(-st_fips)

# Save ---------
cli_h1("Save Final data")
# write sav first for crunch. save RDS and write to dta after applying variable labels in 05
write_rds(ccc_df, "data/release/cumulative_2006-2022_addon.rds")

# anti-join things not to put on dataverse (panel, module)
panel_charid <- mutate(panel_ids, case_id = as.character(case_id)) # for crunch
write_rds(anti_join(ccc_df, panel_ids), "data/release/cumulative_2006-2022.rds", compress = "xz")


# remove panel cases
ccc_common <- anti_join(ccc_factor, panel_charid, by = c("year", "case_id"))

# Write to dta with var labels
for (v in colnames(ccc_common)) {
  attributes(ccc_common[[v]])$label <- ccc_meta$name[which(ccc_meta$alias == v)]
}

write_rds(ccc_common, "data/output/cumulative_2006-2022_factor.rds")

write_dta(ccc_common, "data/release/cumulative_2006-2022.dta", version = 14)
arrow::write_feather(ccc_common, "data/release/cumulative_2006-2022.feather")

# might write to crunch
if (writeToCrunch) {
  cli_h1("Writing to crunch")
  library(crunch)
  
  # crunch var
  bs_df <- bs_stata |> 
    select(-religion) |> # already in 
    mutate(case_id = as.character(case_id),
           year = as.integer(year)) |>
    select(year, case_id, everything())
  
  ccc_crunch <- ccc_common |> 
    left_join(bs_df, by = c("year", "case_id")) |> 
    mutate(year_date = as.Date(str_c(as.character(year), "-11-01"), "%Y-%m-%d")) |> 
    select(year, year_date, everything())
  
  write_sav(ccc_crunch, "data/release/cumulative_2006-2021_crunch.sav") 
  
  if (file.exists("data/release/cumulative_2006-2021_crunch.sav.gz")) {
    file.remove("data/release/cumulative_2006-2021_crunch.sav.gz")
    R.utils::gzip("data/release/cumulative_2006-2021_crunch.sav")
  }
  
  # write to crunch
  login()
  deleteDataset("CCES Cumulative Common Dev")
  newDataset("https://www.dropbox.com/s/p8cx49h82coqfcs/cumulative_2006_2018_crunch.sav?dl=0", 
             "CCES Cumulative Common Dev")
  logout()  
}

cat("Finished merging candidate vars and the rest. Updated Rds and sav. Write to dta. Upload to crunch?\n")



