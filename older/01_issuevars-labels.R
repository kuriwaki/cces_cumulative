# Written by Claude Code
# Archived from 01_define-names-labels.R (2026-06-26).
# The `iss_meta` issue-variable metadata and its output `issuevars_vartable.Rds`
# were only consumed by the (now-archived) Crunch script `08_format-crunch.R`.
# Nothing in the main pipeline (02-07) or the guide reads them. Kept here for
# reference in case the Crunch dataset is ever rebuilt.

library(tidyverse)
library(tibble)

# Issue-variable metadata (Crunch-only) ----
iss_meta <- tribble(
  ~alias, ~name,  ~description,
  "year_date",  "Year", "Year of CCES (date)",
  "banassault", "Ban assault rifles", "On the issue of gun regulation, are you for or against each of the following proposals? ... Ban assault rifles",
  "repealACA",  "Repeal the ACA", "Thinking now about health care policy, would you support or oppose each of the following proposals? ... Repeal the entire Affordable Care Act",
  "ideoself",   "Ideology (7-point)", "How would you rate each of the following individuals and groups? ... Yourself",
  "demparty",   "Perceived ideology of the Democractic Party", "How would you rate each of the following individuals and groups? The Democratic Party",
  "repparty",   "Perceived ideology of the Republican Party", "How would you rate each of the following individuals and groups? ... The Republican Party",
  "bornagain",  "Born-again Christian", "Would you describe yourself as a 'born-again'; or evangelical Christian, or not?",
  "religimpt",  "Importance of religion", "How important is religion in your life?",
  "churchatt",  "Church attendance",  "Aside from weddings and funerals, how often do you attend religious services?",
  "prayer",     "Frequency of prayer", "People practice their religion in different ways. Outside of attending religious services, how often do you pray?",
  "religion",   "Religion", "What is your present religion, if any?",
  "renewable",  "Renewable fuels requirements", "Do you support or oppose each of the following proposals? Require a minimum amount of renewable fuels (wind, solar, and hydroelectric) in the generation of electricity even if electricity prices increase somewhat",
  "cleanair",   "Clean Air Act enforcement", "Do you support or oppose each of the following proposals? Strengthen enforcement of the Clean Air Act and Clean Water Act even if it costs US jobs",
  "meeting",    "Attend political meetings", "During the past year did you ... Attend local political meetings (such as school board or city council)",
  "sign",       "Put up a political sign", "During the past year did you ... Put up a political sign (such as a lawn sign or bumper sticker)",
  "candidate",  "Camapign participation", "During the past year did you ...  Work for a candidate or campaign",
  "donor",      "Donate money", "During the past year did you ... Donate money to a candidate, campaign, or political organization",
  "resent1",    "Racial resentment 1", "Irish, Italians, Jewish and many other minorities overcame prejudice and worked their way up. Blacks should do the same without any special favors.",
  "resent2",    "Racial resentment 2", "Generations of slavery and discrimination have created conditions that make it difficult for blacks to work their way out of the lower class.",
  "spendwelfare", "Welfare spending", "State legislatures must make choices when making spending decisions on important state programs. How would you like your legislature to spend money on each of the five areas below? ... Welfare",
  "spendhealth", "Health care spending", "State legislatures must make choices when making spending decisions on important state programs. How would you like your legislature to spend money on each of the five areas below? ... Health Care",
  "spendeduc",   "Education spending", "State legislatures must make choices when making spending decisions on important state programs. How would you like your legislature to spend money on each of the five areas below? ... Education",
  "spendpolice", "Law enforcement spending", "State legislatures must make choices when making spending decisions on important state programs. How would you like your legislature to spend money on each of the five areas below? ... Law Enforcement",
  "spendtransp", "Transportation and infrastructure spending", "State legislatures must make choices when making spending decisions on important state programs. How would you like your legislature to spend money on each of the five areas below? ... Transportation / Infrastructure",
  "legalstatus", "Immigration legal status", "What do you think the U.S. government should do about immigration? Select all that apply. Grant legal status to all illegal immigrants who have held jobs and paid taxes for at least 3 years, and not been convicted of any felony crimes",
  "security",    "Immigration border patrols", "What do you think the U.S. government should do about immigration? Select all that apply. Increase the number of border patrols on the U.S.-Mexican border",
  "gaymarriage", "Gay marriage", "Do you favor or oppose allowing gays and lesbians to marry legally?"
)

stopifnot(n_distinct(iss_meta$alias) == nrow(iss_meta) &&
            n_distinct(iss_meta$name) == nrow(iss_meta))


# Save ----
saveRDS(iss_meta, "data/output/02_questions/issuevars_vartable.Rds")
