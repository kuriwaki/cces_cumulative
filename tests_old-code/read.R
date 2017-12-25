## read dta

setwd("~/Dropbox/cces_cumulative")
require(dplyr)
require(haven)
require(data.table)



cc12 <- read_dta("data/source/cc/CCES12_Common_OUTPUT_vv_20130930.dta")
cc13 <- read_dta("data/source/cc/CCES13_Common_OUTPUT.dta")
cc14 <- read_dta("data/source/cc/CCES14_Common_OUTPUT_Aug2015_vm.dta")
cc15 <- read_dta("data/source/cc/CCES15_Common_OUTPUT_Jan2016.dta")



### Codebook table.
load("data/output/ccc_vars_orig.rdata") # named ccc.vars

#==================================================================#
# CODEBOOK KEY
#==================================================================#

#===============================#
# 2012 key
#===============================#
key12 <- read.csv("data/source/varnamekey_cc12.csv",
                  stringsAsFactors = F) %>% tbl_df()

key12$code12 <- gsub("^cc", "CC", key12$code12) # change "cc" to "CC"
key12$code12 <- gsub("^v(?=[0-9])", "V", key12$code12, perl = T) #v101 -> V101
key12$code12[grep("CC33(2|4)", key12$code12)] <- toupper(grep("CC33(2|4)", key12$code12, value = T))
table(key12$code12 %in% colnames(cc12)) # all good

## JOIN
ccc.vars <- left_join(ccc.vars, key12, by = c("outname" = "outvar"))

#===============================#
# 2013 key
#===============================#
key13 <- data.frame(code13 = colnames(cc13),
                    descrip13 = sapply(cc13, function(x) attr(x, "label")),
                    stringsAsFactors = F) %>% tbl_df()
  
# code13 that matches with code12 (assume they describe same thing)
overlap.12.13 <- inner_join(key13 %>% select(code13),
                            key12,
                            by = c("code13" = "code12"))


key13 <- left_join(key13,
                   overlap.12.13,
                   by = "code13") %>% 
  dplyr::select(code13, outvar, everything())


# manually assign key13
key13 <- key13 %>%
  mutate(outvar = replace(outvar, code13 == "caseid", "caseid"),
         outvar = replace(outvar, code13 == "weight", "weight"),
         outvar = replace(outvar, code13 == "gender", "gender"),
         outvar = replace(outvar, code13 == "educ", "educ"),
         outvar = replace(outvar, code13 == "race", "race"),
         outvar = replace(outvar, code13 == "hispanic", "hispanic"),
         outvar = replace(outvar, code13 == "birthyr", "birthyr"),
         outvar = replace(outvar, code13 == "ideo5", "ideo5"),
         outvar = replace(outvar, code13 == "educ", "educ"),
         outvar = replace(outvar, code13 == "CC13_311b", "knowledge_sen1_party_name"),
         outvar = replace(outvar, code13 == "CC13_311c", "knowledge_sen2_party_name"),
         outvar = replace(outvar, code13 == "", ""))


## JOIN
ccc.vars <- left_join(ccc.vars, key13 %>% select(-descrip13), by = c("outname" = "outvar"))







#==================================================================#
# RENAME to outlabel
#==================================================================#

# test 13

ccc.vars.13 <- ccc.vars %>% filter(!is.na(code13))

setnames(cc13,
         old = ccc.vars.13$code13,
         new = ccc.vars.13$outname)





searchterm <- "income"
ccc.vars %>% slice(grep(searchterm, outlabel, ignore.case = T)) ## CCC
key13 %>% slice(grep(searchterm, descrip13, ignore.case = T)) ## CC13


