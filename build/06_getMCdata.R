library(readxl)

# the working directory (i.e. the project directory)
wd <- "~/Dropbox/cces_cumulative"

# get file paths of all files in data/source/cq/mc_metadata
file_names <- list.files(file.path(wd, "data/source/cq/mc_metadata"),
                         full.names = TRUE,
                         recursive = TRUE)


# a function that reads and adds a Congress
my_read_excel <- function(filename){
  cong <- as.numeric(gsub(".*congress ([0-9]+) metadata.*", "\\1", filename))
  
  read_excel(filename) %>% 
    mutate(Congress = cong)
}


# read files with custom function
cq_raw <- lapply(file_names, my_read_excel)


# unify names
colnames <- c("Last","First","Middle","Suffix","Nickname",
              "Born","Death",
              "Sex",
              "Position","Party",
              "State","District",
              "Start","End",
              "Religion","Race",
              "JobType1","JobType2","JobType3","JobType4","JobType5",
              "Mil1","Mil2","Mil3", "Congress")

cq_named <- lapply(cq_raw, setNames, colnames)

# filter out non-data rows
df <- plyr::ldply(cq_named, data.frame)
df <- df[!(df$Last=="Last"),]
df <- df[!is.na(df$First),]

#get list of congresspeople from 109-114-----
library(plyr)
setwd("~/Dropbox/cces_cumulative/data/source/voteview")
file_names = list.files(getwd())
files = lapply(file_names,read_csv)
colnames = c("congress", "chamber","icpsr","state_icpsr","district_code","state_abbrev","party_code","occupancy","last_means", "bioname","bioguide_id","born","died","dim1","dim2","log_likelihood","geo_mean_probability","number_of_votes","number_of_errors","conditional")
files <- lapply(files,setNames, colnames)
voteviewlist <- ldply(files, data.frame)
voteviewlist <- voteviewlist[(!voteviewlist$chamber=="President"),]