library(readxl)

# the working directory (i.e. the project directory)
wd <- "~/Dropbox/cces_cumulative"

# get file paths of all files in data/source/cq/mc_metadata
file_names <- list.files(file.path(wd, "data/source/cq/mc_metadata"),
                         full.names = TRUE,
                         recursive = TRUE)

# read files
cq_raw <- lapply(file_names, read_excel)

colnames <- c("Last","First","Middle","Suffix","Nickname",
              "Born","Death",
              "Sex",
              "Position","Party",
              "State","District",
              "Start","End",
              "Religion","Race",
              "JobType1","JobType2","JobType3","JobType4","JobType5",
              "Mil1","Mil2","Mil3")

files <- lapply(cq_raw, setNames, colnames)

df <- plyr::ldply(cq_raw, data.frame)
df <- df[!(df$Last=="Last"),]
df <- df[!is.na(df$First),]