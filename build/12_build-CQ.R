library(dplyr)
library(readxl)

# Reading everything in----
file_names109 <- list.files("data/source/cq/mc_metadata/congress 109 metadata", full.names = TRUE)
files109 <- lapply(file_names109, read_xlsx)

file_names110 <- list.files("data/source/cq/110", full.names = TRUE)
files110 <- lapply(file_names110, read_xlsx)

file_names111 <- list.files("data/source/cq/111", full.names = TRUE)
files111 <- lapply(file_names111, read_xlsx)

file_names112 <- list.files("data/source/cq/112", full.names = TRUE)
files112 <- lapply(file_names112, read_xlsx)

file_names113 <- list.files("data/source/cq/113", full.names = TRUE)
files113 <- lapply(file_names113, read_xlsx)

file_names114 <- list.files("data/source/cq/114", full.names = TRUE)
files114 <- lapply(file_names114, read_xlsx)

# Metadata for congress 115 was compiled in a different format from congresses 109-114
df115 <- read_xlsx("data/source/cq/115/cq 115 metadata.xlsx")


 # Create and clean dataframes before binding----
colnames <- c("Last", "First", "Middle", "Suffix", "Nickname", "Born", "Death", "Sex", "Position", "Party", "State", "District", "Start", "End", "Religion", "Race", "JobType1", "JobType2", "JobType3", "JobType4", "JobType5", "Mil1", "Mil2", "Mil3")
files109 <- lapply(files109, setNames, colnames)
df109 <- plyr::ldply(files109, data.frame)
df109 <- df109[!(df109$Last == "Last"), ]
df109 <- df109[!is.na(df109$First), ]
df109$Congress <- NA

# put in column identifying the session of congress (repeated for all future dataframes)
df109$Congress <- "109"

files110 <- lapply(files110, setNames, colnames)
df110 <- plyr::ldply(files110, data.frame)
df110 <- df110[!(df110$Last == "Last"), ]
df110 <- df110[!is.na(df110$First), ]
df110$Congress <- "110"

files111 <- lapply(files111, setNames, colnames)
df111 <- plyr::ldply(files111, data.frame)
df111 <- df111[!(df111$Last == "Last"), ]
df111 <- df111[!is.na(df111$First), ]
df111$Congress <- "111"

files112 <- lapply(files112, setNames, colnames)
df112 <- plyr::ldply(files112, data.frame)
df112 <- df112[!(df112$Last == "Last"), ]
df112 <- df112[!is.na(df112$First), ]
df112$Congress <- "112"

files113 <- lapply(files113, setNames, colnames)
df113 <- plyr::ldply(files113, data.frame)
df113 <- df113[!(df113$Last == "Last"), ]
df113 <- df113[!is.na(df113$First), ]
df113$Congress <- "113"

files114 <- lapply(files114, setNames, colnames)
df114 <- plyr::ldply(files114, data.frame)
df114 <- df114[!(df114$Last == "Last"), ]
df114 <- df114[!is.na(df114$First), ]
df114$Congress <- "114"

# Dataframe for congress 115 has to be cleaned differently (need to add columns to match up with other dataframes for binding)
df115 <- df115[, 1:19]
df115a <- df115[, 1:4]
df115a$Nickname <- NA
df115b <- df115[, 5]
df115b$Death <- NA
df115c <- df115[, 6:11]
df115c$End <- NA
df115d <- df115[12:16]
df115d$JobType4 <- NA
df115d$JobType5 <- NA
df115e <- df115[, 17:19]
df115 <- cbind(df115a, df115b, df115c, df115d, df115e)
df115$Congress <- "115"

# Bind dataframes----
df109to115 <- bind_rows(df109, df110, df111, df112, df113, df114, df115) 
 
df109to115 <- df109to115 %>% 
  rename(congress = Congress) %>%
  mutate(congress = as.integer(congress)) %>%
  tbl_df()


saveRDS(df109to115, "data/output/03_contextual/cq_profiles.Rds")
