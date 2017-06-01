library(dplyr)
library(readr)
library(tibble)

wd <- "~/Dropbox/cces_cumulative/"


f.names <- list.files(file.path(wd, "data/source"),  recursive = TRUE)


f.info <- file.info(file.path(wd, "data/source", f.names))

f.info.df <- bind_cols(name = f.names, f.info) %>%
  mutate(sizeMB = utils:::format.object_size(size, "MB")) %>%
  select(name, sizeMB, mtime, ctime, atime)


write_delim(f.info.df, file.path(wd, "currentdatainfo.txt"), delim = "\t")
