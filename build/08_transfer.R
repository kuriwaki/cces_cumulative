library(haven)
setwd("~/Dropbox/CCES16_TEAMS")

stopifnot(packageVersion("haven") >= "1.1.0") # older ver has label length restriction

# CHANGE directory where data files are
datadir <- "~/Dropbox/CCES16_TEAMS"

# all filenames
filenames <- list.files(datadir, full.names = TRUE, recursive = TRUE)

# filter down to all SPSS files
fnames_spss <- grep("\\.sav", filenames, value = TRUE)

# loop through and read, write, write.
for (f in fnames_spss) { # REMOVE the [1:5] (which limits to first five folders) once you've verified this works
  
  # read 
  sav_f <- read_sav(f)
  
  # single file name -- remove parent folder
  fs <- gsub(".*/(.*sav$)", "\\1", f)
  
  # write to Rds and dta and spss in other folder
  saveRDS(sav_f, file.path(gsub("\\.sav", ".Rds", f)))
  write_dta(sav_f, file.path(gsub("\\.sav", ".dta", f)))
  # write_sav(sav_f, file.path(datadir, "out_spss", gsub("\\.sav", ".sav", f)))
}
