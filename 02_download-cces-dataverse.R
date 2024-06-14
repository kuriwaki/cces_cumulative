library(ccesMRPprep)
stopifnot(packageVersion("ccesMRPprep") >= "0.1.12")
library(haven)
library(glue)
library(fs)
library(cli)

# this will not recreate all the datasets on my local project, but it gives a start.
# Contact me if you need any data used in subsequent code but not available in Dataverse.

dir_create("data/source/cces")
dir_create("data/output")
dir_create("data/release")

for (yr in 2006:2023) {
  filedir <- "data/source/cces"
  
  filename <- glue("{yr}_cc.dta")
  if (yr == 2007)
    filename <- glue("{yr}_cc.sav")
  
  if (file_exists(path(filedir, filename)))
    next
  
  cli_alert_info("Will download and write {.file {filename}}.")
  dataverse_dl <- get_cces_dataverse(name = yr)
  write_dta(dataverse_dl, path(filedir, filename))
}
