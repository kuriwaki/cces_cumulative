library(ccesMRPprep)
stopifnot(packageVersion("ccesMRPprep") >= "0.1.15")
library(haven)
library(glue)
library(fs)
library(cli)

# this will not recreate all the datasets on my local project, but it gives a start.
# Contact me if you need any data used in subsequent code but not available in Dataverse.

dir_create("data/source/cces")
dir_create("data/output")
dir_create("data/release")

for (yr in 2006:2025) {
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

# Success!
download.file(
  "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/26451/1WJB6C", 
  destfile = "data/source/cces/2006_2012_cumulative.dta"
              )

# Modules and panels ----------

# read_dta("data/source/cces/2018_cc_competitive.dta") # doi:10.7910/DVN/IA9SND
# This is the correct DOI but the download doesn't work
download.file(
  "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/IA9SND", 
  destfile = "data/source/cces/2018_cc_competitive.dta"
)

# Can't find the corresponding DOI
read_dta("data/source/cces/2012_panel_h.dta")

# Can't find the corresponding DOI
read_dta("data/source/cces/2009_hum_recontact.dta")



