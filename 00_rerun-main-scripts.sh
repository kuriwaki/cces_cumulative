# Run scripts
Rscript 01_define-names-labels.R
Rscript 02_read-common.R
Rscript 03_prepare-fixes.R
Rscript 04_stack-cumulative.R
Rscript 05_extract-contextual.R
Rscript 06_merge-contextual_upload.R
# Rscript 07_format-crunch.R
Rscript -e "rmarkdown::render('guide/guide_cumulative_2006_2017.Rmd')"

