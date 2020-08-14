# Run scripts
# Rscript 01_define-names-labels.R || exit 1
Rscript 03_read-common.R || exit 1
# Rscript 04_prepare-fixes.R || exit 1
Rscript 05_stack-cumulative.R || exit 1
Rscript 06_extract-politicians.R || exit 1
Rscript 07_merge-contextual_upload.R || exit 1
# Rscript 07_format-crunch.R || exit 1
Rscript -e "rmarkdown::render('guide/guide_cumulative_2006_2019.Rmd')" || exit 1

