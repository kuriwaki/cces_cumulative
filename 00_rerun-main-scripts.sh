

# Run scripts
Rscript 04_stack-cumulative.R
Rscript 05_extract-contextual.R
Rscript 06_merge-contextual_upload.R
Rscript -e "rmarkdown::render('guide/guide_cumulative_2006_2016.Rmd')"
