# take cumulative data built with 2006 - 2012, and store skeleton.

setwd("~/Dropbox/cces_cumulative")
require(dplyr)
require(haven)


# ccc12 <- read_dta("data/source/2012/cces_common_cumulative_4.dta")
save(ccc12, file = "data/source/2012/ccc12.rdata")
load(file = "data/source/2012/ccc12.rdata")

ccc.vars <- data.frame(outname = names(ccc12),
                       outlabel = sapply(ccc12, function(x) attr(x, "label")),
                       outorder = seq_len(ncol(ccc12)),
                       stringsAsFactors = F) %>% tbl_df()


save(ccc.vars, file = "data/output/ccc_vars_orig.rdata")

