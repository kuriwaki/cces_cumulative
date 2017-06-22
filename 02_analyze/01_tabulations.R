library(dplyr)
library(haven)
library(readxl)


# Read metadata ----
cq_raw <- readRDS("data/output/meta/fmt_metadata_cc16.Rds")

sq <- read_excel("data/output/meta/Labels_2016.xlsx") %>% 
  select(Name, Label) %>%
  rename(stataName = Name) %>%
  mutate(sOrder = 1:n())

# nathan and liz
nl <- read_csv("data/source/2016_guidebook_variables_orderedby2014.csv")


# inner join from stata to nl to get order
sq_ordered <- inner_join(sq, nl, by = c("stataName" = "code16")) %>% 
  select(stataName, rowID) %>%
  arrange(rowID)
sq_ordered


# Get order ----------
# pick rows in crunch for which there exists a stataName -- exact match
cq <- inner_join(cq_raw, sq_ordered, by = c("alias" = "stataName"))




writeToFile <- TRUE
dir_to_print <- file.path(here(), "data/output/meta/tabs/")
# dir_to_print <- file.path("~/Dropbox/CCES_SDA/2016/Guide/Tabulations/")

existing_files <- list.files(dir_to_print, full.names = TRUE)
if (length(existing_files) > 0) {file.remove(existing_files)}


# Tabulations -----

rows_to_tab <- which(cq$type != "numeric" & cq$type != "text" & cq$type != "datetime")

xtlist <- foreach(i = rows_to_tab) %do% {

    # the tabs
    simp.tab <- tibble(`Unweighted N` = as.integer(unlist(cq$count[i])),
                       `num` = unlist(cq$level[i]),
                       `Choice Text` = unlist(cq$labels[i]))
    
    # the quesiton
    addtorow <- list()
    addtorow$pos <- list(-1, 0)
    
    qcodename <- paste0('\\textbf{', 
                        gsub("\\_", "\\\\_", cq$alias[i]), 
                        '} & & \\hfill ', 
                        gsub("\\_", "\\\\_", cq$name[i]))
      
    qwordtext <- paste0('\\begin{minipage}{\\columnwidth}%\n',
                        cq$wording[i], '%\n',
                        '\\end{minipage}\\tabularnewline\n')
    
    addtorow$command <- c(paste0(qcodename, "\\\\\n"),
                          paste0(qwordtext, "\\\\"))
    
    
    filename <- paste0(formatC(cq$rowID[i], width = 3, format = "d", flag = "0"), 
                       "_",  
                       cq$alias[i], 
                       ".tex")
    
    if(i %% 50 == 0) cat(paste0(i, " out of ", length(rows_to_tab), " done ...\n"))
    list(filename = filename,
         addtorow = addtorow,
         xtab = xtable(simp.tab, 
                       align = "lR{.125\\textwidth}p{.125\\textwidth}p{.7\\textwidth}",
                       display = c("d", "d", "d", "s")))
}

stopifnot(writeToFile) 


for (i in 1:length(xtlist)) {
  print(xtlist[[i]]$xtab, 
        include.rownames = FALSE,
        include.colnames = FALSE,
        add.to.row =  xtlist[[i]]$addtorow,
        timestamp = NULL,
        floating = FALSE,
        file = file.path(dir_to_print, xtlist[[i]]$filename))
  if(i %% 50 == 0) cat(paste0(i, " out of ", length(xtlist), " done ...\n"))
}




# gen latex ----
# LaTeX code to have these input at once (but in order)?

texfiles <- list.files(dir_to_print) # sorted

sink("test_codebook/testcodebook.tex")
cat(
"\\documentclass[12pt,letterpaper,oneside,titlepage]{article}
\\usepackage{array}
\\newcolumntype{R}[1]{>{\\raggedleft\\let\\newline\\\\\\arraybackslash\\hspace{0pt}}m{#1}}
\\usepackage[margin=1in]{geometry}
\\begin{document}

"
)

for (i in 1:length(texfiles)) {
  cat(paste0("\\input{../data/output/meta/tabs/", texfiles[i], "}"))
  cat(
    "
    \\vspace{1cm}

    ")
}

cat("\\end{document}")
sink()


# save for other scripts
saveRDS(cq, "data/output/fmt_metadata_ordered_cc16.Rds")
