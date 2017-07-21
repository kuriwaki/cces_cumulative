rm(list = ls())
library(dplyr)
library(haven)
library(readxl)
library(readr)
library(here)
library(foreach)
library(xtable)
library(data.table)


# Specify target directories ---
dir_to_print <- file.path("~/Dropbox/CCES_SDA/2016/Guide/Tabulations/")
dir_for_codebook <- file.path("~/Dropbox/CCES_SDA/2016/Guide/")
writeToFile <- TRUE

# Read metadata ----
cq_raw <- readRDS("data/output/meta/fmt_metadata_cc16.Rds")

sq <- read_excel("data/output/meta/Labels_2016.xlsx") %>% 
  select(Name, Label) %>%
  rename(stataName = Name) %>%
  mutate(sOrder = 1:n())

# nathan and liz
nl <- read_csv("data/source/cces/2016_guidebook_variables_orderedby2014.csv")

# whichch ones are we missing?
nl %>% 
  filter(section14 != "Profile") %>%
  filter(!grepl("_post", code16)) %>% 
  filter(!grepl("(CC|_)3[0-9][0-9]", code16)) %>%
  filter(!grepl("(CC|_)4[0-9][0-9]", code16)) %>%
  filter(!is.na(code16))


# inner join from stata to nl to get order
sq_ordered <- inner_join(sq, nl, by = c("stataName" = "code16")) %>% 
  select(stataName, rowID, section14) %>%
  arrange(rowID)


# Get order ----------
# pick rows in crunch for which there exists a stataName -- exact match
cq <- inner_join(cq_raw, sq_ordered, by = c("alias" = "stataName"))



# delete exisitng ------
existing_files <- list.files(dir_to_print, full.names = TRUE)
if (length(existing_files) > 0) {file.remove(existing_files)}


# Tabulations as xtab objects -----
rows_to_tab <- which(cq$type != "text" & cq$type != "datetime")

xtlist <- foreach(i = rows_to_tab) %do% {
  
  if (cq$type[i] != "numeric") {
    simp.tab <- tibble(`Unweighted N` = as.integer(unlist(cq$count[i])),
                       `num` = unlist(cq$level[i]),
                       `Choice Text` = unlist(cq$labels[i]))
    # make into xtable
    xtab <- xtable(simp.tab, 
                   align = "lR{.23\\textwidth}p{.05\\textwidth}p{.7\\textwidth}",
                   display = c("d", "d", "d", "s"))
  }
  
  if (cq$type[i] == "numeric") xtab <- xtable(summary(data.frame(x = unlist(cq$count[i]))))
    
    
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
                          paste0(qwordtext, "\\\\\n"))
    
    # for the .tex file, number with the rowID
    filename <- paste0(formatC(cq$rowID[i], width = 3, format = "d", flag = "0"), 
                       "_",  
                       cq$alias[i], 
                       ".tex")
    
    
    if(i %% 50 == 0) cat(paste0(i, " out of ", length(rows_to_tab), " done ...\n"))
    list(filename = filename,
         addtorow = addtorow,
         section = cq$section14[i],
         xtab = xtab)
}


# print these xtables to file -------
stopifnot(writeToFile) 

for (i in 1:length(xtlist)) {
  print(xtlist[[i]]$xtab, 
        include.rownames = FALSE,
        include.colnames = FALSE,
        tabular.environment = ifelse(xtlist[[i]]$filename == "004_inputstate.tex",
                                     "longtable",
                                     "tabular"),
        add.to.row =  xtlist[[i]]$addtorow,
        hline.after = c(-1, nrow(xtlist[i]$xtable)),
        timestamp = NULL,
        floating = FALSE,
        file = file.path(dir_to_print, xtlist[[i]]$filename))
  if(i %% 50 == 0) cat(paste0(i, " out of ", length(xtlist), " done ...\n"))
}


# latex helpers
tableToSection <- distinct(cq, rowID, section14) %>% 
  arrange(rowID)




# gen latex ----
# LaTeX code to have these input at once (but in order)?

texfiles <- list.files(dir_to_print) # sorted


# list of tables
sink(file.path(dir_for_codebook, "2016codebook_contents.tex"))
for (i in 1:length(texfiles)) {
  
  # figure out the section
  rID <- as.numeric(gsub("^([0-9]+)_.*", "\\1", texfiles[[i]]))
  section_i <- tableToSection %>% filter(rowID == rID) %>% pull(section14)
  
  
  # add section divider if new section
  if (i == 1) section_pre <- ""
  if (!is.na(section_i) & !is.na(section_pre)) {
    if (section_i != section_pre) {
    cat(paste0("\\newpage\n\\section{", section_i, "}\n"))
    }
  }
  
  
  # print your table
  cat(paste0("\\input{Tabulations/", texfiles[i], "}"))
  cat("\n\\vspace{0.8cm}\n") # vertical space
  section_pre <- section_i # store
}


sink()


# the document ------
genWrapper <- FALSE
if (genWrapper) {
  sink(file.path(dir_for_codebook, "2016codebook_wrapper.tex"))
  cat("\\documentclass[12pt,letterpaper,oneside,titlepage]{article}
\\usepackage{array}
\\newcolumntype{R}[1]{>{\\raggedleft\\let\\newline\\\\\\arraybackslash\\hspace{0pt}}m{#1}}
\\usepackage[margin=1in]{geometry}
\\usepackage{longtable}
\\begin{document}
\\begin{center}
\\Huge \\textsc{Guide to the 2016 Cooperative Congressional Election Survey}
\\vskip1cm
\\vfill
\\end{center}
\\normalsize
\\newpage
\\tableofcontents\n\n\n")
  cat("\\input{2016codebook_contents.tex}\n\n\n")
  cat("\\end{document}")
  sink()
}


# save for other scripts ----
saveRDS(cq, "data/output/fmt_metadata_ordered_cc16.Rds")

