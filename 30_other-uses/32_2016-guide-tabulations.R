library(dplyr)
library(haven)
library(readxl)
library(readr)
library(foreach)
library(xtable)


# Specify target directories ---

# guide where individual variable tables will be printed
dir_to_print <- file.path("~/Dropbox/CCES_SDA/2016/Guide/Tabulations/")

# main guidebook directory
dir_for_codebook <- file.path("~/Dropbox/CCES_SDA/2016/Guide/")
writeToFile <- TRUE

# Read metadata ----
cq_raw <- readRDS("data/output/meta/fmt_metadata_cc16.Rds")

sq <- read_excel("data/output/meta/Labels_2016.xlsx") %>% 
  select(Name, Label) %>%
  rename(stataName = Name) %>%
  mutate(sOrder = 1:n())

# nathan and liz
nl <- read_csv("data/source/cces_meta/2016_guidebook_variables_orderedby2014.csv")

# manual input
man <- read_csv(file.path(dir_for_codebook, "identifiers_2016_source.csv")) %>% 
  mutate(wording = "    ")


# some special q's to mark with a warning sign
warning_regex <- c("(CC16_301.|CC16_312_.*|CC16_331_4|CC16_331_5|CC16_331_6|CC16_331_8|CC16_340f|CC16_350|CC16_351A|CC16_351C|CC16_351D)")

# inner join from stata to nl to get order
sq_ordered <- inner_join(sq, nl, by = c("stataName" = "code16")) %>% 
  select(stataName, rowID, section14) %>%
  arrange(rowID)


# Get order ----------
# pick rows in crunch for which there exists a stataName -- exact match
cq <- inner_join(cq_raw, sq_ordered, by = c("alias" = "stataName"))

# rows for stuff not in crunch and for which we will produce a blank table
notable <- inner_join(man, sq_ordered, by = c("alias" = "stataName"))

cq <- bind_rows(cq, notable)




# Tabulations as xtab objects -----
rows_to_tab <- which(cq$type != "text" & cq$type != "datetime")

# column formats
alignCols <- "lR{.23\\textwidth}p{.05\\textwidth}p{.7\\textwidth}"

xtlist <- foreach(i = rows_to_tab) %do% {
  
  if (cq$type[i] != "numeric" & cq$type[i] != "none") {
    simp.tab <- tibble(`Unweighted N` = as.integer(unlist(cq$count[i])),
                       `num` = unlist(cq$level[i]),
                       `Choice Text` = unlist(cq$labels[i]))
    # make into xtable
    xtab <- xtable(simp.tab, 
                   align = alignCols,
                   display = c("d", "d", "d", "s"))
  }
  
  if (cq$type[i] == "numeric") {
    sum.i <- summary(unlist(cq$count[i]))
    xtab <- xtable(tibble(`X1` = names(sum.i),
                          `X2` = as.numeric(sum.i),
                          `X3` = ""),
                   align = alignCols,
                   display = c("d", "s", "f", "s"))
    
  }
  
  if (cq$type[i] == "none") {
    xtab <- xtable(tibble(`X1` = "", `X2` = "", `X3` = ""),
                   align = alignCols,
                   display = c("d", "d", "d", "s"))
  }    
    
    # the quesiton
    addtorow <- list()
    addtorow$pos <- list(-1, 0)
    
    tex_alias <- cq$alias[i]
    if (grepl(warning_regex, tex_alias)) tex_alias <- paste0(tex_alias, "{~\\danger}")
    tex_alias <- gsub("\\_", "\\\\_", tex_alias)
    tex_label <-  gsub("\\_", "\\\\_", cq$name[i])
    
    qcodename <- paste0('\\textbf{', 
                        tex_alias, 
                        '} & & \\hfill ', 
                       tex_label)
      
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



# delete exisitng ------
existing_files <- list.files(dir_to_print, full.names = TRUE)
if (length(existing_files) > 0) {file.remove(existing_files)}



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
        floating = FALSE,
        file = file.path(dir_to_print, xtlist[[i]]$filename))
  if(i %% 50 == 0) cat(paste0(i, " out of ", length(xtlist), " done ...\n"))
}


# latex helpers
tableToSection <- distinct(cq, rowID, section14) %>% 
  arrange(rowID)




# Generate the entire Latex document for content ----
# LaTeX code to have these input at once (but in order)?

texfiles <- list.files(dir_to_print) # sorted


# list of tables
sink(file.path(dir_for_codebook, "2016codebook_contents.tex"))
for (i in 1:length(texfiles)) {
  
  # figure out the section
  rID <- as.numeric(gsub("^([0-9]+)_.*", "\\1", texfiles[[i]]))
  section_i <- tableToSection %>% filter(rowID == rID) %>% pull(section14)
  
  
  # add section divider if new section
  if (i == 1) section_pre <- "Sample Identifiers"
  
  if (!is.na(section_i) & !is.na(section_pre)) { # non-pathological cases
    
    if (i == 1) {
      cat(paste0("\\subsection{", section_pre, "}\n")) # first section
    }
    if (section_i != section_pre & (section_i != "Pre-Election Survey Contextual Variables")) {
      cat(paste0("\\newpage\n\\subsection{", section_i, "}\n")) # other section
    }
    if (section_i != section_pre & section_i == "Pre-Election Survey Contextual Variables") {
      cat("\\input{ValidatedVote_KB_SK}") # first squeeze in VV
      cat("\\newpage\n")
      cat("\\part{Part IV}\n")
      
      cat("\\section{Contextual Variables}\n")
      cat("Contextual variables consist of the names and parties of the candidates for U. S. House, U. S. Senate, and Governor. For all offices, Candidate 1 is the Democrat and Candidate 2 is the Republican, except when no Democrat is running. When no Democrat is running, the Republican is listed as Candidate 1. When only one candidate is running, Candidate 2 is listed as ``NA''.\n")
      
      cat(paste0("\\subsection{", section_i, "}\n")) 
    }
  }
  
  
  # print your table
  cat(paste0("\\input{Tabulations/", texfiles[i], "}"))
  cat("\n\\vspace{0.8cm}\n") # vertical space
  section_pre <- section_i # store
}

# incumbent table dump at the end
cat("\\input{ContextualVariables_KB_SK}\n")

# cross references as a new part
cat("\\part{Part V}\n")
cat("\\input{CrossReferenceVariables_KB_SK}")

sink()


# save for other scripts ----
saveRDS(cq, "data/output/fmt_metadata_ordered_cc16.Rds")
