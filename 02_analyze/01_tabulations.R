


# Read metadata ----
cq <- readRDS("data/output/meta/fmt_metadata_cc16.Rds") %>%
  mutate(aliasAbbrv = gsub("grid", "", alias))

sq <- read_excel("data/output/meta/Labels_2016.xlsx") %>% 
  select(Name, Label) %>%
  rename(stataName = Name) %>%
  mutate(sOrder = 1:n())

# nathan and liz
nl <- read_csv("data/source/2016_guidebook_variables_orderedby2014.csv")


# inner join from stata to nl to get order
sq_ordered <- inner_join(sq, nl, by = c("Name" = "code16")) %>% 
  arrange(rowID)

sq_ordered



# Tabulations -----

# move to separate script
choiceqs.rownum <- which(qwording$nChoices != 0 & qwording$type != "multiple_response")
tabs.list <- list()


writeToFile <- TRUE
dir_to_print <- file.path(here(), "data/output/meta/tabs/")
dir_to_print <- file.path("~/Dropbox/CCES_SDA/2016/Guide/Tabulations/")

for (i in choiceqs.rownum) {
  
  
  # for each question
  for (j in 1:nQs) {
    cat(paste("Question i = ", i, "; SubQ j = ", j, "\n"))
    
    # the tabs
    simp.tab <- tibble(`Unweighted N` = var.arr[j, ],
                       `num` = choiceno.vec,
                       `Choice Text` = choicenames.vec)
    
    # the quesiton
    addtorow <- list()
    addtorow$pos <- list(-1, -1)
    
    qcodename <- paste0('\\textbf{', gsub("\\_", "\\\\_", q.aliases[j]), '} & & \\hfill ', gsub("\\_", "\\\\_", q.names[j]))
    qwordtext <- paste0('\\multicolumn{3}{l}{', qwording$wording[i], '}')
    
    addtorow$command <- c(paste0(qcodename, "\\\\"),
                          paste0(qwordtext, "\\\\"))
    
    
    # format
    simp.xtab <- xtable(simp.tab, display = c("d", "d", "d", "s"))
    filename <- paste0(formatC(i, width = 3, format = "d", flag = "0"), "_",  q.aliases[j], ".tex")
    tabs.list[[filename]] <- simp.xtab
    
    
    # write
    if (writeToFile) {
      print(simp.xtab, 
            include.rownames = FALSE,
            include.colnames = FALSE,
            add.to.row =  addtorow,
            timestamp = NULL,
            floating = FALSE,
            file = file.path(dir_to_print, filename))
    }    
  }
}
