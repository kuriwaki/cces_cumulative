


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
  
  # if a grid question more than zero. 1 means only one question
  nQs <- ifelse(qwording$nSubQuestions[i] == 0, 1, qwording$nSubQuestions[i])
  
  # alias and name
  alias <- qwording$alias[i]
  name <- qwording$name[i]
  
  # counts
  var.tab <- crtabs(paste0("~ ", alias), ds, useNA = "ifany")
  var.arr <- var.tab@arrays$.unweighted_counts
  if (class(var.arr) == "array") var.arr <- matrix(var.arr, nrow = 1)
  
  # choices
  cat.obj <- meta[[name]]@body$categories
  choicenames.vec <- sapply(cat.obj, "[", "name") %>% unlist() %>% as.character()
  choiceno.vec <- sapply(cat.obj, "[", "id") %>% unlist() %>% as.integer()
  
  
  # if a grid, get alias for each question
  if (nQs > 1) {
    subcat.obj <- meta[[name]]@body$subreferences
    q.aliases <- sapply(subcat.obj, "[", "alias") %>% unlist() %>% as.character()
    q.names <- sapply(subcat.obj, "[", "name") %>% unlist() %>% as.character()
  } else {
    q.aliases <- alias
    q.names <- name
  }
  
  list(alias = q.aliases,
       name = q.names,
       wording = qwording$wording[i],
       counts = var.arr,
       level = choiceno.vec,
       labels = choicenames.vec)
  
  
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
