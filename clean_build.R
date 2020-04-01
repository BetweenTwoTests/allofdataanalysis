cleanFolder <- function(answer = NA) {
  while(!(answer %in% c('y', 'n'))) {
    answer <- readline(paste("Clean ", getwd(),"? (y/n)"))
  }
  if(answer == 'y') {
    rules <- c('.log', '.vrb', '.nav', '.snm', '.toc',
               '.md', '.rds',
               '-tikzDictionary', '.tex', '.synctex.gz')
    files_to_remove <- list.files(pattern = paste0('\\',rules ,'$', collapse = '|'))
    files_to_remove <- files_to_remove[!(files_to_remove %in% 
                                         c("README.md", "book.bib", "preamble.tex"))]
    file.remove(files_to_remove)
  }
}
cleanFolder("y")
