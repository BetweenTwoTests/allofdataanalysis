# https://github.com/rstudio/bookdown/issues/418#issuecomment-314265432

# detach packages
packages = sapply(sessionInfo()$otherPkgs, function(x) x$Package)
packages = packages[packages != "bookdown"]
packages = sapply(packages, function(p) paste0("package:", p))
lapply(packages, detach, character.only = TRUE, unload = TRUE)
# clear environment
rm(list = ls())