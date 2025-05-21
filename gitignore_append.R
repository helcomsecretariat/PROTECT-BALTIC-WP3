l <- list.files(full.names = TRUE, recursive = TRUE)
l <- l[sapply(l, file.size) >= 100*1000000] # MB
l <- gsub("^\\./", "", l)
gitfile <- file(".gitignore", open = "a")
writeLines(l, gitfile)
close(gitfile)
