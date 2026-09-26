path <- "R/twin_ve_struct.R"
text <- readLines(path, warn = FALSE)
marker <- "#' Structural elimination: weighted states over twin endogenous values."
idx <- which(text == marker)[1]
if (is.na(idx)) stop("marker not found")
head <- text[seq_len(idx - 1L)]

tail <- readLines("scratch/twin_ve_tail.R", warn = FALSE)
writeLines(c(head, tail), path)
message("Wrote ", path, " (", length(head) + length(tail), " lines)")
