old_wd <- getwd()

setwd("vignettes/")

vignettes <- c(
  "a-getting-started",
  "b-plotting",
  "c-canonical-models",
  "d-front-door",
  "e-posteriors"
)

for(v in vignettes) {
  knitr::knit(paste(v, ".Rmd.orig", sep = ""), output = paste(v, ".Rmd", sep = ""))
  knitr::purl(paste(v, ".Rmd.orig", sep = ""), output = paste(v, ".R", sep = ""))
}

setwd(old_wd)
