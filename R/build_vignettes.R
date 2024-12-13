#' vignettes: a character vector vignette names of the vignettes you wish to
#'  build. Please write the vignette name without any file extension i.e.
#'  instead of "vignette_name.Rmd" please specify "vignette_name".
#'  When updating an existing vignette simply call `build_vignettes()`
#'  When writing a new vignette add your vignette name as an optional argument
#'  to the function call i.e. `build_vignettes("vignette-name")`

build_vignettes <- function(vignettes = c("a-getting-started", "b-plotting", "c-canonical-models", "d-front-door", "e-posteriors"), ...) {

  call <- match.call()
  if ("vignettes" %in% names(call)) {
    vignettes <- c("a-getting-started",
                   "b-plotting",
                   "c-canonical-models",
                   "d-front-door",
                   "e-posteriors",
                   eval(call$vignettes)
                  )
  }

  old_wd <- getwd()

  setwd("vignettes/")

  for(v in vignettes) {
    knitr::knit(paste(v, ".Rmd.orig", sep = ""), output = paste(v, ".Rmd", sep = ""))
    knitr::purl(paste(v, ".Rmd.orig", sep = ""), output = paste(v, ".R", sep = ""))
  }

  setwd(old_wd)
}
