
# Code taken from rstanarm

.onLoad <- function(libname, pkgname) { # nocov start
  modules <- paste0("stan_fit4", names(stanmodels), "_mod")
  for (m in modules) loadModule(m, what = TRUE)
} # nocov end

.onAttach <- function(...) {
  gbiqqLib <- dirname(system.file(package = "gbiqq"))
  pkgdesc <- suppressWarnings(utils::packageDescription("gbiqq", lib.loc = gbiqqLib))
  if (length(pkgdesc) > 1) {
    builddate <- gsub(';.*$', '', pkgdesc$Packaged)
    packageStartupMessage(paste("gbiqq (Version ", pkgdesc$Version, ", packaged: ", builddate, ")", sep = ""))
  }

  packageStartupMessage("- For execution on a local, multicore CPU with excess RAM we recommend calling")
  packageStartupMessage("options(mc.cores = parallel::detectCores())")

}

