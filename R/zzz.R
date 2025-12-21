.onLoad <- function(libname, pkgname) {
  # nocov start
  modules <- paste0("stan_fit4", names(stanmodels), "_mod")
  for (m in modules) loadModule(m, what = TRUE)
  # nocov end
}


.onAttach <- function(libname, pkgname) {
  if (is.null(getOption("mc.cores"))) {

    is_parallel_worker <- tryCatch({
      exists("parallel:::mcexit") &&
        !is.null(getOption("parallel.par.procname"))
    }, error = function(e) FALSE)

    is_worker <- is_parallel_worker ||
      !is.null(getOption("parallel.par.procname")) ||
      exists("parallel:::mcexit") ||
      Sys.getenv("R_PARALLEL") != ""

    if (!is_worker) {
      cores <- parallel::detectCores()
      packageStartupMessage(
        "CausalQueries: For large problems, consider enabling parallel computation.\n",
        "Available cores: ", cores,
        ". To enable: options(mc.cores = ", cores, ")"
      )
    }
  }
}


utils::globalVariables(c("x", "y", "name"))
