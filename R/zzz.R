
.onLoad <- function(libname, pkgname) {
	# nocov start
    modules <- paste0("stan_fit4", names(stanmodels), "_mod")
    for (m in modules) loadModule(m, what = TRUE)

    # Set up parallel computation for Stan
    if (is.null(getOption("mc.cores"))) {
        cores <- parallel::detectCores()
        options(mc.cores = cores)
        packageStartupMessage(
            "CausalQueries: Stan parallel computation enabled with ",
            cores, " cores.\n",
            "To change: options(mc.cores = N)"
        )
    }
} # nocov end


utils::globalVariables(c("x", "y", "name"))
