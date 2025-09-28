
.onLoad <- function(libname, pkgname) {
	# nocov start
    modules <- paste0("stan_fit4", names(stanmodels), "_mod")
    for (m in modules) loadModule(m, what = TRUE)

    # Advise on parallel computation for Stan
    if (is.null(getOption("mc.cores"))) {
        cores <- parallel::detectCores()
        packageStartupMessage(
            "CausalQueries: For large problems, consider enabling parallel computation.\n",
            "Available cores: ", cores, ". To enable: options(mc.cores = ", cores, ")"
        )
    }
} # nocov end


utils::globalVariables(c("x", "y", "name"))
