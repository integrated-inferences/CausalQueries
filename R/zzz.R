
.onLoad <- function(libname, pkgname) {
	# nocov start
    modules <- paste0("stan_fit4", names(stanmodels), "_mod")
    for (m in modules) loadModule(m, what = TRUE)
} # nocov end


utils::globalVariables(c("x", "y", "name"))
