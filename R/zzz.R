
.onLoad <- function(libname, pkgname) {
	# nocov start
    modules <- paste0("stan_fit4", names(stanmodels), "_mod")
    for (m in modules) loadModule(m, what = TRUE)

    # Advise on parallel computation for Stan (only in main process)
    if (is.null(getOption("mc.cores"))) {
        # Check if we're in a parallel worker process
        is_parallel_worker <- tryCatch({
            exists("parallel:::mcexit") && exists(".Random.seed") && 
            !is.null(getOption("parallel.par.procname"))
        }, error = function(e) FALSE)
        
        # Also check for common parallel worker indicators
        is_worker <- is_parallel_worker || 
                    !is.null(getOption("parallel.par.procname")) ||
                    exists("parallel:::mcexit") ||
                    Sys.getenv("R_PARALLEL") != ""
        
        if (!is_worker) {
            cores <- parallel::detectCores()
            packageStartupMessage(
                "CausalQueries: For large problems, consider enabling parallel computation.\n",
                "Available cores: ", cores, ". To enable: options(mc.cores = ", cores, ")"
            )
        }
    }
} # nocov end


utils::globalVariables(c("x", "y", "name"))
