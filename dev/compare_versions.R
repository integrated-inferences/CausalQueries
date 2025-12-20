
# One-time: install CRAN version into its own library
dir_cran <- tempfile("cranlib_"); dir.create(dir_cran)
remotes::install_version("CausalQueries", version = "1.4.3", lib = dir_cran, upgrade = "never")

# Load dev version in the current process
devtools::load_all(".")

# Helper: run expression in CRAN version
call_cran_fun <- function(fun, ..., lib = dir_cran) {
  args <- list(...)
  callr::r(
    function(fun, args, lib) {
      .libPaths(c(lib, .libPaths()))
      library(CausalQueries)  # CRAN
      do.call(fun, args)
    },
    args = list(fun, args, lib)
  )
}

time_dev_vs_cran_micro <- function(fun, ..., lib = dir_cran, times = 5L) {
  args <- list(...)

  # dev timing (current process)
  if (!requireNamespace("microbenchmark", quietly = TRUE)) stop("Install microbenchmark")
  mb_dev <- microbenchmark::microbenchmark(do.call(fun, args), times = times)
  res_dev <- do.call(fun, args)

  # cran timing (inside child)
  cran <- callr::r(function(fun, args, lib, times) {
    .libPaths(c(lib, .libPaths()))
    library(CausalQueries)
    if (!requireNamespace("microbenchmark", quietly = TRUE)) stop("Install microbenchmark in child")
    mb <- microbenchmark::microbenchmark(do.call(fun, args), times = times)
    res <- do.call(fun, args)
    list(summary = summary(mb)[, c("min","lq","median","uq","max","mean")], result = res)
  }, args = list(fun, args, lib, times))

  list(
    dev  = list(summary = summary(mb_dev)[, c("min","lq","median","uq","max","mean")], result = res_dev),
    cran = cran
  )
}


# Define once: fairly heavy model
pipe_fun <- function(...) make_model("A -> B <- C <- A; B <-> C") |> update_model()


# Compare + time
res_dev  <- pipe_fun()
res_cran <- call_cran_fun(pipe_fun)

timed    <- time_dev_vs_cran_micro(pipe_fun, times = 4)

timed$dev$summary
timed$cran$summary
