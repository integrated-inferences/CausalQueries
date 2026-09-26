library(pkgload)
load_all(".", quiet = TRUE)
m <- make_model("X -> M -> Y; X -> Y", legacy = FALSE)
params <- get_parameters(m)
q <- "Y[X=1] - Y[X=0]"
cases <- list(
  ALL = "ALL",
  obs = "X==1",
  do_given = "Y[X=1]==0",
  nested_given = "Y[X=1, M=M[X=0]]==1"
)
for (nm in names(cases)) {
  gv <- cases[[nm]]
  a <- CausalQueries:::twin_world_admit(m, q, gv, confound_supported = TRUE)
  message(sprintf(
    "%s: ok=%s reason=%s n=%s",
    nm, a$ok, ifelse(is.na(a$reason), "NA", a$reason), length(a$worlds)
  ))
  if (isTRUE(a$ok)) {
    grid <- as.numeric(query_distribution(
      m, q, given = gv, parameters = params, query_eval = "grid"
    ))
    st <- as.numeric(query_distribution(
      m, q, given = gv, parameters = params, query_eval = "ve_struct"
    ))
    message(sprintf("  grid=%g ve_struct=%g diff=%g", grid, st, abs(grid - st)))
  }
}
# nested query + nested given
q2 <- "Y[X=1, M=M[X=0]]"
gv2 <- "M[X=0]==1"
a2 <- CausalQueries:::twin_world_admit(m, q2, gv2, confound_supported = TRUE)
message(sprintf(
  "nested_q+given: ok=%s n=%s", a2$ok, length(a2$worlds)
))
if (isTRUE(a2$ok)) {
  grid <- as.numeric(query_distribution(
    m, q2, given = gv2, parameters = params, query_eval = "grid"
  ))
  st <- as.numeric(query_distribution(
    m, q2, given = gv2, parameters = params, query_eval = "ve_struct"
  ))
  message(sprintf("  grid=%g ve_struct=%g diff=%g", grid, st, abs(grid - st)))
}
