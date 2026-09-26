library(pkgload)
load_all(".", quiet = TRUE)
m <- suppressMessages(make_model(
  paste(
    "Marginalization -> Participation;",
    "Marginalization -> Understanding;",
    "Marginalization -> Inclusion;",
    "Marginalization -> Connections;",
    "Invitation -> Participation;",
    "Municipality -> Participation;",
    "Municipality -> Understanding;",
    "Municipality -> Inclusion;",
    "Municipality -> Connections;",
    "Participation -> Understanding;",
    "Participation -> Connections;",
    "Participation -> Inclusion;",
    "Inclusion -> Trust;",
    "Understanding -> Trust;",
    "Connections -> Trust"
  ),
  monotone = "m",
  drop_interactions = 3,
  legacy = FALSE
))
q <- "Trust[Marginalization=1] - Trust[Marginalization=0]"
t0 <- system.time({
  query_model(m, q, query_eval = "ve_struct", using = "parameters")
})[["elapsed"]]
set.seed(1)
options(CausalQueries.ve_struct_draw_chunk = 256)
t1 <- system.time({
  q1 <- query_model(m, q, query_eval = "ve_struct", using = "priors", n_draws = 256)
})[["elapsed"]]
message(sprintf(
  "param=%.2f prior256=%.2f ratio=%.1f mean=%.4f",
  t0, t1, t1 / max(t0, 1e-9), q1[["mean"]]
))
