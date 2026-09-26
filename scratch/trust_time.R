library(pkgload)
load_all(".", quiet = TRUE)
message("loaded")
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
message("made")
q <- "Trust[Marginalization=1] - Trust[Marginalization=0]"
t0 <- system.time({
  q0 <- query_model(m, q, query_eval = "ve_struct", using = "parameters")
})[["elapsed"]]
message(sprintf("param %.2fs mean=%g", t0, q0[["mean"]]))
set.seed(1)
options(CausalQueries.ve_struct_draw_chunk = 4)
t1 <- system.time({
  q1 <- query_model(m, q, query_eval = "ve_struct", using = "priors", n_draws = 8)
})[["elapsed"]]
message(sprintf(
  "prior8 %.2fs mean=%g sd=%g ratio=%.1f",
  t1, q1[["mean"]], q1[["sd"]], t1 / max(t0, 1e-9)
))
