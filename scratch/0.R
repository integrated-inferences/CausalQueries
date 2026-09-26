
library(CausalQueries)

theory_model <- make_model(
"
Marginalization -> Participation;
Marginalization -> Understanding;
Marginalization -> Inclusion;
Marginalization -> Connections;
Invitation -> Participation;
Municipality -> Participation;
Municipality -> Understanding;
Municipality -> Inclusion;
Municipality -> Connections;
Participation -> Understanding;
Participation -> Connections;
Participation -> Inclusion;
Inclusion  -> Trust;
Understanding  -> Trust;
Connections  -> Trust"
   , monotone = "m",
drop_interactions = 3)


q0 <- query_model(theory_model, "Trust[Marginalization=1] - Trust[Marginalization=0]")

q0 <- query_model(theory_model, "Trust[Marginalization=1] - Trust[Marginalization=0]", query_eval = "ve_struct")
q1 <- query_model(theory_model, "Trust[Marginalization=1] - Trust[Marginalization=0]", using = "priors")
