
gbiqq_designer <- function(
	dag = make_model("X" %->% "Y"),
	restrictions = NULL,
	priors = "uniform",
	data_strategy = list(n = 1,vars = list(NULL), probs = list(NULL), ms = NULL, subsets = list(NULL)),
	estimand = "Y[X=1] - Y[X=0]"
		#'    vars = list(c("X", "Y"), "M"),
		#'    probs = list(1, .5),
		#'    subsets = list(NULL, "X==1 & Y==0"))
) {1}
