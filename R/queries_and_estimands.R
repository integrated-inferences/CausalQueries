#' Calculate estimand
#'
#' Calculated from a prior or posterior distribution
#'
#'
#' @param model A  model
#' @param posterior if true use a posterior distribution, otherwise use the prior
#' @param subset quoted expression evaluates to logical statement. subset allows estimand to be conditioned on *observational* distribution.
#' @param type_distribution if provided saves calculatio, otherwise clculated from model; may be based on prior or posterion
#' @export
#' @examples
#' model <- make_model("X" %->% "Y")
#' model <- reduce_nodal_types(model, restrictions = list(Y = "Y10"))
#' model <- add_prior_distribution(model)
#' summary(calculate_estimand(model))

calculate_estimand <- function(model,
															 posterior = FALSE,
															 do_1 = list(X = 1),
															 do_2 = list(X = 0),
															 q = function(Q1, Q2) Q1$Y == 1 & Q2$Y == 0,
															 subset = TRUE,
															 type_distribution = NULL) {


	if(!is.logical(subset)) subset <- with(reveal_outcomes(model),
																				 eval(parse(text = subset)))
	if(all(!subset)) return(0)

	x <- as.matrix(q(reveal_outcomes(model, dos = do_1),
									 reveal_outcomes(model, dos = do_2)),
								 ncol = 1)[subset]

	if(is.null(type_distribution)) type_distribution <-
		draw_type_prob_multiple(model, posterior = posterior)[subset,]

	apply(type_distribution, 2, function(wt) weighted.mean(x, wt))

}


#' Calculate multipl estimands
#'
#' Calculated from a prior or posterior distribution
#'
#'
#' @param model A  model
#' @param posterior if true use a posterior distribution, otherwise use the prior
#' @param subset quoted expression evaluates to logical statement. subset allows estimand to be conditioned on *observational* distribution.
#' @export
#' @examples
#' model <- make_model("X" %->% "Y")
#' model <- add_prior_distribution(model)
#' calculate_multiple_estimands(model,
#'            posteriors = FALSE,
#'            dos = list(op_1 =  list(do_1 = list(X = 1), do_2 = list(X = 0)),
#'											 op_2 =  list(do_1 = list(X = 1), do_2 = list(X = 0))),
#'						qs =  list(q1   =  function(Q1, Q2) Q1$Y -  Q2$Y,
#'											 q2   =  function(Q1, Q2) Q1$Y == 1 & Q2$Y == 0),
#'						subsets = TRUE,
#'						estimand_labels = c("ATE", "Share_positive"))
#'
calculate_multiple_estimands <- function(model = make_model("X" %->% "Y"),
																				 posteriors = list(FALSE),
																				 dos = list(op_1 =  list(do_1 = list(X = 1), do_2 = list(X = 0))
																				 ),
																				 qs =  list(q1   =  function(Q1, Q2) Q1$Y == 1 & Q2$Y == 0
																				 ),
																				 subsets = list(TRUE),
																				 estimand_labels = NULL,
																				 stats = c(mean = mean, sd = sd)){

	f <- function(posterior, do, q, subset){
		v <-calculate_estimand(model, posterior = posterior, do_1 = do[[1]], do_2 = do[[2]],q = q, subset = subset)
		sapply(stats, function(g) g(v))
	}

	out <- mapply(f, posteriors, dos, qs, subsets)
	rownames(out) <- paste(names(stats))
	if(!is.null(estimand_labels)) colnames(out) <- estimand_labels
	data.frame(out)
}
