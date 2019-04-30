#' Draw type probabilities after estimation
#'
#'
#' @param updated_model An updated model
#' @export

posterior_type_prob <- function(updated_model){
	P  <- get_parameter_matrix(updated_model)
	df <- rstan::extract(updated_model$posterior, pars= "lambdas")$lambdas
	apply(df, 1, function(j) draw_type_prob(model, P = P,  lambda = j))
}

#' Calculate posterior estimand
#'
#'
#' @param updated_model An updated model
#' @param subset quoted expression evaluates to logical statement. subset allows estimand to be conditioned on *observational* distribution.
#' @export
posterior_estimand <- function(updated_model,
															 do_1 = list(X = 1),
															 do_2 = list(X = 0),
															 q = function(Q1, Q2) Q1$Y == 1 & Q2$Y == 0,
															 subset = TRUE) {


	if(!is.logical(subset)) subset <- with(reveal_outcomes(updated_model),
																				 eval(parse(text = subset)))

	x <- as.matrix(q(reveal_outcomes(updated_model, dos = do_1),
									 reveal_outcomes(updated_model, dos = do_2)),
								 ncol = 1)[subset]
	Q <- posterior_type_prob(updated_model)[subset,]

	apply(Q, 2, function(wt) weighted.mean(x, wt))

}
