#' Set confound
#'
#' Adjust parameter matrix to allow confounding.
#'
#'
#' Confounding between X and Y arises when the nodal types for X and Y are not independently distributed. In the X -> Y graph, for instance, there are 2 nodal types for X and 4 for Y. There are thus 8 joint nodal types:
#' \preformatted{
#' |          | t^X                |                    |           |
#' |-----|----|--------------------|--------------------|-----------|
#' |     |    | 0                  | 1                  | Sum       |
#' |-----|----|--------------------|--------------------|-----------|
#' | t^Y | 00 | Pr(t^X=0 & t^Y=00) | Pr(t^X=1 & t^Y=00) | Pr(t^Y=00)|
#' |     | 10 | .                  | .                  | .         |
#' |     | 01 | .                  | .                  | .         |
#' |     | 11 | .                  | .                  | .         |
#' |-----|----|--------------------|--------------------|-----------|
#' |     |Sum | Pr(t^X=0)          | Pr(t^X=1)          | 1         |
#' }
#'
#' This table has 8 interior elements and so an unconstrained joint distribution would have 7 degrees of freedom.
#' A no confounding assumption means that Pr(t^X | t^Y) = Pr(t^X), or  Pr(t^X, t^Y) = Pr(t^X)Pr(t^Y). In this case there would be 3 degrees of freedom for Y and 1 for X, totalling 4 rather than 7.
#'
#' \code{set_confounds} lets you relax this assumption by increasing the number of parameters characterizing the joint distribution. Using the fact that P(A,B) = P(A)P(B|A) new parameters are introduced to capture P(B|A=a) rather than simply P(B).
#'
#' A statement of the form \code{list(X = "Y[X=1]==1")} can be interpreted as: "Allow X to have a distinct conditional distribution when Y has types that involve Y[X=1]==1. In this case nodal types for Y would continue to have 3 degrees of freedom. But there would be parameters assigning the probability of X when t^Y = 01 or t^Y=11 and other parameters for residual cases. Thus 6 degrees of freedom in all. This is still short of an unconstrained distribution, though an  unconstrained distribution can be achieved with repeated application of statements of this form, for instance via  \code{list(X = "Y[X=1]>Y[X=0]"), X = "Y[X=1]==Y[X=0]")}.
#'
#' Similarly a statement of the form \code{list(Y = "X==1")} can be interpreted as: "Allow Y to have a distinct conditional distribution when X=1. In this case there would be two distributions over nodal types for Y, producing 2*3 = 6 degrees of freedom. Nodal types for X would continue to have 1 degree of freedom. Thus 7 degrees of freedom in all, corresponding to a fully unconstrained joint distribution.
#'
#' @param model A model created by make_model()
#' @param confound A named list relating nodes to statements that identify causal types with which they are confounded
#' @export
#' @examples
#' library(dplyr)
#' model <- make_model("X -> Y") %>%
#'   set_confound(list(X = "(Y[X=1]>Y[X=0])",
#'                     X = "(Y[X=1]<Y[X=0])"))
#' plot_dag(model)
#'
#' confound = list(A = "(D[A=., B=1, C=1]>D[A=., B=0, C=0])")
#' model <- make_model("A -> B -> C -> D; B -> D") %>%
#'  set_confound(confound = confound)
#'
#' #To do -- handle confound spread over previous allocations such as
#' model <- make_model("X -> Y")
#' confound = list(X = "(Y[X=1] == 1)")
#' \dontrun{model <- set_confound(model = model, confound = confound)}
#' confound2 = list(X = "(Y[X=1]>Y[X=0])")
#' model <- make_model("X -> Y <- S; S -> W") %>%
#' set_confound(list(X = "S==1", S = "W[S=1]==1"))
#' attr(model$P, "confounds")

set_confound <-  function(model, confound = NULL){

	if(is.null(confound)) {message("No confound provided"); return(model)}
	if(is.null(model$P)) model <- set_parameter_matrix(model)

  P         <- model$P
	param_set <- attr(P, "param_set")
	pars      <- rownames(P)
	types     <- colnames(P)

	# Ancestors
	A  <- names(confound)

	# Descendant types
	D <-lapply(confound, function(x) {get_types(model, x)$type_list})

	# Ds <- lapply(D, function(k)
	# 	unique(sapply(k[[1]], function(j) strsplit(j, "[.]")[[1]][2])))

	for(j in 1:length(A)) {
		a <- A[j]   # param_name


    # Get a name for the new parameter: using hyphen separator to recognize previous confounding
		existing_ancestor <- startsWith(param_set, paste0(a, "-"))

		top_digit <- ifelse(
			!any(existing_ancestor), 0,
			max(as.numeric(sapply(param_set[existing_ancestor],
							 function(j) strsplit(j, "[-]")[[1]][2])))
			)

		# Extend priors and parameters
		if(!is.null(model$priors)) model$priors <-
			c(model$priors[param_set == a], model$priors)

		if(!is.null(model$parameters)) model$parameters <-
			c(model$parameters[param_set == a], model$parameters)

		# Extend P: Make duplicate block of rows for each ancestor

		P             <- rbind(P[param_set == a,], P)

		new_param_name <- paste0(a,"-", top_digit +1)

#		new_params    <- paste0(pars[param_set == a], "-", top_digit +1)
		new_params    <- pars[param_set == a]
		pars          <- c(new_params, pars)
		names(pars)   <- param_set
    row.names(P)  <- pars

    new_param_set <-  paste0(param_set[param_set == a], "-", top_digit +1)
		param_set     <- c(new_param_set,param_set)

		# Zero out duplicated entries
		P[param_set == A[j],            types %in% D[[j]]]    <- 0
		P[param_set %in% new_param_set, !(types %in% D[[j]])] <- 0

	}

	# Clean up for export
	P <- P[apply(P, 1, sum)!=0,]
	attr(P, "param_set") <- param_set


	# Make a dataset of ancestor to descendant confound relations
	V <- lapply(confound, var_in_query, model= model)
	confounds_df <- data.frame(
		ancestor = rep(as.vector(names(V)), times = as.vector(unlist(lapply(V, length)))),
	  descendant = unlist(lapply(V, unlist)), stringsAsFactors = FALSE)
	confounds_df <- confounds_df[confounds_df$ancestor != confounds_df$descendant,]
	rownames(confounds_df) <- NULL

	if(!is.null(attr(P, "confounds"))) confounds_df <- rbind(attr(P, "confounds"), confounds_df)
	confounds_df <- confounds_df[!duplicated(confounds_df),]

	attr(P, "confounds") <- confounds_df

	attr(P, "param_family") <- param_family(P, model)


	model$P     <- P

	# Clean up priors: Use flat priors for new parameters
	# and retain any previously specified priors
	old_priors  <- get_priors(model)
	priors      <- make_priors(model)
  priors[names(old_priors)] <- old_priors
  model$priors <- priors

	model

	}
