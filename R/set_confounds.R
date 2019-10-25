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
#' A statement of the form \code{list(X = "Y[X=1]==1")} can be interpreted as:
#' "Allow X to have a distinct conditional distribution when Y has types that involve Y[X=1]==1."
#' In this case nodal types for Y would continue to have 3 degrees of freedom.
#' But there would be parameters assigning the probability of X when t^Y = 01 or t^Y=11 and
#' other parameters for residual cases. Thus 6 degrees of freedom in all. This is still short of
#' an unconstrained distribution, though an  unconstrained distribution can be achieved with
#' repeated application of statements of this form, for instance via
#' \code{list(X = "Y[X=1]>Y[X=0]"), X = "Y[X=1]==Y[X=0]")}.
#'
#' Similarly a statement of the form \code{list(Y = "X==1")} can be interpreted as:
#' "Allow Y to have a distinct conditional distribution when X=1." In this case there would be
#' two distributions over nodal types for Y, producing 2*3 = 6 degrees of freedom.
#' Nodal types for X would continue to have 1 degree of freedom.
#' Thus 7 degrees of freedom in all, corresponding to a fully unconstrained joint distribution.
#'
#' @param model A model created by make_model()
#' @param confound A named list relating nodes to statements that identify causal types with which they are confounded
#' @export
#' @examples
#'
#' model <- make_model("X -> Y") %>%
#'   set_confound(list(X = "(Y[X=1]>Y[X=0])"))
#'
#' confound <- list(X = "(Y[X=1]>Y[X=0])",
#'                  X = "(Y[X=1]<Y[X=0])")
#'
#' model <- make_model("X -> Y") %>% set_confound(confound)
#'
#' confound = list(A = "(D[A=., B=1, C=1]>D[A=., B=0, C=0])")
#' model <- make_model("A -> B -> C -> D; B -> D") %>%
#'  set_confound(confound = confound)
#'
#' # Example where two parents are qwasconfounded
#' model <- make_model("A -> B <- C") %>%
#'   set_confound(list(A = "C==1")) %>%
#'   set_parameters(c(0,1,1,0, .5, .5, rep(.0625, 16)))
#' cor(simulate_data(model, n = 20))
#'
#' model <- make_model("X -> Y")
#' confound = list(X = "(Y[X=1] > Y[X=0])", X = "(Y[X=1] == 1)")
#' model <- set_confound(model = model, confound = confound)
#'
#' confound2 = list(X = "(Y[X=1]>Y[X=0])")
#' model <- make_model("X -> Y <- S; S -> W") %>%
#' set_confound(list(X = "S==1", S = "W[S=1]==1"))
#' attr(model$P, "confounds")

set_confound <-  function(model, confound = NULL){

	if(is.null(confound)) {message("No confound provided"); return(model)}
	if(is.null(model$P)) model <- set_parameter_matrix(model)

  P         <- model$P
	pars      <- rownames(P)
	types     <- colnames(P)

	# Check to see if any statements involve full confounding and redefine lists
	# checks <- lapply(confound, function(x) x %in% model$variables)

	# Origin (Node with conditional distribution)
	A  <- names(confound)

	# Descendant types
	D <-lapply(confound, function(x) {get_types(model, x)$type_list})

	# Squeeze in exploded lists if needed
	# if(any(checks)){
	# 	D <-lapply(confound[checks], function(x) {
	# 		if(x %in% model$variables) {explode(x)
	# 		} else {
	# 	get_types(model, x)$type_list
	# 		}})}
	# This figure out which causal types contain nodal types
	# ct <- get_causal_types(model)
	# causal_type_names <- colnames(P)
	# nt <- get_nodal_types(model, collapse = FALSE)
	# yt <- apply(nt$Y, 1, paste, collapse = "")
	# x <- lapply(yt, function (t) causal_type_names[ct$Y %in% t])
	# names(x) <- rep("X", length(x))

	for(j in 1:length(A)) {

		a <- A[j]   # param_name

    # Get a name for the new parameter: using hyphen separator to recognize previous confounding
		existing_ancestor <- startsWith(model$parameters_df$param_set, paste0(a, "-"))


		if(!any(existing_ancestor)) {
			model$parameters_df$param_set[model$parameters_df$param_set == a] <- paste0(a, "-", 1)
			top_digit = 1
		} else {
			top_digit <- max(as.numeric(sapply(model$parameters$param_set[existing_ancestor],
																						function(j) strsplit(j, "[-]")[[1]][2])))
		}

		# last param_set_name
		a1 <- paste0(a, "-", top_digit)

		# new param_set_name
		a2 <- paste0(a, "-", top_digit + 1)

		# Extend priors and parameters

		to_add <- model$parameters_df %>%
			dplyr::filter(param_set == a1) %>%
			dplyr::mutate(param_set  = a2,
									 param_names = paste(param_set, param, sep = "."))

		# Extend P: Make duplicate block of rows for each ancestor
		# Should contain all values from parameter family
		P_new <- data.frame(P[, ]) %>%
			filter(model$parameters_df$param_family == a) %>%
			group_split(model$parameters_df$param_set[model$parameters_df$param_family==a], keep = FALSE)
		P_new <- 	1*(Reduce(f = "+", P_new) >0)
		# Zero out duplicated entries: 1
		P_new[, !(types %in% D[[j]])] <- 0
		P   <- rbind(P_new, P)

		# Extend parameter_df
		model$parameters_df <- rbind(to_add,
																 mutate(model$parameters_df,
																 			 param_names = paste(param_set, param, sep = ".")))

		# Zero out duplicated entries: 2
		P[(model$parameters_df$param_family  == a) & (model$parameters_df$param_set!= a2),
			types %in% D[[j]]]    <- 0

	}

	# Clean up for export
	rownames(P) <- model$parameters_df$param_names
	to_keep <- apply(P, 1, sum)!=0
	model$parameters_df <- dplyr::filter(model$parameters_df, to_keep)
	P <- P[to_keep,]


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

	model$P     <- P
	model

	}


#' #' explode confound
#' #'
#' #' Helper to turn a confound of the form Y="X" into a list of type statements
#' #'
#' explode_confound <- function(W) {
#' 	new_list <-	lapply(unlist(model$nodal_types[W[[1]]]), function(j) j)
#' 	names(new_list) <- rep(names(W), length(new_list))
#' 	new_list
#' }
