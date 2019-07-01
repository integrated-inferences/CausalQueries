#' Set confound
#'
#' Adjust parameter matrix to allow confounding
#'
#' @param model A model created by make_model()
#' @param confound A named list relating nodes to statements that identify causal types with which they are confounded
#' @export
#' @examples
#' model <- make_model("X -> Y") %>%
#'   set_confound(list(X = "(Y[X=1]>Y[X=0])",
#'                     X = "(Y[X=1]<Y[X=0])"))
#' plot_dag(model)
#' model <- make_model("A -> B -> C -> D; B -> D")
#' confound = list(A = "(D[A=., B=1, C=1]>D[A=., B=0, C=0])")
#' set_confound(model = model, confound = confound)
#'
# To do -- handle confound spread over previous allocations such as
#' model <- make_model("X -> Y")
#' confound = list(X = "(Y[X=1] == 1")
#' model <- set_confound(model = model, confound = confound)
#' confound2 = list(X = "(Y[X=1]>Y[X=0])")
#' model <- make_model("X -> Y <- S; S -> W") %>% set_confound(list(X = "S==1", S = "W[S=1]==1"))
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


    # Get a name for the new parameter: using dot separator
		top_digit <- ifelse(
			!any(startsWith(param_set, paste0(a, "."))), 0,
			max(as.numeric(sapply(param_set[startsWith(param_set, paste0(a, "."))],
							 function(j) strsplit(j, "[.]")[[1]][2])))
			)

		# Extend priors and parameters
		if(!is.null(model$priors)) model$priors <- 	c(model$priors[param_set == a], model$priors)
		if(!is.null(model$parameters)) model$parameters <- 	c(model$parameters[param_set == a], model$parameters)

		# Extend P: Make duplicate block of rows for each ancestor

		P             <- rbind(P[param_set == a,], P)

		new_param_name <- paste0(a,".", top_digit +1)

		new_params    <- paste0(pars[param_set == a], ".", top_digit +1)
		pars          <- c(new_params, pars)
    row.names(P) <- pars

    new_param_set <-  paste0(param_set[param_set == a], ".", top_digit +1)
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


	model$P <- P
	old_priors <- get_priors(model)
	priors <- make_priors(model)
  priors[names(old_priors)] = old_priors

  model$priors <- priors
	model

	}
