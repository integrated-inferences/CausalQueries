#' Get joint distribution of nodal types
#'
#' Identifies confounded nodal types. Default behavior draws a random admissible parameter vector.
#'
#' @param model A model made by make_model
#' @param parameters A parameter vector
#' @param generic_parameters Logical, require selection of random parameter.
#' @export
#' @examples
#' model <- make_model("X -> Y") %>%
#'   set_confound(list("X <-> Y"))
#'
#' get_nodal_joint_probability(model)
#'
#' # Default parameter values may not reveal dependencies
#' get_nodal_joint_probability(model, generic_parameters = FALSE)
#'

get_nodal_joint_probability <-
	function(model, parameters = NULL, generic_parameters = NULL) {

	parameters_df <- model$parameters_df

	# Figure parameters to use
	if(!is.null(parameters))	parameters <- gbiqq:::clean_param_vector(model,parameters)
	if(is.null(parameters)) {
		if(is.null(generic_parameters)) generic_parameters <- TRUE
		if(!generic_parameters)parameters <- get_parameters(model)
		if(generic_parameters) parameters <-
				gbiqq:::clean_param_vector(model,runif(nrow(parameters_df)))}

	nodal_type <- parameters_df$nodal_type
	P          <- data.frame(get_parameter_matrix(model))
	type_prob  <- get_type_prob(model, parameters = parameters)

	joint <- sapply(unique(nodal_type),
				 function(par1)
				 	{sapply(unique(nodal_type), function(par2) prob_par1_given_par2(par1, par2, nodal_type, P, type_prob))}
				 ) %>% data.frame(stringsAsFactors = FALSE)

	# Add in node and nodal_type
	select(parameters_df, param_family, nodal_type) %>%
		distinct %>%
		mutate(nodal_type = factor(nodal_type)) %>%
		right_join(cbind(nodal_type = (rownames(joint)), joint), by = "nodal_type")
}

#' Get confounds dataframe
#'
#' Identifies confounded nodal types. Default behavior draws a random admissible parameter vector.
#'
#' @param model A model made by make_model
#' @param ... arguments passed to get_nodal_joint_probability
#' @export
#' @examples
#' model <- make_model("X -> M -> Y; X <-> M")
#' make_confounds_df(model)
#'
#' model <- make_model("X -> M -> Y") %>%
#'   set_confound(list(X = "Y[X=1] > Y[X=0]"))
#' make_confounds_df(model)

make_confounds_df <- function(model, ...){

 nodes <- model$nodes
 joint_prob <- get_nodal_joint_probability(model, ...)
 param_family <- joint_prob$param_family
 joint_prob_mat <- dplyr::select(joint_prob, -c(param_family, nodal_type))

x <-  sapply(nodes, function(j) {
 	l <- lapply(nodes[nodes!=j], function(k) {
	 	M <-  filter(joint_prob_mat, param_family==j)[param_family==k]
	 	data.frame(j=j, k=k, confounded = any(!apply(M, 2, zero_range)))
	 	})
	do.call(rbind, l)}, simplify = FALSE)

confound_df <- do.call(rbind, x)

# Put in causal order
confound_df <- confound_df[confound_df$confounded, 1:2]
for(i in 1:nrow(confound_df)){
	confound_df[i,] <- (nodes[nodes %in% sapply(confound_df[i,], as.character)])
}

distinct(confound_df)

}


# helper from hadley
# https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
	if (length(x) == 1) return(TRUE)
	x <- range(x) / mean(x)
	isTRUE(all.equal(x[1], x[2], tolerance = tol))
}

#' helper to get conditional probability of nodal types
prob_par1_given_par2 <- function(par1, par2, nodal_type, P, type_prob){
	par1_in_type <- dplyr::filter(P, nodal_type %in% par1) %>% apply(2, function(j) any(j==1))
	par2_in_type <- dplyr::filter(P, nodal_type %in% par2) %>% apply(2, function(j) any(j==1))
	sum(type_prob[par1_in_type & par2_in_type])/	sum(type_prob[par2_in_type])
  }

