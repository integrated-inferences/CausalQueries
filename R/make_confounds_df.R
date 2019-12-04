#' Make a confounds dataframe
#'
#' Identifies confounded nodal types.
#'
#' @param model A model made by make_model
#' @export
#' @examples
#' model <- make_model("X -> Y") %>%
#' set_confound("X <-> Y", add_confounds_df  = FALSE)
#' make_confounds_df(model)
#'
#' model <- make_model("X -> M -> Y; X <-> Y") %>%
#' set_restrictions(c("M[X=1] == M[X=0]", "Y[M=1]==Y[M=0]"))
#' make_confounds_df(model)
#'
#' model <- make_model("X -> M -> Y; X <-> M; M <-> Y") %>%
#' set_restrictions(c("M[X=1] == M[X=0]", "Y[M=1]==Y[M=0]"))
#' make_confounds_df(model)
#'
#' model <- make_model("X -> M -> Y") %>%
#'   set_confound(list(X = "Y[X=1] > Y[X=0]"), add_confounds_df = FALSE)
#' make_confounds_df(model)
#'
#' model <- make_model("X -> M -> Y")
#' make_confounds_df(model)
#'
#' model <- make_model("X -> Y") %>%
#'   set_confound(list(X = "X==1"))
#'
#' model <- make_model("Y2 <- X -> Y1") %>%
#'  set_restrictions(c("(Y1[X=1] == Y1[X=0])", "(Y2[X=1] == Y2[X=0])")) %>%
#'  set_confound(list("X <-> Y1", "X <-> Y2"))
#'
#' model$P
#' model$confounds_df
#'
#' # Hard case 1: OK
#' model <- make_model("A -> X <- B ; A <-> X; B <-> X")
#' table(model$parameters_df$param_set)
#' model$confounds_df
#'
#' # Hard case 2: Not OK
#' model <- make_model("A <- X -> B; A <-> X; B <-> X") %>%
#' set_restrictions(c("A[X=0] == A[X=1]", "B[X=0] == B[X=1]"))
#' table(model$parameters_df$param_set)
#' model$confounds_df


make_confounds_df <- function(model){

	if(is.null(model$P)) {message("No confounding"); return(NULL)}

	if(any(apply(model$P, 1, sum)==0)) warning("Some rows in model$P sum to 0.
																						 Likely indicative of malformed confounds
																						 (such as the distribution of X depends on X)
																						 confounds_df may not be reliable.")
	nodes <- model$nodes
  n <- nrow(model$P)

	joint_zero <-  (apply(model$P, 1, function(j) apply(as.matrix(model$P[,j==1], nrow = n), 1, sum)==0)*1) %>%
		data.frame

	param_family <- model$parameters_df$param_family

  # This produces a nodes * nodes matrix of confounding
		x <-  sapply(nodes, function(j) {
		         sapply(nodes, function(k) {
			sum(joint_zero[param_family==j, param_family==k])})})
	  diag(x) <- 0

	  # This reshapes into a 2 column df
		x <- which(x>0, arr.ind = TRUE)
		if(sum(x) == 0) return(NA)

		confound_df <- matrix(nodes[x], nrow(x)) %>% data.frame()

	# Put in causal order
	for(i in 1:nrow(confound_df)){
		confound_df[i,] <- (nodes[nodes %in% sapply(confound_df[i,], as.character)])
	}

	distinct(confound_df)

}


#' Set a confounds_df
#'
#' Normally a confounds_df is added to a model whenever confounding is set.
#' The confounds_df can be manually provided however using set_sconfounds_df.
#'
#' @param model A model made by make_model
#' @export
#' @examples
#' model <- make_model("X -> Y") %>%
#'   set_confound(list("X <-> Y"), add_confounds_df = FALSE)
#' model$confounds_df
#' set_confounds_df(model)$confounds_df
#'
#' # An example where a confounds dataframe needs to be updated manually
#' # Here a restriction applied after a confounding relation is set removes the confounding
#' model <- make_model("X -> Y") %>%
#'   set_confound(list(X = "(Y[X=1] > Y[X=0])")) %>%
#'   set_restrictions("(Y[X=1] > Y[X=0])")
#' model$confounds_df  # Incorrect
#' model <- set_confounds_df(model)
#' model$confounds_df  # Correct
#' # plot(model)

set_confounds_df <- function(model) {

	model$confounds_df <- make_confounds_df(model)
	model
	}



#' Get joint distribution of nodal types
#'
#' Identifies possible conditional probabilities of nodal types. May be used to identify patterns of non independence.
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
										{sapply(unique(nodal_type), function(par2)
											prob_par1_given_par2(par1, par2, nodal_type, P, type_prob))}
		) %>% data.frame(stringsAsFactors = FALSE)

		# Add in node and nodal_type
		select(parameters_df, param_family, nodal_type) %>%
			distinct %>%
			mutate(nodal_type = factor(nodal_type)) %>%
			right_join(cbind(nodal_type = (rownames(joint)), joint), by = "nodal_type")
	}

#' helper to get conditional probability of nodal types
#'
#' @param par1 parameter 1
#' @param par2 parameter 1
#' @param nodal_type vector of nodal types
#' @param P P matrix
#' @param type_prob vector of type probabilities
#'
prob_par1_given_par2 <- function(par1, par2, nodal_type, P, type_prob){
	par1_in_type <- dplyr::filter(P, nodal_type %in% par1) %>%
		apply(2, function(j) any(j==1))
	par2_in_type <- dplyr::filter(P, nodal_type %in% par2) %>%
		apply(2, function(j) any(j==1))
	sum(type_prob[par1_in_type & par2_in_type])/	sum(type_prob[par2_in_type])
}


