#' Make a confounds dataframe
#'
#' Identifies confounded nodal types.
#'
#' @param model A model made by make_model
#' @export
#' @examples
#' model <- make_model('X -> Y') %>%
#' set_confound('X <-> Y', add_confounds_df  = FALSE)
#' make_confounds_df(model)
#'
#' model <- make_model('X -> M -> Y; X <-> Y') %>%
#' set_restrictions(c('M[X=1] == M[X=0]', 'Y[M=1]==Y[M=0]'))
#' make_confounds_df(model)
#'
#' model <- make_model('X -> M -> Y; X <-> M; M <-> Y') %>%
#' set_restrictions(c('M[X=1] == M[X=0]', 'Y[M=1]==Y[M=0]'))
#' make_confounds_df(model)
#'
#' # The implied confounding is between X and M and also between X and Y
#' model <- make_model('X -> M -> Y') %>%
#'   set_confound(list(X = 'Y[X=1] > Y[X=0]'), add_confounds_df = FALSE)
#' make_confounds_df(model)
#'
#' model <- make_model('X -> M -> Y')
#' make_confounds_df(model)
#'
#' # Bad case
#' \dontrun{
#' model <- make_model('X -> Y') %>%
#'   set_confound(list(X = 'X==1'))
#'}
#'
#' # Complex confounding 1
#' model <- make_model('A -> X <- B ; A <-> X; B <-> X')
#' model$confounds_df
#'
#' # Complex confounding 2
#' model <- make_model('A <- X -> B; A <-> X; B <-> X') %>%
#' set_restrictions(c('A[X=0] == A[X=1]', 'B[X=0] == B[X=1]'))
#' table(model$parameters_df$param_set)
#' model$confounds_df
#'
#' # Full confounding: X, A|X, B|A,X with 7 degrees of freedom
#' model <- make_model('A <- X -> B; A <-> X; B <-> X; A<->B') %>%
#' set_restrictions(c('A[X=0] == A[X=1]', 'B[X=0] == B[X=1]'))
#' table(model$parameters_df$param_set)
#' model$confounds_df

make_confounds_df <- function(model) {

    if (is.null(model$P)) {
        message("No confounding")
        return(NULL)
    }

    if (any(apply(model$P, 1, sum) == 0))
        warning("
\tSome rows in model$P sum to 0. Likely indicative of malformed confounds
\t(such as the distribution of X depends on X) confounds_df may not be reliable.")

    # Correlations of the rows of the P matrix capture the qualitative nature of correlations of
    # parameters
    par_corr <- cor(t(model$P))
    par_corr <- round(par_corr, 12)  ## To avoid false positives from tiny correlation estimates

    # Check if these correlations are *differentially conditional* within a param_set Key action is done
    # by distinct: we partition into submatrices and see if the rows are different
    pars <- model$parameters_df$param_set
    sets <- unique(pars)
    x <- 1 * sapply(sets, function(i) {
        sapply(sets, function(j) {
            (as.data.frame(par_corr[pars == i, pars == j]) %>% distinct() %>% nrow) > 1
        })
    })
    diag(x) <- 0

    # We now aggregate to the node level to make a nodes*nodes matrix
    node_map <- (select(model$parameters_df, node, param_set) %>% distinct)$node
    nodes <- unique(node_map)
    x <- sapply(nodes, function(i) {
        sapply(nodes, function(j) {
            sum(x[node_map == i, node_map == j])
        })
    })


    # This reshapes into a 2 column df
    x <- which(x > 0, arr.ind = TRUE)
    if (sum(x) == 0)
        return(NA)

    confound_df <- matrix(nodes[x], nrow(x)) %>% data.frame(stringsAsFactors = FALSE)

    # Put in causal order
    for (i in 1:nrow(confound_df)) {
        confound_df[i, ] <- (nodes[nodes %in% sapply(confound_df[i, ], as.character)])
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
#' model <- make_model('X -> Y') %>%
#'   set_confound(list('X <-> Y'), add_confounds_df = FALSE)
#' model$confounds_df
#' set_confounds_df(model)$confounds_df
#'
#' # An example where a confounds dataframe needs to be updated manually
#' # Here a restriction applied after a confounding relation is set removes the confounding
#' model <- make_model('X -> Y') %>%
#'   set_confound(list(X = '(Y[X=1] > Y[X=0])')) %>%
#'   set_restrictions('(Y[X=1] > Y[X=0])')
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
#' model <- make_model('X -> Y') %>%
#'   set_confound(list('X <-> Y'))
#'
#' get_nodal_joint_probability(model)
#'

get_nodal_joint_probability <- function(model, parameters = NULL, generic_parameters = NULL) {

    parameters_df <- model$parameters_df

    # Figure parameters to use
    if (!is.null(parameters))
        parameters <- gbiqq:::clean_param_vector(model, parameters)
    if (is.null(parameters)) {
        if (is.null(generic_parameters))
            generic_parameters <- TRUE
        if (!generic_parameters)
            parameters <- get_parameters(model)
        if (generic_parameters)
            parameters <- gbiqq:::clean_param_vector(model, runif(nrow(parameters_df)))
    }

    nodal_type <- parameters_df$nodal_type
    P <- data.frame(get_parameter_matrix(model))
    type_prob <- get_type_prob(model, parameters = parameters)

    joint <- sapply(unique(nodal_type), function(par1) {
        sapply(unique(nodal_type), function(par2) prob_par1_given_par2(par1, par2, nodal_type, P, type_prob))
    }) %>% data.frame(stringsAsFactors = FALSE)

    # Add in node and nodal_type
    select(parameters_df, node, nodal_type) %>% distinct %>% mutate(nodal_type = factor(nodal_type)) %>%
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
prob_par1_given_par2 <- function(par1, par2, nodal_type, P, type_prob) {
    par1_in_type <- dplyr::filter(P, nodal_type %in% par1) %>% apply(2, function(j) any(j == 1))
    par2_in_type <- dplyr::filter(P, nodal_type %in% par2) %>% apply(2, function(j) any(j == 1))
    sum(type_prob[par1_in_type & par2_in_type])/sum(type_prob[par2_in_type])
}


