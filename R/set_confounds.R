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
#' The simplest way to allow for confounding is by adding a bidirected edge, such as via: \code{set_confound(model, list('X <-> Y'))}.
#' In this case the descendent node has a distribution conditional on the value of the ancestor node.
#'
#' Ordering of conditioning can also be controlled however via  \code{set_confound(model, list(X = 'Y'))}
#' in which case \code{X} is given a distribution conditional on nodal types of \code{Y}.
#'
#' More specific confounding statements are also possble using causal syntax.
#' A statement of the form \code{list(X = 'Y[X=1]==1')} can be interpreted as:
#' 'Allow \code{X} to have a distinct conditional distribution when \code{Y} has types that involve Y[X=1]==1.'
#' In this case nodal types for \code{Y} would continue to have 3 degrees of freedom.
#' But there would be parameters assigning the probability of X when t^Y = 01 or t^Y=11 and
#' other parameters for residual cases. Thus 6 degrees of freedom in all. This is still short of
#' an unconstrained distribution, though an  unconstrained distribution can be achieved with
#' repeated application of statements of this form, for instance via
#' \code{list(X = 'Y[X=1]>Y[X=0]'), X = 'Y[X=1]==Y[X=0]')}.
#'
#' Similarly a statement of the form \code{list(Y = 'X==1')} can be interpreted as:
#' 'Allow Y to have a distinct conditional distribution when X=1.' In this case there would be
#' two distributions over nodal types for Y, producing 2*3 = 6 degrees of freedom.
#' Nodal types for X would continue to have 1 degree of freedom.
#' Thus 7 degrees of freedom in all, corresponding to a fully unconstrained joint distribution.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param confound A named \code{list}. It relates nodes to statements that identify causal types with which they are confounded
#' @param add_confounds_df Logical. Attach a dataframe with confound links. Defaults to TRUE.
#' @export
#' @examples
#'
#' model <- make_model('X -> Y') %>%
#'   set_confound(list('X <-> Y'))
#' get_parameters(model)
#'
#'# In this case we notionally place a distribution but in fact Y has degenerate support
#' make_model('X -> Y -> Z') %>%
#'   set_restrictions(c(increasing('X', 'Y')), keep = TRUE) %>%
#'   set_confound(list('X <-> Y')) %>%
#'   get_parameter_matrix()
#'
#' # X nodes assigned conditional on Y
#' make_model('X -> Y') %>%
#'   set_confound(list(X = 'Y')) %>%
#'   get_parameter_matrix()
#'
#' # Y nodes assigned conditional on X
#' make_model('X -> Y') %>%
#'   set_confound(list(Y = 'X')) %>%
#'   get_parameter_matrix()
#'
#' model <- make_model('X -> Y') %>%
#'   set_confound(list(X = '(Y[X=1]>Y[X=0])', X = '(Y[X=1]<Y[X=0])', X = '(Y[X=1]==Y[X=0])'))
#'
#' model <- make_model('X -> M -> Y') %>%
#' set_confound (list(X = '(Y[X=1]>Y[X=0])',
#'                  M = 'Y',
#'                  X = '(Y[X=1]<Y[X=0])'))
#'
#' confound = list(A = '(D[A=., B=1, C=1]>D[A=., B=0, C=0])')
#' model <- make_model('A -> B -> C -> D; B -> D') %>%
#'  set_confound(confound = confound)
#'
#' # Example where two parents are confounded
#' model <- make_model('A -> B <- C') %>%
#'   set_confound(list(A = 'C==1')) %>%
#'   set_parameters(c(0,1,1,0, .5, .5, rep(.0625, 16)))
#' cor(simulate_data(model, n = 20))
#'
#' model <- make_model('X -> Y')
#' confound <- list(X = '(Y[X=1] > Y[X=0])', X = '(Y[X=1] == 1)')
#' model <- set_confound(model = model, confound = confound)
#'
#' model <- make_model('X -> Y <- S; S -> W') %>%
#'   set_restrictions(c(
#'   increasing('X', 'Y'), increasing('S', 'W'),
#'   increasing('S', 'Y'), decreasing('S', 'Y')))
#' model1 <-  set_confound(model, list(X = 'S==1', S = 'W[S=1]==1'), add_confounds_df = TRUE)
#' model1$confounds_df
#' model2 <-  set_confound(model, list(S = 'X==1', S = 'W[S=1]==1'), add_confounds_df = TRUE)
#' model2$confounds_df


set_confound <- function(model, confound = NULL, add_confounds_df = TRUE) {

    # Housekeeping
    if (is.null(confound)) {
        message("No confound provided")
        return(model)
    }
    if (is.null(model$P))
        model <- set_parameter_matrix(model)

    nodes <- model$nodes
    P <- model$P
    pars <- rownames(P)
    types_matrix <- model$causal_types
    types_names <- rownames(types_matrix)

    # Descendant types

    # Check to see if any statements involve full confounding and redefine lists

    for (j in 1:length(confound)) {
        if (grepl("<->", confound[[j]])) {
            z <- sapply(strsplit(confound[[j]], "<->"), trimws)
            z <- rev(nodes[nodes %in% sapply(z, as.character)])
            confound[j] <- as.character(z[2])
            names(confound)[j] <- z[1]
        }
    }

    # Figure our when confound expressed as model node
    checks <- unlist(lapply(confound, function(x) x %in% model$nodes))

    # Function to either get types for a simple confound, or else expand to list for confound expressed
    # as node
    f <- function(i) {
        x <- confound[i]
        if (checks[i]) {
            # Confounded nodes
            nod_typs <- types_matrix[x[[1]]][[1]]
            exploded_list <- lapply(unique(nod_typs)[-1], function(n) types_names[n == nod_typs])
            names(exploded_list) <- rep(names(x), length(exploded_list))
            return(exploded_list)
        }
        if (!checks[i]) {
            simple_list <- list((map_query_to_causal_type(model, x[[1]])$type_list))
            names(simple_list) <- names(x)
            simple_list
        }
    }

    D <- f(1)
    if (length(confound) > 1)
        for (j in 2:length(confound)) D <- c(D, f(j))

    # Origin (Node with conditional distribution)
    A <- names(D)

    # Magic

    for (j in 1:length(A)) {

        # Housekeeping for renaming confound vars
        a <- A[j]  # param_name

        # Get a name for the new parameter: using hyphen separator to recognize previous confounding
        if (!any(startsWith(model$parameters_df$param_set, paste0(a, "_"))))
            model$parameters_df <- dplyr::mutate(model$parameters_df, param_set = ifelse(param_set ==
                a, paste0(a, "_", 0), param_set))

        # Now extend priors and parameters
        to_add <- model$parameters_df %>% dplyr::filter(node == a) %>% dplyr::mutate(param_set =  continue_names(param_set),
            param_names = paste(param_set, nodal_type, sep = "."))

        # Extend P: Make duplicate block of rows for each ancestor Should contain all values from parameter
        # family (gathered here by recombining)
        P_new <- data.frame(P) %>% dplyr::filter(model$parameters_df$node == a)  #%>%

        # Zero out duplicated entries: 1 (New P elements have 0 for non specified types)
        P_new[, !(types_names %in% D[[j]])] <- 0
        P <- rbind(P_new, data.frame(as.matrix(P[, ])))

        # Extend parameter_df
        model$parameters_df <- rbind(to_add, dplyr::mutate(model$parameters_df, param_names = paste(param_set,
            nodal_type, sep = ".")))

        # Zero out duplicated entries: 2
        older <- 1:nrow(P) > nrow(P_new)
        P[(model$parameters_df$node == a) & older, types_names %in% D[[j]]] <- 0
    }

    # Clean up for export
    rownames(P) <- model$parameters_df$param_names

    # Drop family if an entire set is empty
    sets <- unique(model$parameters_df$param_set)
    to_keep <- sapply(sets, function(j) sum(P[model$parameters_df$param_set == j, ]) > 0)

    if (!all(to_keep)) {
        keep <- model$parameters_df$param_set %in% sets[to_keep]
        model$parameters_df <- dplyr::filter(model$parameters_df, keep)
        P <- P[keep, ]
    }

    # Reorder
    new_order <- with(model$parameters_df, order(gen, param_set))  #, nodal_type))
    model$parameters_df <- model$parameters_df[new_order, ]
    model$P <- data.frame(P[new_order, ])
    class(model$P) <- c("parameter_matrix", "data.frame")
    rownames(model$parameters_df) <- NULL

    # Make a dataset of conditioned_node and conditioned_on nodes for graphing confound relations
    if (add_confounds_df)
        model <- set_confounds_df(model)

    attr(model$P, "confounds_df") <- model$confounds_df
    attr(model$P, "param_set") <- unique(model$parameters_df$param_set)
    # Export
    model

}



#' Continue names
#'
#' Slightly hacky function to continue param_set names in a sequence
#' @param x A vector with strings of the form c('x_1', 'x_2')
#' @param split String on which to split. String should appear only once in each vector entry.
#' @examples
#' x <- c('S_3', 'S_3', 'S_5')
#' CausalQueries:::continue_names(x)

continue_names <- function(x, split = "_") {
    z <- strsplit(x, split = split)
    r <- lapply(z, function(x) x[[2]])
    m <- max(as.numeric(r)) + 1
    lapply(z, function(w) paste0(w[[1]], split, as.numeric(w[[2]]) + m)) %>% unlist()
}


#' Set confounds
#'
#' alias for set_confound. See set_confound.
#' @param ... arguments passed to set_confound
#' @export
set_confounds <- function(...) set_confound(...)
