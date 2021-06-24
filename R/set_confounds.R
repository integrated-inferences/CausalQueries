#' Set confound
#'
#' Adjust parameter matrix to allow confounding. To be called only by make_model.
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
#' Allow  confounding by adding a bidirected edge in the model statement
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param confound A named \code{list}. It relates nodes to statements that identify causal types with which they are confounded
#' @return An object of class \code{causal_model}. It essentially returns a list containing the elements comprising
#' a model (e.g. 'statement', 'nodal_types' and 'DAG') with the parameter matrix updated according to `confound`.
#' @export
#' @examples
#'
#' make_model('X -> Y; X <-> Y') %>%
#' get_parameters()
#'
#'make_model("X -> Y") %>%
#' set_confound("X <-> Y") %>%
#' get_parameters()
#'
#' make_model('X -> M -> Y; X <->Y') %>%
#' get_parameters()
#'
#' # Example where two parents are confounded
#' model <- make_model('A -> B <- C; A <->C') %>%
#'   set_parameters(c(.5, .5, 0.05, .95, .95, 0.05, rep(.0625, 16)))
#' cor(simulate_data(model, n = 20))
#'

set_confound <- function(model, confound = NULL) {

    if(!(all(model$parameters_df$given == "")))
        stop("Confounds have already been declared. Please declare confounds only once.")

    # extract the node from the nodal type name
    node_from_type <- function(type)
        sapply(strsplit(type, "\\."), function(x) x[[1]])

    # extract the conditions from the nodal type name
    conditions_from_type <- function(type)
        sapply(strsplit(type, "_"), function(x) gsub('\\.', '', unique(x)), simplify = FALSE)


    CausalQueries:::is_a_model(model)

    # Housekeeping
    if (is.null(confound)) {
        message("No confound provided")
        return(model)
    }
    if (is.null(model$P))
        model <- set_parameter_matrix(model)

    # Turn A <-> B format to lists
    for (j in 1:length(confound)) {
        if (grepl("<->", confound[[j]])) {
            z <- sapply(strsplit(confound[[j]], "<->"), trimws)
            z <- rev(model$nodes[model$nodes %in% sapply(z, as.character)])
            confound[j] <- as.character(z[2])
            names(confound)[j] <- z[1]
        }}


    names_P <- names(model$P)
    model$parameters_df$given <- ""

    # Expand parameters_df
    ##################################################################################

    for(i in 1:length(confound)){

    from_nodal_types <-
            model$parameters_df %>% filter(node == confound[i]) %>% pull(param_names)

    to_add <-
        lapply(from_nodal_types, function(j)
            model$parameters_df %>%
                filter(node == names(confound)[i]) %>%
                mutate(given = ifelse(given == "", j, paste0(given, ", ", j)),
                       param_names = paste0(param_names, "_", j),
                       param_set = paste0(param_set, ".", j))) %>% bind_rows

    model$parameters_df <-
        rbind(
            filter(model$parameters_df, node != names(confound)[i]),
            to_add) %>%
        arrange(gen, param_set)
    }


    # P matrix expand
    ##################################################################################

    for(i in 1:length(confound)){
    # for(i in 1:2){

    from_nodal_types <-
            model$parameters_df %>% filter(node == confound[i]) %>% pull(param_names)

    to_add <-
        lapply(from_nodal_types, function(j){
            newP <- model$P %>%
                filter(node_from_type(rownames(model$P)) == names(confound)[i])
            rownames(newP) <- paste0(rownames(newP), "_", j)

    # delete relevant entries: need to figure if *all* conditioning
    # nodes are in observed data. Sometimes never: eg:"X.1_Y.00_X.0"
    # ie. (Z| (X1), (Y00|X0)
    row_elements <- conditions_from_type(rownames(newP)) # nodal types in parnames (list)
    for(k in 1:length(row_elements)){
         to_zero <- sapply(row_elements[k][[1]],
                              function(nd) sapply(nd, function(ndd) grepl(ndd, names_P))) %>% apply(1, prod)
          newP[k, to_zero==0] <- 0}

    newP}) %>% bind_rows
    to_add <- filter(to_add, apply(to_add, 1, sum) != 0)  # Remove impossible rows with all zeros

    # Add in
        model$P <-
            model$P %>%
            filter(node_from_type(rownames(model$P)) != names(confound)[i]) %>%
            rbind(to_add)
        }

    # Clean up
    ##################################################################################
    # P reorder
    model$parameters_df <- model$parameters_df %>% filter(param_names %in% row.names(model$P))
    model$P <-
        model$P[match(model$parameters_df$param_names, rownames(model$P)),]

    class(model$P) <- c("parameter_matrix", "data.frame")
    rownames(model$parameters_df) <- NULL

    # # Drop family if an entire set is empty
    # sets <- unique(model$parameters_df$param_set)
    # to_keep <-
    #     sapply(sets, function(j) sum(model$P[model$parameters_df$param_set == j, ]) > 0)
    #
    # if (!all(to_keep)) {
    #     keep <- model$parameters_df$param_set %in% sets[to_keep]
    #     model$parameters_df <- dplyr::filter(model$parameters_df, keep)
    #     model$P <- model$P[keep, ]
    # }
    #

    # Make a dataset of conditioned_node and conditioned_on nodes for graphing confound relations
    confounds_df <- data.frame(names(confound), unlist(confound))
    colnames(confounds_df) <- c("node 1", "node 2")

    # Export
    model$confounds_df <- confounds_df
    attr(model$P, "confounds_df") <- model$confounds_df
    attr(model$P, "param_set") <- unique(model$parameters_df$param_set)
    model

}

set_confounds <- set_confound


