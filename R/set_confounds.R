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
#' A no confounding assumption means that Pr(t^X | t^Y) = Pr(t^X), or  Pr(t^X, t^Y) = Pr(t^X)Pr(t^Y).
#' In this case there would be 3 degrees of freedom for Y and 1 for X, totaling 4 rather than 7.
#'
#' \code{set_confounds} lets you relax this assumption by increasing the number of parameters characterizing the joint distribution. Using the fact that P(A,B) = P(A)P(B|A) new parameters are introduced to capture P(B|A=a) rather than simply P(B).
#' For instance here two parameters (and one degree of freedom) govern the distribution of types X  and four parameters (with 3 degrees of freedom) govern  the types for Y given the type of X for a total of 1+3+3 = 7 degress of freedom.
#'
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param confound A \code{list} of statements indicating pairs of nodes whose types are jointly distributed (e.g. list("A <-> B", "C <-> D")).
#' @return An object of class \code{causal_model} with updated parameters_df and parameter matrix.
#' @export
#' @examples
#'
#' make_model('X -> Y; X <-> Y') %>%
#' get_parameters()
#'
#' make_model('X -> M -> Y; X <-> Y') %>%
#' get_parameters()
#'
#' model <- make_model('X -> M -> Y; X <-> Y; M <-> Y')
#' model$parameters_df
#'
#' # Example where set_confound is implemented after restrictions
#'make_model("A -> B -> C") %>%
#' set_restrictions(increasing("A", "B")) %>%
#' set_confound("B <-> C") %>%
#' get_parameters()
#'
#' # Example where two parents are confounded
#' make_model('A -> B <- C; A <-> C') %>%
#'   set_parameters(node = "C", c(0.05, .95, .95, 0.05)) %>%
#'   make_data(n = 50) %>%
#'   cor()
#'
#'  # Example with two confounds, added sequentially
#' model <- make_model('A -> B -> C') %>%
#'   set_confound(list("A <-> B", "B <-> C"))
#' model$statement
#' # plot(model)


set_confound <- function(model, confound = NULL) {

  if(any(lapply(confound, function(k) grepl(";", k)) %>% unlist))
    stop("Please provide multipe confounds as a list")

    given <- gen <- NULL

    if(!(all(model$parameters_df$given == "")))
        stop("Confounds have already been declared. Please declare confounds only once.")


    # extract the node from the nodal type name
    node_from_type <- function(type)
        sapply(strsplit(type, "\\."), function(x) x[[1]])

    # extract the conditions from the nodal type name
    conditions_from_type <- function(type)
        sapply(strsplit(type, "_"), function(x) gsub('\\.', '', unique(x)), simplify = FALSE)


    is_a_model(model)

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


    # Add confounds to model statement if not present already
    # (Present if provided in first instance; not if provided later)
    # Note need to check writing on both directions and deal with loose spacing
    statement <- gsub(" ", "", model$statement)
    order_1 <- paste0(names(confound), "<->", confound)
    order_2 <- paste0(confound, "<->", names(confound))
    for (j in 1:length(confound)) {
      if(!grepl(order_1[j], statement) & !grepl(order_2[j], statement))
       model$statement <-
          paste(model$statement, ";",  paste(names(confound)[j], "<->", confound[j]))
    }


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

    newP}) %>%
      bind_rows

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
    model$parameters_df <-
      model$parameters_df %>%
      filter(param_names %in% row.names(model$P))

    model$P <-
        model$P[match(model$parameters_df$param_names, rownames(model$P)),]

    class(model$P) <- c("parameter_matrix", "data.frame")
    rownames(model$parameters_df) <- NULL

    # Export
    attr(model$P, "param_set") <- unique(model$parameters_df$param_set)
    model

}

set_confounds <- set_confound


