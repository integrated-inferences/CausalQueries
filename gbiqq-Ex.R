pkgname <- "gbiqq"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('gbiqq')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_dots")
### * add_dots

flush(stderr()); flush(stdout())

### Name: add_dots
### Title: Helper to fill in missing do operators in causal expression
### Aliases: add_dots

### ** Examples

model <- make_model('X -> Y <- M')
gbiqq:::add_dots('Y[X=1]', model)
gbiqq:::add_dots('Y[]', model)




cleanEx()
nameEx("all_data_types")
### * all_data_types

flush(stderr()); flush(stdout())

### Name: all_data_types
### Title: All data types
### Aliases: all_data_types

### ** Examples

all_data_types(make_model('X -> Y'))
model <- make_model('X -> Y') %>% set_restrictions(labels = list(Y = '00'), keep = TRUE)
  all_data_types(model)
  all_data_types(model, complete_data = TRUE)
  all_data_types(model, possible_data = TRUE)
  all_data_types(model, given  = 'X==1')
  all_data_types(model, given  = 'X==1 & Y==1')




cleanEx()
nameEx("clean_params")
### * clean_params

flush(stderr()); flush(stdout())

### Name: clean_params
### Title: Check parameters sum to 1 in paramset; normalize if needed; add
###   names if needed
### Aliases: clean_params

### ** Examples

model <- make_model('X->Y')
model$parameters_df$param_value <- 1:6
clean_params(model$parameters_df, warning = TRUE)



cleanEx()
nameEx("collapse_data")
### * collapse_data

flush(stderr()); flush(stdout())

### Name: collapse_data
### Title: Make compact data with data strategies
### Aliases: collapse_data

### ** Examples


model <- make_model('X -> Y')
df <- simulate_data(model, n = 10)
df[1,1] <- ''
collapse_data(df, model)

collapse_data(df, model, drop_NA = FALSE)

collapse_data(df, model, drop_family = TRUE)

collapse_data(df, model, summary = TRUE)

data <- simulate_data(model, n = 0)
collapse_data(data, model)

model <- make_model('X -> Y') %>% set_restrictions('X==1')
df <- simulate_data(model, n = 10)
df[1,1] <- ''
collapse_data(df, model)
data <- data.frame(X= 0:1)
collapse_data(data, model)

model <- make_model('X->Y')
long_data <- simulate_data(model, n = 6)
collapse_data(long_data, model)



cleanEx()
nameEx("complements")
### * complements

flush(stderr()); flush(stdout())

### Name: complements
### Title: Make statement for complements
### Aliases: complements

### ** Examples

complements('A', 'B', 'W')




cleanEx()
nameEx("continue_names")
### * continue_names

flush(stderr()); flush(stdout())

### Name: continue_names
### Title: Continue names
### Aliases: continue_names

### ** Examples

x <- c('S_3', 'S_3', 'S_5')
gbiqq:::continue_names(x)



cleanEx()
nameEx("data_type_names")
### * data_type_names

flush(stderr()); flush(stdout())

### Name: data_type_names
### Title: Data type names
### Aliases: data_type_names

### ** Examples

model <- make_model('X -> Y')
data <- simulate_data(model, n = 2)
data_type_names(model, data)




cleanEx()
nameEx("decreasing")
### * decreasing

flush(stderr()); flush(stdout())

### Name: decreasing
### Title: Make monotonicity statement (negative)
### Aliases: decreasing

### ** Examples

decreasing('A', 'B')




cleanEx()
nameEx("drop_empty_families")
### * drop_empty_families

flush(stderr()); flush(stdout())

### Name: drop_empty_families
### Title: Drop empty families
### Aliases: drop_empty_families

### ** Examples


data_events <- data.frame(event = c('X0Y0', 'Y0'), strategy = c('XY', 'Y'), count = 1:0)
gbiqq:::drop_empty_families(data_events)




cleanEx()
nameEx("expand_data")
### * expand_data

flush(stderr()); flush(stdout())

### Name: expand_data
### Title: Expand compact data object to data frame
### Aliases: expand_data

### ** Examples

model <- make_model('X->M->Y')
simulate_events(model, n = 5) %>%
  expand_data(model)
simulate_events(model, n = 0) %>%
  expand_data(model)




cleanEx()
nameEx("expand_wildcard")
### * expand_wildcard

flush(stderr()); flush(stdout())

### Name: expand_wildcard
### Title: Expand wildcard
### Aliases: expand_wildcard

### ** Examples

expand_wildcard('(Y[X=1, M=.] > Y[X=1, M=.])')




cleanEx()
nameEx("get_ambiguities_matrix")
### * get_ambiguities_matrix

flush(stderr()); flush(stdout())

### Name: get_ambiguities_matrix
### Title: Get ambiguities matrix
### Aliases: get_ambiguities_matrix

### ** Examples

model <- make_model('X -> Y')
get_ambiguities_matrix(model = model)




cleanEx()
nameEx("get_data_families")
### * get_data_families

flush(stderr()); flush(stdout())

### Name: get_data_families
### Title: get_data_families
### Aliases: get_data_families

### ** Examples

get_data_families(model = make_model('X->Y'))
get_data_families(model = make_model('X->Y'), mapping_only = TRUE)
get_data_families(model = make_model('X-> M -> Y'))





cleanEx()
nameEx("get_event_prob")
### * get_event_prob

flush(stderr()); flush(stdout())

### Name: get_event_prob
### Title: Draw event probabilities
### Aliases: get_event_prob

### ** Examples

model <- make_model('X -> Y')
get_event_prob(model = model)
get_event_prob(model = model, parameters = rep(1, 6))
get_event_prob(model = model, parameters = 1:6)



cleanEx()
nameEx("get_nodal_types")
### * get_nodal_types

flush(stderr()); flush(stdout())

### Name: get_nodal_types
### Title: Get list of types for nodes in a DAG
### Aliases: get_nodal_types

### ** Examples

model <- make_model('X -> K -> Y')
get_nodal_types(model)

model <- make_model('X -> K -> Y') %>%
   set_restrictions(statement = 'K[X=1]>K[X=0]') %>%
   set_confound(list(K = 'Y[K=1]>Y[K=0]'))
unlist(get_nodal_types(model))




cleanEx()
nameEx("get_param_dist")
### * get_param_dist

flush(stderr()); flush(stdout())

### Name: get_param_dist
### Title: Get a distribution of model parameters
### Aliases: get_param_dist

### ** Examples

get_param_dist(model = make_model('X->Y'), using = 'priors', n_draws = 4)
get_param_dist(model = make_model('X->Y'), using = 'parameters')



cleanEx()
nameEx("get_parameter_names")
### * get_parameter_names

flush(stderr()); flush(stdout())

### Name: get_parameter_names
### Title: Get parameter names
### Aliases: get_parameter_names

### ** Examples


get_parameter_names(make_model('X->Y'))




cleanEx()
nameEx("get_parameters")
### * get_parameters

flush(stderr()); flush(stdout())

### Name: get_parameters
### Title: Get parameters
### Aliases: get_parameters

### ** Examples

get_parameters(make_model('X -> Y'))



cleanEx()
nameEx("get_parents")
### * get_parents

flush(stderr()); flush(stdout())

### Name: get_parents
### Title: Get list of parents in a dag
### Aliases: get_parents

### ** Examples

model <- make_model('X -> K -> Y')
get_parents(model)



cleanEx()
nameEx("get_prior_distribution")
### * get_prior_distribution

flush(stderr()); flush(stdout())

### Name: get_prior_distribution
### Title: Get a prior distribution from priors
### Aliases: get_prior_distribution

### ** Examples

make_model('X -> Y') %>% set_prior_distribution(n_draws = 5) %>% get_prior_distribution()
make_model('X -> Y') %>% get_prior_distribution(3)




cleanEx()
nameEx("get_priors")
### * get_priors

flush(stderr()); flush(stdout())

### Name: get_priors
### Title: Get priors
### Aliases: get_priors

### ** Examples

get_priors(make_model('X -> Y'))



cleanEx()
nameEx("get_query_types")
### * get_query_types

flush(stderr()); flush(stdout())

### Name: get_query_types
### Title: Get values of types according to a query
### Aliases: get_query_types

### ** Examples

model <- make_model('X -> M -> Y; X->Y')
query <- '(Y[X=1] > Y[X=0]) & (M[X=0]==1)'
x <- get_query_types(model, query)
summary(x)

query <- 'Y[M=M[X=0], X=1]==1'
x <- get_query_types(model, query)
get_query_types(model, query)

query <- '(Y[X=1, M = 1] >  Y[X=0, M = 1]) & (Y[X=1, M = 0] >  Y[X=0, M = 0])'
get_query_types(model, query)

query <- 'Y[X=1] == Y[X=0]'
get_query_types(model, query)

query <- '(X == 1) & (M==1) & (Y ==1) & (Y[X=0] ==1)'
x <- get_query_types(model, query)

query <- '(Y[X = .]==1)'
get_query_types(model, query)

model <- make_model('X -> Y')



cleanEx()
nameEx("get_type_prob")
### * get_type_prob

flush(stderr()); flush(stdout())

### Name: get_type_prob
### Title: Get type probabilities
### Aliases: get_type_prob

### ** Examples

get_type_prob(model = make_model('X->Y'))
get_type_prob(model = make_model('X->Y'), parameters = 1:6)




cleanEx()
nameEx("get_type_prob_multiple")
### * get_type_prob_multiple

flush(stderr()); flush(stdout())

### Name: get_type_prob_multiple
### Title: Draw matrix of type probabilities, before or after estimation
### Aliases: get_type_prob_multiple

### ** Examples

model <- make_model('X -> Y')
get_type_prob_multiple(model, using = 'priors', n_draws = 3)
get_type_prob_multiple(model, using = 'parameters', n_draws = 3)



cleanEx()
nameEx("get_types")
### * get_types

flush(stderr()); flush(stdout())

### Name: get_types
### Title: Get values of types according to a query
### Aliases: get_types

### ** Examples

model <- make_model('X -> M -> Y; X->Y')
query <- '(Y[X=1] > Y[X=0]) & (M[X=0]==1)'
x <- get_types(model, query)
summary(x)

query <- 'Y[M=M[X=0], X=1]==1'
x <- get_types(model, query)
get_types(model, query)

query <- '(Y[X=1, M = 1] >  Y[X=0, M = 1]) & (Y[X=1, M = 0] >  Y[X=0, M = 0])'
get_types(model, query)

query <- 'Y[X=1] == Y[X=0]'
get_types(model, query)

query <- '(X == 1) & (M==1) & (Y ==1) & (Y[X=0] ==1)'
x <- get_types(model, query)

query <- '(Y[X = .]==1)'
get_types(model, query)



cleanEx()
nameEx("increasing")
### * increasing

flush(stderr()); flush(stdout())

### Name: increasing
### Title: Make monotonicity statement (positive)
### Aliases: increasing

### ** Examples

increasing('A', 'B')




cleanEx()
nameEx("interacts")
### * interacts

flush(stderr()); flush(stdout())

### Name: interacts
### Title: Make statement for any interaction
### Aliases: interacts

### ** Examples

interacts('A', 'B', 'W')
get_query_types(model = make_model('X-> Y <- W'),
         query = interacts('X', 'W', 'Y'))




cleanEx()
nameEx("interpret_type")
### * interpret_type

flush(stderr()); flush(stdout())

### Name: interpret_type
### Title: Interpret or find position in nodal type
### Aliases: interpret_type

### ** Examples

model <- make_model('R -> X; Z -> X; X -> Y')
#Example using digit position
interpret_type(model, position = list(X = c(3,4), Y = 1))
#Example using condition
interpret_type(model, condition = c('X | Z=0 & R=1', 'X | Z=0 & R=0'))
#Return interpretation of all digit positions of all nodes
interpret_type(model)



cleanEx()
nameEx("lookup_nodal_type")
### * lookup_nodal_type

flush(stderr()); flush(stdout())

### Name: lookup_nodal_type
### Title: Lookup nodal types according to a query
### Aliases: lookup_nodal_type

### ** Examples

model <- make_model('X->Y')

lookup_nodal_type(model, '(Y[X=0] > Y[X=1])')
lookup_nodal_type(model, '(Y[X=0] >= Y[X=1])')

model <- make_model('X -> M -> Y; X->Y')
query <- '(Y[X=0] > Y[X=1])'
x <- lookup_nodal_type(model, query)

query <- '(Y[X=0, M = .] > Y[X=1, M = 0])'
x <- lookup_nodal_type(model, query)


query <- '(Y[] == 1)'
x <- lookup_nodal_type(model, query)
x <- lookup_nodal_type(model, query, join_by = '&')

query <- '(X == 1)'
x <- lookup_nodal_type(model, query)

query <- '(M[X=1] == M[X=0])'
x <- lookup_nodal_type(model, query)

# Complements
model <- make_model('M->Y; X->Y')
query <- complements('X', 'M', 'Y')
lookup_nodal_type(model, query)



cleanEx()
nameEx("make_ambiguities_matrix")
### * make_ambiguities_matrix

flush(stderr()); flush(stdout())

### Name: make_ambiguities_matrix
### Title: Make ambiguities matrix
### Aliases: make_ambiguities_matrix

### ** Examples

model <- make_model('X -> Y')
make_ambiguities_matrix(model = model)




cleanEx()
nameEx("make_confounds_df")
### * make_confounds_df

flush(stderr()); flush(stdout())

### Name: make_confounds_df
### Title: Make a confounds dataframe
### Aliases: make_confounds_df

### ** Examples

model <- make_model('X -> Y') %>%
set_confound('X <-> Y', add_confounds_df  = FALSE)
make_confounds_df(model)

model <- make_model('X -> M -> Y; X <-> Y') %>%
set_restrictions(c('M[X=1] == M[X=0]', 'Y[M=1]==Y[M=0]'))
make_confounds_df(model)

model <- make_model('X -> M -> Y; X <-> M; M <-> Y') %>%
set_restrictions(c('M[X=1] == M[X=0]', 'Y[M=1]==Y[M=0]'))
make_confounds_df(model)

# The implied confounding is between X and M and also between X and Y
model <- make_model('X -> M -> Y') %>%
  set_confound(list(X = 'Y[X=1] > Y[X=0]'), add_confounds_df = FALSE)
make_confounds_df(model)

model <- make_model('X -> M -> Y')
make_confounds_df(model)

# Bad case
## Not run: 
##D model <- make_model('X -> Y') %>%
##D   set_confound(list(X = 'X==1'))
## End(Not run)

# Complex confounding 1
model <- make_model('A -> X <- B ; A <-> X; B <-> X')
model$confounds_df

# Complex confounding 2
model <- make_model('A <- X -> B; A <-> X; B <-> X') %>%
set_restrictions(c('A[X=0] == A[X=1]', 'B[X=0] == B[X=1]'))
table(model$parameters_df$param_set)
model$confounds_df

# Full confounding: X, A|X, B|A,X with 7 degrees of freedom
model <- make_model('A <- X -> B; A <-> X; B <-> X; A<->B') %>%
set_restrictions(c('A[X=0] == A[X=1]', 'B[X=0] == B[X=1]'))
table(model$parameters_df$param_set)
model$confounds_df



cleanEx()
nameEx("make_data")
### * make_data

flush(stderr()); flush(stdout())

### Name: make_data
### Title: Make data
### Aliases: make_data

### ** Examples



# Simple draws
model <- make_model("X -> M -> Y")
make_data(model)
make_data(model, n = 3, nodes = c("X","Y"))
make_data(model, n = 3, param_type = "prior_draw")
make_data(model, n = 10, param_type = "define", alpha =  0:9)

# Data Strategies
# A strategy in which X, Y are observed for sure and M is observed
# with 50% probability for X=1, Y=0 cases

model <- make_model("X -> M -> Y")
make_data(
  model,
  n = 8,
  nodes = list(c("X", "Y"), "M"),
  probs = list(1, .5),
  subsets = list(NULL, "X==1 & Y==0"))

# Simulate multiple datasets is fastest if w is provided
model <- make_model("X -> Y")
w <- get_event_prob(model)
replicate(5, make_data_single(model, n = 5, w = w))




cleanEx()
nameEx("make_data_single")
### * make_data_single

flush(stderr()); flush(stdout())

### Name: make_data_single
### Title: Generate full dataset
### Aliases: make_data_single

### ** Examples


model <- make_model("X -> Y")

# Simplest behavior uses by default  the parameter vector contained in model,
# which is flat by default:
make_data_single(model, n = 5)

make_data_single(model, n = 5, param_type = "prior_draw")

# Simulate multiple datasets is fastest if w is provided
w <- get_event_prob(model)
replicate(5, make_data_single(model, n = 5, w = w))




cleanEx()
nameEx("make_model")
### * make_model

flush(stderr()); flush(stdout())

### Name: make_model
### Title: Make a model
### Aliases: make_model

### ** Examples

make_model(statement = "X -> Y")
modelXKY <- make_model("X -> K -> Y; X -> Y")

# Example where cyclicaly dag attempted
## Not run: 
##D  modelXKX <- make_model("X -> K -> X")
## End(Not run)

# Examples with confounding
model <- make_model("X->Y; X <-> Y")
model$P
model <- make_model("Y2 <- X -> Y1; X <-> Y1; X <-> Y2")
model$P
model$confounds_df
dim(model$P)
model$P
model <- make_model("X1 -> Y <- X2; X1 <-> Y; X2 <-> Y")
dim(model$P)
model$parameters_df

# A single node graph is also possible
model <- make_model("X")
plot(model)

# Unconnected nodes cannot
## Not run: 
##D  model <- make_model("X <-> Y")
##D  plot(model)
## End(Not run)



cleanEx()
nameEx("make_parameter_matrix")
### * make_parameter_matrix

flush(stderr()); flush(stdout())

### Name: make_parameter_matrix
### Title: Make parameter matrix
### Aliases: make_parameter_matrix

### ** Examples

model <- make_model('X -> Y')
make_parameter_matrix(model)



cleanEx()
nameEx("make_parameters")
### * make_parameters

flush(stderr()); flush(stdout())

### Name: make_parameters
### Title: Make a 'true' parameter vector
### Aliases: make_parameters

### ** Examples


# Simple examples
model <- make_model('X -> Y')
data  <- simulate_data(model, n = 2)
model <- update_model(model, data)
make_parameters(model, parameters = c(.25, .75, 1.25,.25, .25, .25))
make_parameters(model, param_type = 'flat')
make_parameters(model, param_type = 'prior_draw')
make_parameters(model, param_type = 'prior_mean')
make_parameters(model, param_type = 'posterior_draw')
make_parameters(model, param_type = 'posterior_mean')

# Harder examples, using \code{define} and priors arguments to define
# specific parameters using causal syntax

make_parameters(make_model('X -> Y'),
               statement = 'Y[X=1]>Y[X=0]', alphas = 2)
make_model('X -> Y') %>%
   make_parameters(statement = c('Y[X=1]>Y[X=0]', 'Y[X=1]<Y[X=0]'), alphas = c(2,0))

# May be built up
make_model('X -> Y') %>%
  set_confound(list(X = 'Y[X=1]>Y[X=0]'))  %>%
  set_parameters(confound = list(X='Y[X=1]>Y[X=0]', X='Y[X=1]<=Y[X=0]'),
                 alphas = list(c(.2, .8), c(.8, .2))) %>%
  set_parameters(statement = 'Y[X=1]>Y[X=0]', alphas = .5) %>%
  get_parameters



cleanEx()
nameEx("make_prior_distribution")
### * make_prior_distribution

flush(stderr()); flush(stdout())

### Name: make_prior_distribution
### Title: Make a prior distribution from priors
### Aliases: make_prior_distribution

### ** Examples

make_model('X -> Y') %>% make_prior_distribution(n_draws = 5)




cleanEx()
nameEx("make_priors")
### * make_priors

flush(stderr()); flush(stdout())

### Name: make_priors
### Title: Make Priors
### Aliases: make_priors

### ** Examples

model <- make_model('X -> M -> Y')
make_priors(model, node = 'X', alphas = 3)
make_priors(model, node = c('X', 'Y'), alphas = 3)
make_priors(model, node = list('X', 'Y'), alphas = list(3, 6))
make_priors(model, node = c('X', 'Y'), distribution = c('certainty', 'jeffreys'))
make_priors(model, statement = 'Y[M=1] > Y[M=0]', alphas = 3)
make_priors(model, statement = c('Y[M=1] > Y[M=0]', 'M[X=1]== M[X=0]'), alphas = c(3, 2))
## Not run: 
##D # Error if statement seeks to
##D make_priors(model, statement = 'Y[X=1] > Y[X=0]', alphas = 3)
## End(Not run)
model <- make_model('X->Y') %>%
 set_confound(list(X = 'Y[X=1] > Y[X=0]', X = 'Y[X=1] < Y[X=0]'))
make_priors(model,
            confound = list(X='Y[X=1] > Y[X=0]',
                            X='Y[X=1] < Y[X=0]'),
            alphas = c(3, 6))
make_model('X -> Y') %>%
  set_confound(list(X = 'Y[X=1]>Y[X=0]'))%>%
  make_priors(statement = 'X==1',
              confound = list(X = 'Y[X=1]>Y[X=0]', X = 'Y[X=1]<Y[X=0]'),
              alphas = c(2, .5))



cleanEx()
nameEx("make_priors_single")
### * make_priors_single

flush(stderr()); flush(stdout())

### Name: make_priors_single
### Title: make_priors_single
### Aliases: make_priors_single

### ** Examples

model <- make_model('X -> M -> Y; X->Y')

gbiqq:::make_priors_single(model, distribution = 'jeffreys')

gbiqq:::make_priors_single(model, alphas = 3)

# Examples of selecting subsets
# By node
gbiqq:::make_priors_single(model, node = 'M', alphas = 8)

# By nodal type statement
gbiqq:::make_priors_single(model,
        statement = '(Y[X=1, M = .] > Y[X=0, M = .])', alphas = 2)

# By nodal type label (safest to provide node also)
gbiqq:::make_priors_single(model, node = 'X', label = '0', alphas = 9)

# By confound query: Applies only to types that are involved in confounding
# Only alters named node in confound, even if other nodes are listed in 'nodes'
confounds <- list(X = 'Y[X=1] > Y[X=0]', X = 'Y[X=1] < Y[X=0]')
model     <- make_model('X->Y') %>% set_confound(confounds)
gbiqq:::make_priors_single(model, confound = confounds[1], alphas = 3)
gbiqq:::make_priors_single(model, node = 'Y', confound = confounds[1], alphas = 3)

# A residual  confound condition can also be defined
gbiqq:::make_priors_single(model, confound = list(X = '!(Y[X=1] > Y[X=0])'), alphas = 3)
gbiqq:::make_priors_single(model, confound = list(X = '(Y[X=1] == Y[X=0])'), alphas = 3)

# make_priors_single can also be used for some vector valued statements
model <- make_model('X -> M -> Y')
gbiqq:::make_priors_single(model, node = c('X', 'Y'), alphas = 2)
gbiqq:::make_priors_single(model, label = c('1', '01'), alphas = 2)

# Incompatible conditions produce no change
# Such cases best handled by  make_priors
gbiqq:::make_priors_single(model, node = 'X', label = '01', alphas = 2)

# Problematic example
## Not run: 
##D gbiqq:::make_priors_single(model, alphas = 1:2)
## End(Not run)




cleanEx()
nameEx("non_decreasing")
### * non_decreasing

flush(stderr()); flush(stdout())

### Name: non_decreasing
### Title: Make monotonicity statement (non negative)
### Aliases: non_decreasing

### ** Examples

non_decreasing('A', 'B')




cleanEx()
nameEx("non_increasing")
### * non_increasing

flush(stderr()); flush(stdout())

### Name: non_increasing
### Title: Make monotonicity statement (non positive)
### Aliases: non_increasing

### ** Examples

non_increasing('A', 'B')




cleanEx()
nameEx("observe_data")
### * observe_data

flush(stderr()); flush(stdout())

### Name: observe_data
### Title: Observe data, given a strategy
### Aliases: observe_data

### ** Examples

model <- make_model("X -> Y")
df <- simulate_data(model, n = 8)
# Observe X values only
observe_data(complete_data = df, nodes_to_observe = "X")
# Observe half the Y values for cases with observed X = 1
observe_data(complete_data = df,
     observed = observe_data(complete_data = df, nodes_to_observe = "X"),
     nodes_to_observe = "Y", prob = .5,
     subset = "X==1")



cleanEx()
nameEx("perm")
### * perm

flush(stderr()); flush(stdout())

### Name: perm
### Title: Produces the possible permutations of a set of nodes
### Aliases: perm

### ** Examples


## Not run: 
##D perm(3)
## End(Not run)



cleanEx()
nameEx("plot_dag")
### * plot_dag

flush(stderr()); flush(stdout())

### Name: plot_dag
### Title: Plot your dag using dagitty
### Aliases: plot_dag

### ** Examples

## Not run: 
##D model <- make_model('X -> K -> Y; X -> Y')
##D plot_dag(model)
##D model <- make_model('X -> K -> Y; X <-> Y')
##D plot_dag(model)
## End(Not run)




cleanEx()
nameEx("prep_gbiqq_data")
### * prep_gbiqq_data

flush(stderr()); flush(stdout())

### Name: prep_gbiqq_data
### Title: Prepare data for stan
### Aliases: prep_gbiqq_data

### ** Examples

model <- make_model('X->Y')
data  <-  collapse_data(simulate_data(model, n = 6), model)
prep_gbiqq_data(model, data)

model <- make_model('X->Y') %>% set_confound(list(X = 'Y[X=1]>Y[X=0]'))
data  <-  collapse_data(simulate_data(model, n = 6), model)
prep_gbiqq_data(model, data)




cleanEx()
nameEx("query_distribution")
### * query_distribution

flush(stderr()); flush(stdout())

### Name: query_distribution
### Title: Calculate estimand distribution
### Aliases: query_distribution

### ** Examples

model <- make_model("X -> Y") %>%
         set_prior_distribution()

 distribution <- query_distribution(model, query = "(Y[X=1] - Y[X=0])")
 distribution <- query_distribution(model, query = "(Y[X=1] - Y[X=0])", given = "X==1")
 distribution <- query_distribution(model, query = "(Y[X=1] - Y[X=0])", given = "Y[X=1]==1")
 distribution <- query_distribution(model, query = "(Y[X=1] > Y[X=0])")
 distribution <- query_distribution(model, query = "(Y[X=.] == 1)", join_by = "&")
 distribution <- query_distribution(model, query = "(Y[X=1] - Y[X=0])", using = "parameters")
## Not run: 
##D  df    <- simulate_data(model, n = 3)
##D  updated_model <- gbiqq(model, df)
##D  query_distribution( updated_model , query = "(Y[X=1] - Y[X=0])", using = "posteriors")
## End(Not run)



cleanEx()
nameEx("query_model")
### * query_model

flush(stderr()); flush(stdout())

### Name: query_model
### Title: Generate estimands dataframe
### Aliases: query_model

### ** Examples

model <- make_model("X -> Y") %>% set_prior_distribution(n_draws = 10000)

estimands_df <-query_model(
               model,
               query = list(ATE = "Y[X=1] - Y[X=0]", Share_positive = "Y[X=1] > Y[X=0]"),
               using = c("parameters", "priors"),
               expand_grid = TRUE)

estimands_df <-query_model(
               model,
               query = list(ATE = "Y[X=1] - Y[X=0]", Share_positive = "Y[X=1] > Y[X=0]"),
               using = c("parameters", "priors"),
               expand_grid = FALSE)

estimands_df <- query_model(
                model,
                using = list( "parameters", "priors"),
                query = list(ATE = "Y[X=1] - Y[X=0]", Is_B = "Y[X=1] > Y[X=0]"),
                given = list(TRUE,  "Y==0 & X==1"),
                expand_grid = TRUE,
                digits = 3)

# An example: a stat representing uncertainty of token causation
token_var <- function(x) mean(x)*(1-mean(x))
estimands_df <- query_model(
                model,
                using = list( "parameters", "priors"),
                query = "Y[X=1] > Y[X=0]",
                stats = c(mean = mean, sd = sd, token_var = token_var))




cleanEx()
nameEx("reveal_outcomes")
### * reveal_outcomes

flush(stderr()); flush(stdout())

### Name: reveal_outcomes
### Title: Reveal outcomes
### Aliases: reveal_outcomes

### ** Examples

model <- make_model("X -> Y")
reveal_outcomes(model)

model <- make_model("X1->Y;X2->M;M->Y")
reveal_outcomes(model, dos = list(X1 = 1, M = 0))

model <- make_model("X->M->Y")
reveal_outcomes(model, dos = list(M = 1), node = "Y")




cleanEx()
nameEx("set_confound")
### * set_confound

flush(stderr()); flush(stdout())

### Name: set_confound
### Title: Set confound
### Aliases: set_confound

### ** Examples


model <- make_model('X -> Y') %>%
  set_confound(list('X <-> Y'))
get_parameters(model)

# In this case we notionally place a distribution but in fact Y has degenerate support
make_model('X -> Y -> Z') %>%
  set_restrictions(c(increasing('X', 'Y')), keep = TRUE) %>%
  set_confound(list('X <-> Y')) %>%
  get_parameter_matrix()

# X nodes assigned conditional on Y
make_model('X -> Y') %>%
  set_confound(list(X = 'Y')) %>%
  get_parameter_matrix()

# Y nodes assigned conditional on X
make_model('X -> Y') %>%
  set_confound(list(Y = 'X')) %>%
  get_parameter_matrix()

model <- make_model('X -> Y') %>%
  set_confound(list(X = '(Y[X=1]>Y[X=0])', X = '(Y[X=1]<Y[X=0])', X = '(Y[X=1]==Y[X=0])'))

model <- make_model('X -> M -> Y') %>%
set_confound (list(X = '(Y[X=1]>Y[X=0])',
                 M = 'Y',
                 X = '(Y[X=1]<Y[X=0])'))

confound = list(A = '(D[A=., B=1, C=1]>D[A=., B=0, C=0])')
model <- make_model('A -> B -> C -> D; B -> D') %>%
 set_confound(confound = confound)

# Example where two parents are confounded
model <- make_model('A -> B <- C') %>%
  set_confound(list(A = 'C==1')) %>%
  set_parameters(c(0,1,1,0, .5, .5, rep(.0625, 16)))
cor(simulate_data(model, n = 20))

model <- make_model('X -> Y')
confound <- list(X = '(Y[X=1] > Y[X=0])', X = '(Y[X=1] == 1)')
model <- set_confound(model = model, confound = confound)

model <- make_model('X -> Y <- S; S -> W') %>%
  set_restrictions(c(
  increasing('X', 'Y'), increasing('S', 'W'),
  increasing('S', 'Y'), decreasing('S', 'Y')))
model1 <-  set_confound(model, list(X = 'S==1', S = 'W[S=1]==1'), add_confounds_df = TRUE)
model1$confounds_df
model2 <-  set_confound(model, list(S = 'X==1', S = 'W[S=1]==1'), add_confounds_df = TRUE)
model2$confounds_df



cleanEx()
nameEx("set_confounds_df")
### * set_confounds_df

flush(stderr()); flush(stdout())

### Name: set_confounds_df
### Title: Set a confounds_df
### Aliases: set_confounds_df

### ** Examples

model <- make_model('X -> Y') %>%
  set_confound(list('X <-> Y'), add_confounds_df = FALSE)
model$confounds_df
set_confounds_df(model)$confounds_df

# An example where a confounds dataframe needs to be updated manually
# Here a restriction applied after a confounding relation is set removes the confounding
model <- make_model('X -> Y') %>%
  set_confound(list(X = '(Y[X=1] > Y[X=0])')) %>%
  set_restrictions('(Y[X=1] > Y[X=0])')
model$confounds_df  # Incorrect
model <- set_confounds_df(model)
model$confounds_df  # Correct
# plot(model)



cleanEx()
nameEx("set_parameters")
### * set_parameters

flush(stderr()); flush(stdout())

### Name: set_parameters
### Title: Set parameters
### Aliases: set_parameters

### ** Examples

make_model('X->Y') %>% set_parameters(1:6) %>% get_parameters()

make_model('X -> Y') %>%
  set_confound(list(X = 'Y[X=1]>Y[X=0]'))  %>%
  set_parameters(confound = list(X='Y[X=1]>Y[X=0]', X='Y[X=1]<=Y[X=0]'),
                 alphas = list(c(.2, .8), c(.8, .2))) %>%
  set_parameters(statement = 'Y[X=1]>Y[X=0]', alphas = .5) %>%
  get_parameters



cleanEx()
nameEx("set_prior_distribution")
### * set_prior_distribution

flush(stderr()); flush(stdout())

### Name: set_prior_distribution
### Title: Add prior distribution draws
### Aliases: set_prior_distribution

### ** Examples

make_model('X -> Y') %>% set_prior_distribution(n_draws = 5) %>% get_prior_distribution()




cleanEx()
nameEx("set_priors")
### * set_priors

flush(stderr()); flush(stdout())

### Name: set_priors
### Title: Set prior distribution
### Aliases: set_priors

### ** Examples


library(dplyr)
model <- make_model('X -> Y') %>%
  set_priors(alphas = 3)
get_priors(model)

model <- make_model('X -> Y') %>%
set_priors(distribution = 'jeffreys')
get_priors(model)

model <- make_model('X -> Y') %>%
set_priors(1:6)
get_priors(model)
model <- make_model('X -> Y') %>%
set_priors(node = 'Y', alphas = 2)
get_priors(model)



cleanEx()
nameEx("set_restrictions")
### * set_restrictions

flush(stderr()); flush(stdout())

### Name: set_restrictions
### Title: Restrict a model
### Aliases: set_restrictions

### ** Examples


# 1. Restrict parameter space using statements
model <- make_model('X->Y') %>%
  set_restrictions(statement = c('X == 0'))

model <- make_model('X->Y') %>%
  set_restrictions(non_increasing('X', 'Y'))

model <- make_model('X -> Y <- W') %>%
  set_restrictions(c(decreasing('X', 'Y'), substitutes('X', 'W', 'Y')))

model$parameters_df

model <- make_model('X-> Y <- W') %>%
  set_restrictions(statement = decreasing('X', 'Y'))
model$parameters_df

model <- make_model('X->Y') %>%
  set_restrictions(decreasing('X', 'Y'))
model$parameters_df

model <- make_model('X->Y') %>%
  set_restrictions(c(increasing('X', 'Y'), decreasing('X', 'Y')))
model$parameters_df

# Restrict to define a model with monotonicity
model <- make_model('X->Y') %>%
set_restrictions(statement = c('Y[X=1] < Y[X=0]'))
get_parameter_matrix(model)

# Restrict to a single type in endogenous node
model <- make_model('X->Y') %>%
set_restrictions(statement =  '(Y[X = 1] == 1)', join_by = '&', keep = TRUE)
get_parameter_matrix(model)

#  Use of | and &
# Keep node if *for some value of B* Y[A = 1] == 1
model <- make_model('A->Y<-B') %>%
set_restrictions(statement =  '(Y[A = 1] == 1)', join_by = '|', keep = TRUE)
dim(get_parameter_matrix(model))


# Keep node if *for all values of B* Y[A = 1] == 1
model <- make_model('A->Y<-B') %>%
set_restrictions(statement =  '(Y[A = 1] == 1)', join_by = '&', keep = TRUE)
dim(get_parameter_matrix(model))

# Restrict multiple nodes
model <- make_model('X->Y<-M; X -> M' ) %>%
set_restrictions(statement =  c('(Y[X = 1] == 1)', '(M[X = 1] == 1)'), join_by = '&', keep = TRUE)
get_parameter_matrix(model)

# Restrictions on levels for endogenous nodes aren't allowed
## Not run: 
##D model <- make_model('X->Y') %>%
##D set_restrictions(statement =  '(Y == 1)')
## End(Not run)

# 2. Restrict parameter space Using labels:
model <- make_model('X->Y') %>%
set_restrictions(labels = list(X = '0', Y = '00'))

# Restrictions can be  with wildcards
model <- make_model('X->Y') %>%
set_restrictions(labels = list(Y = '?0'))
get_parameter_matrix(model)

# Running example: there are only four causal types
model <- make_model('S -> C -> Y <- R <- X; X -> C -> R') %>%
set_restrictions(labels = list(C = '1000', R = '0001', Y = '0001'), keep = TRUE)
get_parameter_matrix(model)




cleanEx()
nameEx("simulate_data")
### * simulate_data

flush(stderr()); flush(stdout())

### Name: simulate_data
### Title: simulate_data is an alias for make_data
### Aliases: simulate_data

### ** Examples

simulate_data(make_model("X->Y"))



cleanEx()
nameEx("simulate_events")
### * simulate_events

flush(stderr()); flush(stdout())

### Name: simulate_events
### Title: Draw compact data
### Aliases: simulate_events

### ** Examples

model <- make_model('X -> Y')
simulate_events(model = model)
simulate_events(model = model, param_type = 'prior_draw')
simulate_events(model = model, include_strategy = TRUE)




cleanEx()
nameEx("st_within")
### * st_within

flush(stderr()); flush(stdout())

### Name: st_within
### Title: Get string between two regular expression patterns
### Aliases: st_within

### ** Examples

a <- '(XX[Y=0] == 1) > (XX[Y=1] == 0)'
st_within(a)
b <- '(XXX[[Y=0]] == 1 + XXX[[Y=1]] == 0)'
st_within(b)



cleanEx()
nameEx("substitutes")
### * substitutes

flush(stderr()); flush(stdout())

### Name: substitutes
### Title: Make statement for substitutes
### Aliases: substitutes

### ** Examples


get_query_types(model = make_model('A -> B <- C'),
         query = substitutes('A', 'C', 'B'))

query_model(model = make_model('A -> B <- C'),
         queries = substitutes('A', 'C', 'B'),
         using = 'parameters')



cleanEx()
nameEx("te")
### * te

flush(stderr()); flush(stdout())

### Name: te
### Title: Make treatment effect statement (positive)
### Aliases: te

### ** Examples

te('A', 'B')

model <- make_model('X->Y') %>% set_restrictions(increasing('X', 'Y'))
query_model(model, list(ate = te('X', 'Y')),  using = 'parameters')

# set_restrictions  breaks with te because it requires a listing
# of causal types, not numeric output.
## Not run: 
##D model <- make_model('X->Y') %>% set_restrictions(te('X', 'Y'))
## End(Not run)




cleanEx()
nameEx("translate_dagitty")
### * translate_dagitty

flush(stderr()); flush(stdout())

### Name: translate_dagitty
### Title: Puts your DAG into daggity syntax (useful for using their
###   plotting functions)
### Aliases: translate_dagitty

### ** Examples

## Not run: 
##D model <- make_model('X -> Y')
##D translate_dagitty(model)
## End(Not run)




cleanEx()
nameEx("update_causal_types")
### * update_causal_types

flush(stderr()); flush(stdout())

### Name: update_causal_types
### Title: Update causal types based on nodal types
### Aliases: update_causal_types

### ** Examples

update_causal_types(make_model('X->Y'))



cleanEx()
nameEx("update_model")
### * update_model

flush(stderr()); flush(stdout())

### Name: update_model
### Title: Fit causal model using stan
### Aliases: update_model

### ** Examples

model <- make_model('X->Y')
data_long   <- simulate_data(model, n = 4)
data_short  <- collapse_data(data_long, model)
model_1 <- update_model(model, data_long)

# Throws error unless compact data indicated:
## Not run: 
##D model_3 <- update_model(model, data_short)
## End(Not run)
model_4 <- update_model(model, data_short, data_type = 'compact')

# It is possible to implement updating without data, in which case the posterior
# is a stan object that reflects the prior
model5 <- update_model(model)

# Advanced: Example of a model with tailored parameters.
# We take a model and add a tailored P matrix (which maps from parameters
# to causal types) and a tailored parameters_df which reports that
# all parameters are in one family.
# Parameters in this example are not connected with nodal types in any way.

## Not run: 
##D model <- make_model('X->Y')
##D model$P <- diag(8)
##D colnames(model$P) <- rownames(model$causal_types)
##D model$parameters_df <- data.frame(
##D   param_names = paste0('x',1:8),
##D   param_set = 1, priors = 1, parameters = 1/8)
##D 
##D # Update fully confounded model on strongly correlated data
##D 
##D data <- make_data(make_model('X->Y'), n = 100,
##D   parameters = c(.5, .5, .1,.1,.7,.1))
##D fully_confounded <- update_model(model, data, keep_fit = TRUE)
##D fully_confounded$stan_fit
##D query_model(fully_confounded, 'Y[X = 1] > Y[X=0]', using = 'posteriors')
##D # To see the confounding:
##D with(fully_confounded$posterior_distribution %>% data.frame(),
##D {par(mfrow = c(1,2))
##D  plot(x1, x5, main = 'joint distribution of X0.Y00, X0.Y01')
##D  plot(x1, x6, main = 'joint distribution of X0.Y00, X1.Y01')})
## End(Not run)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
