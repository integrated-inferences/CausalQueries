library(rstan)


rstan_options(auto_write = TRUE)


test <- stan(file = "other/test_multinomial.stan",data = list(
	K = 4, x = 2 *
		rmultinom(1, size = 20, prob = 1:4 / 10)[,1]))

test

rstan::stan_model(file =  "other/test_multinomial.Stan")
?stan
