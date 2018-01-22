w <- A_w %*% w_fundamental


for(i in 1:n_strategies){
	print(sum(w[starts[i]:ends[i]]))
}
