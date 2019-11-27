

model <- make_model("X -> M -> Y; X <-> M; M <-> Y")



nodal_types_entangled <- function(model){

	nodal_type <- model$parameters_df$nodal_type
	nts <- unique(nodal_type)

	1-sapply(nts, function(nt_i) {
		sapply(nts, function(nt_j) nonind(nt_i,nt_j, model$P, nodal_type))})
	}

nonind <- function(nt_i,nt_j, P, nodal_type){
	Pi <- dplyr::filter(P, nodal_type == nt_i)
	Pj <- dplyr::filter(P, nodal_type == nt_j)
	distinct_combinations_ij(Pi, Pj)}

distinct_combinations_ij <- function(Pi, Pj){
	ni <- nrow(Pi)
	nj <- nrow(Pj)
	addresses <- as.matrix(perm(c(ni,nj)-1) , ncol = 2) +1
	comb_list <- apply(addresses, 1, function(k) (Pi[k[1], ]) * (Pj[k[2], ]) )
	comb_mat  <- do.call(rbind, 	comb_list <- apply(addresses, 1, function(k) (Pi[k[1], ]) * (Pj[k[2], ]) ))
	apply(comb_mat, 1, sum) %>% gbiqq:::zero_range()
}



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
