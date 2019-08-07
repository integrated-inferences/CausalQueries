lookup_type(model, query){

	b <- 1:bracket_starts[i]
	var <- paste0(w_query[b], collapse = "")
	var <- gbiqq::st_within(var)
	var <- var[length(var)]
	w_query         <- gsub(" ", "", query)
	w_query         <- unlist(strsplit(query, ""))
	bracket_starts  <- grep( "\\[", w_query)
	bracket_ends    <- grep( "\\]", w_query)
	.query        	<- w_query[(bracket_starts):bracket_ends]
	brackets <- grepl("\\[|\\]",  .query)
	.query   <- .query[!brackets]
	.query   <-  paste0(.query, collapse = "")
	.query   <- unlist(strsplit(.query, ","))

	for (j in 1:length(.query)) {
		do <- unlist(strsplit( .query[j], ""))
		stop <- gregexpr("=", .query[j], perl = TRUE)[[1]][1]  - 1
		var_name <-  paste0(do[1:stop], collapse = "")
		var_name <- gsub(" ", "", var_name)
		value <- c(eval(parse(text = paste0(do, collapse = "") ), envir =  eval_var))
		vars  <-  model$variables
		if(!var_name %in% vars) 	stop(paste("Variable", var_name ,"is not part of the model."))
		dos[[j]] <- value
		names(dos)[[j]] <- var_name

	}

	b <- 1:bracket_starts
	var <- paste0(w_query[b], collapse = "")
	var <- st_within(var)
	var <- var[length(var)]
	w_query <- paste0(node, paste0 (  w_query[(bracket_ends+1):nchar(query)], collapse = ""))


	eval_var <- reveal_outcomes(model, dos, node = var)
	selected_types    <- c(eval(parse(text = w_query),  eval_var))
  names(selected_types) <- rownames(eval_var)


}
