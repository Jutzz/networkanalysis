# Improved version of the script originally published in https://www.spatialanalytics.co.nz/post/2018/04/01/fixing-st-par/
# Ákos Bede-Fazekas (bfakos [at] ecolres.hu); 17. July 2022

st_par <- function(X, FUN, n_cores, ...){
	fold_length <- floor(nrow(X) / n_cores)
	split_vector <- rep(x = 1:n_cores, times = c(rep(x = fold_length, times = n_cores - 1), nrow(X) - fold_length * (n_cores - 1)))
	split_results <- split(X, split_vector) %>% parallel::mclapply(function(x) FUN(x, ...), mc.cores = n_cores)
	output_class <- class(split_results[[1]])
	if (length(output_class) == 2) output_class <- output_class[2]
	if (output_class %in% c("matrix", "array", "units")){
		result <- do.call("rbind", split_results)
		names(result) <- NULL
	} else if (output_class == "sfc") {
		result <- do.call("c", split_results)
		result <- FUN(result)
	} else if (output_class %in% c('list', 'sgbp') ){
		result <- do.call("c", split_results)
		names(result) <- NULL
	} else if (output_class == "data.frame" ){
		result <- do.call("rbind", split_results)
	} else stop("Unknown class. st_parallel only accepts the following outputs at present: sfc, list, sf, matrix, array, units, sgbp.")
	return(result)
}
