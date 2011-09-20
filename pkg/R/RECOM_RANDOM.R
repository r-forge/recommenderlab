## recommend random unknown items
RANDOM <- function(data=NULL, parameter=NULL) {

    ## nothing to do!
	model <- list(range= range(as(data, "dgCMatrix")))

	predict <- function(model=NULL, newdata, n=10, 
		type=c("topNList", "ratings"), ...) {
	
	    type <- match.arg(type)
	    n <- as.integer(n)


	    if(type=="ratings") {
		ratings <- matrix(runif(nrow(newdata)*ncol(newdata), 
				model$range[1], model$range[2]),
			nrow=nrow(newdata), ncol=ncol(newdata))
		## remove known ratings
		ratings[as(as(newdata, "ngCMatrix"), "matrix")] <- NA
		return(as(ratings, "realRatingMatrix"))
	    }

	    itemlist <- 1:ncol(newdata)

	    ## remove known items and sample
	    reclist <- lapply(LIST(newdata, decode = FALSE, ratings = FALSE), 
		    FUN = function(x) sample(itemlist[-x], 
			    min(length(itemlist[-x]), n)))

	    new("topNList", items = reclist, 
		    itemLabels = colnames(newdata), n = n)
	}

	## this recommender has no model
	new("Recommender", method = "RANDOM", dataType = "ratingMatrix", 
		ntrain = nrow(data),
		model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(method="RANDOM", dataType="realRatingMatrix", 
	fun=RANDOM, 
	description="Produce random recommendations (real ratings).")

recommenderRegistry$set_entry(method="RANDOM", dataType="binaryRatingMatrix", 
	fun=RANDOM, 
	description="Produce random recommendations (binary ratings).")

