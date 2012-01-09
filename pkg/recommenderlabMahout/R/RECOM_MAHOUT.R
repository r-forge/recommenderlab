## always recommends the top-N popular items (without known items)
MAHOUT <- function(data, parameter = NULL) {

    model <- list( data???)

    predict <- function(model, newdata, n=10, ...) {
	
	### code goes here.
	write/read data
	createRecommender (Java)
	package item ids as a topNList	
	
	
	topN <- removeKnownItems(model$topN, newdata, replicate=TRUE)
	topN <- bestN(topN, n)
	return(topN)
    }

    ## construct recommender object
    new("Recommender", method = "POPULAR", dataType = class(data),
	    ntrain = nrow(data), model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(
	method="MAHOUT", dataType = "realRatingMatrix", fun=MAHOUT, 
	description="Recommender based on Mahout."
)




