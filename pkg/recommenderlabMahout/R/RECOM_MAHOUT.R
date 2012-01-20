library(recommenderlabMahout)
data(MovieLense)

### use write.table
write.csv(file = "data.csv",  as(MovieLense[1:20,], "data.frame"))


data <- .jnew("java/lang/String", file)

## always recommends the top-N popular items (without known items)
MAHOUT_SLOPEONE <- function(data, parameter = NULL) {

    model <- list( data )

    predict <- function(model, newdata, n=10, ...) {
	
	##bind the newdata with data in model


### use write.table
### write.csv(file = "data.csv",  as(MovieLense[1:20,], "data.frame"))
write.csv(file = "data.csv",  as(complete data with newdata), "data.frame"))

data <- .jnew("java/lang/String", file)

#recsystreeclustering <- .jnew("com/movieRecommender/test")
#result =.jcall(recsystreeclustering,"[I","createRecommender","treeClusteringRecommender",file, as.integer(6012))
#create a for loop with ids of those users. Return a prediction object. 
user <- 6012

obj <- .jnew("com/movieRecommender/test")
result = .jcall(obj,"[I","createRecommender","slopeOneRecommender",data, as.integer(user))
result
	
	#convert result (int array) to an object in topN 
	#item labels are obtained from data
	topN <- new("topNList", list(user = result), colnames(model$data),     		10L)
	topN <- removeKnownItems(model$topN, newdata, replicate=TRUE)
	topN <- bestN(topN, n) //n is taken from predict
	return(topN)
    }

    ## construct recommender object
    new("Recommender", method = " MAHOUT_SLOPEONE", dataType = class(data),
	    ntrain = nrow(data), model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(
	method="MAHOUT_SLOPEONE", dataType = "realRatingMatrix", fun= MAHOUT_SLOPEONE, 
	description="Recommender based on Mahout."
)





