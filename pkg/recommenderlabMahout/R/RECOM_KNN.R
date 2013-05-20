 test_code <- function() {
  library(recommenderlabMahout)
  data("MovieLense")
     rec <- Recommender(MovieLense, method = "MAHOUT_KNN")
     pre <- predict(rec, 3, n = 10)
     pre
     as(pre, "list")
}

MAHOUT_KNN <- function(data, parameter = NULL) {

    model <- list(data=data)
    
    predict <- function(model, newdata, n=10, ...) {

	### write data
	file <- tempfile(fileext=".csv")
	write.table(get.data.frame(model$data, decode=FALSE), 
		file=file, quote=FALSE, row.names=FALSE, col.names=FALSE, 
		sep=",")
	
	data <- .jnew("java/lang/String", file)
	user <- newdata
	howmany <- 10
	obj <- .jnew("com/movieRecommender/Main")
	result <- .jcall(obj,"[I","createRecommender","knnRecommender",data, as.integer(user), as.integer(howmany))
	topN <- new("topNList", items = list(user = result), itemLabels = colnames(model$data), n=length(result))
	topN <- bestN(topN, n) 
	return(topN)
    }

    new("Recommender", method = "MAHOUT_KNN", dataType = class(data),
	    ntrain = nrow(data), model = model, predict = predict)
}

#recommenderRegistry$modify_entry(
recommenderRegistry$set_entry(
	method="MAHOUT_KNN", dataType = "realRatingMatrix", fun= MAHOUT_KNN, 
	description="Recommender based on Mahout."
	)
