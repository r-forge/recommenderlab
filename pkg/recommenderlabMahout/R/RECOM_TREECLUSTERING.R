 test_code <- function() {
  library(recommenderlabMahout)
  data("MovieLense")
     rec <- Recommender(MovieLense, method = "MAHOUT_TREECLUSTERING")
     pre <- predict(rec, 1, n = 3)
     pre
     as(pre, "list")
}

MAHOUT_TREECLUSTERING <- function(data, parameter = NULL) {

    model <- list(data= data)
    predict <- function(model, newdata, n=10, ...) {

file <- "/tmp/data.csv"
write.table(getData.frame(MovieLense[1:5], decode=FALSE), file=file, quote=FALSE, row.names=FALSE, col.names=FALSE, sep=",")
data <- .jnew("java/lang/String", file)
user <- newdata
howmany <- 10
obj <- .jnew("com/movieRecommender/Main")
result <- .jcall(obj,"[I","createRecommender","treeClusteringRecommender",data, as.integer(user), as.integer(howmany))
	topN <- new("topNList", items = list(user = result), itemLabels = colnames(model$data), n=length(result))
	topN <- bestN(topN, n) 
	return(topN)
    }

    new("Recommender", method = "MAHOUT_TREECLUSTERING", dataType = class(data),
	    ntrain = nrow(data), model = model, predict = predict)
}

#recommenderRegistry$modify_entry(
recommenderRegistry$set_entry(
	method="MAHOUT_TREECLUSTERING", dataType = "realRatingMatrix", fun= MAHOUT_TREECLUSTERING, 
	description="Recommender based on Mahout."
)
