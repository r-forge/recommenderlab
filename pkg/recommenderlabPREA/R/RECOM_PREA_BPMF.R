#Derek Phanekham
#11-14-2013

.REAL_PREA_BPMF_PARAM <- list(
  k = 30, 
  method = "bpmf",
  normalize = "center", 
  normalize_sim_matrix = FALSE,
  alpha = 0.5,
  na_as_zero = FALSE,
  minRating = NA,
  feature_count = 2,
  max_iteration = 20
)


#accepts a matrix of type 'realRatingMatrix'
REAL_PREA_BPMF<- function(data, parameter= NULL) {
  
  param <- .get_parameters(.REAL_PREA_BPMF_PARAM, parameter)
  
  #trip is a triplet (i, j, x) representation of a sparse matrix
  tripletMatrix <- (as(as(data,"dgCMatrix"), "dgTMatrix")) 
  
  #interface is a Java object of the type CFInterface
  interface <- .jnew("CFInterface", check=TRUE, silent=FALSE)
  
  
  rowlength <- tripletMatrix@Dim[1]
  columnlength <- tripletMatrix@Dim[2]
  column <- (1:columnlength)
  row <- (1:rowlength)
  param$range[1] = rowlength
  param$range[2] = columnlength
  
  #calls interface.createRatingMatrix(rowlength, columnlength, i[], j[], x[]);
  #basically transfers the matrix to a Java reporesentation of a sparse matrix
  ratingMat <-.jcall(interface, returnSig = "LRecContainer;",
                     "createRatingMatrix", rowlength, columnlength,
                     tripletMatrix@i, tripletMatrix@j, tripletMatrix@x, silent=FALSE, check=TRUE)
  
  strings <- .jarray( c(param$method, "simple", "0.2", param$feature_count, param$max_iteration))
  recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
  
  #This describes the model for R.
  model <- c(list(description = "PREA: REGSVD", preaObject = recommenderJObject), param)
  #This is the predict function that will be used to
  #produce a top N list
  #and produce a matrix of ratings
  predict <- function(model, newdata, n = 10, data=NULL, type=c("topNList", "ratings"), ...) {
    print("predicting things")
    type <- match.arg(type)
    r <- model$preaObject
    predictedValues <- sapply(.jcall(interface, returnSig = "[[D", "runRecommender", r), .jevalArray, silent=FALSE)
    predictedValues <- as(predictedValues, "realRatingMatrix")
    
    if (type=="topNList") {
      ratings <- matrix(runif(nrow(newdata)*ncol(newdata), model$range[1], model$range[2]),nrow=nrow(newdata), ncol=ncol(newdata), 
                        dimnames=dimnames(newdata))
      
      ratings <- as(ratings, "realRatingMatrix")
      ratings <- removeKnownRatings(ratings, newdata)
      return(getTopNLists(ratings, n))
      
    } else if (type == "ratings") {
      print("compiling ratings")
      return(predictedValues)
    }
    #return matrix 1 rating for each item and 1 row for each user
    
  }
  
  ## construct recommender object
  new("Recommender", method = "PREA_BPMF", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}


## register recommender
# is now in onLoad.R
