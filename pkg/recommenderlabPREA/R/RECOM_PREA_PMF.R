#Derek Phanekham
#11-14-2013

.REAL_PREA_PMF_PARAM <- list(
  k = 30, 
  method = "pmf",
  normalize = "center", 
  normalize_sim_matrix = FALSE,
  alpha = 0.5,
  na_as_zero = FALSE,
  minRating = NA,
  feature_count = 100,
  learning_rate = 50,
  regularizer = 0.4,
  momentum = 0.8,
  max_iteration = 20
)


#accepts a matrix of type 'realRatingMatrix'
REAL_PREA_PMF<- function(data, parameter= NULL) {
  
  param <- .get_parameters(.REAL_PREA_PMF_PARAM, parameter)
  
  #trip is a triplet (i, j, x) representation of a sparse matrix
  tripletMatrix <- (as(as(data,"dgCMatrix"), "dgTMatrix")) 
  colAndRowNames <- data@data@Dimnames
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
  
  strings <- .jarray( c(param$method, "simple", "0.2", param$feature_count,
                        param$learning_rate, param$regularizer, param$momentum, param$max_iteration))
  recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
  
  #This describes the model for R.
  model <- c(list(description = "PREA: PMF", preaObject = recommenderJObject), param)
  #This is the predict function that will be used to
  #produce a top N list
  #and produce a matrix of ratings
  predict <- function(model, newdata, n = 10, data=NULL, type=c("topNList", "ratings"), ...) {
    print("predicting things")
    type <- match.arg(type)
    r <- model$preaObject
    
    predictedValues <- sapply(.jcall(interface, returnSig = "[[D", "runRecommender", r), .jevalArray, silent=FALSE)
    predictedValues <- as(predictedValues, "realRatingMatrix")
    predictedValues@data@Dimnames <- colAndRowNames
    
    if (type=="topNList") {
      ratings <- predictedValues[newdata,]
      return(getTopNLists(ratings, n))
      
    } else if (type == "ratings") {
      print("compiling ratings")
      return(predictedValues)
    }
    #return matrix 1 rating for each item and 1 row for each user
    
  }
  
  ## construct recommender object
  new("Recommender", method = "PREA_PMF", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}


## register recommender
# is now in onLoad.R

