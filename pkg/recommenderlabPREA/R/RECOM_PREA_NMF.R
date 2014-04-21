#Derek Phanekham
#11-14-2013

.REAL_PREA_NMF_PARAM <- list(
  k = 30, 
  method = "nmf",
  normalize = "center", 
  normalize_sim_matrix = FALSE,
  alpha = 0.5,
  na_as_zero = FALSE,
  minRating = NA,
  feature_count = 100,
  regularizer = 0.0001,
  max_iteration = 5
)


#accepts a matrix of type 'realRatingMatrix'
REAL_PREA_NMF<- function(data, parameter= NULL) {
  
  param <- .get_parameters(.REAL_PREA_NMF_PARAM, parameter)
  
  #trip is a triplet (i, j, x) representation of a sparse matrix
  RatingsAsTripletMatrix <- (as(as(data,"dgCMatrix"), "dgTMatrix")) 
  colAndRowNames <- data@data@Dimnames
  #interface is a Java object of the type CFInterface
  interface <- .jnew("CFInterface", check=TRUE, silent=FALSE)
  
  rowlength <- RatingsAsTripletMatrix@Dim[1]
  columnlength <- RatingsAsTripletMatrix@Dim[2]
  param$range[1] = rowlength
  param$range[2] = columnlength
  
  recommenderJObject <- NULL
  model <- c(list(description = "PREA: NMF", preaObject = NULL, data=data), param)
  
  if(param$lazy == FALSE) {
    #calls interface.createRatingMatrix(rowlength, columnlength, i[], j[], x[]);
    #basically transfers the matrix to a Java reporesentation of a sparse matrix
    ratingMat <-.jcall(interface, returnSig = "LRecContainer;",
                       "createRatingMatrix", rowlength, columnlength,
                       RatingsAsTripletMatrix@i, RatingsAsTripletMatrix@j, RatingsAsTripletMatrix@x, silent=FALSE, check=TRUE)
    
    strings <- .jarray( c(param$method, "simple", "0.2", param$feature_count, param$regularizer, param$max_iteration))
    recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
    model <- c(list(description = "PREA: NMF", preaObject = recommenderJObject, data=data), param)
    
  }
  
  
  #This describes the model for R.
  #This is the predict function that will be used to
  #produce a top N list
  #and produce a matrix of ratings
  predict <- function(model, newdata, n = 10, data=NULL, type=c("topNList", "ratings"), ...) {
    
    type <- match.arg(type)
    r <- model$preaObject
    OriginalData <- model$data
    
    if(class(newdata) == "integer") {
      print("integer")
      if(model$lazy == TRUE) {
        strings <- .jarray( c(param$method, "simple", "0.2", param$feature_count, param$regularizer, param$max_iteration))
        recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)        
        r <- recommenderJObject
        
      }
      
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
      
    }
    
    else if(class(newdata) == "realRatingMatrix") {
      if(model$lazy == FALSE) stop("To add new data when predicting, rebuild the recommender with param=c(lazy=TRUE)")
      
      CombinedMatrix <- as(rbind2(as(newdata,"dgCMatrix"), as(OriginalData, "dgCMatrix")), "dgTMatrix")
      CombinedColAndRowNames <- newdata@data@Dimnames
      
      CombinedRowlength <- CombinedMatrix@Dim[1]
      CombinedColumnlength <- CombinedMatrix@Dim[2] 
      
      #calls interface.createRatingMatrix(rowlength, columnlength, i[], j[], x[]);
      #basically transfers the matrix to a Java reporesentation of a sparse matrix
      JavaRatingMat <-.jcall(interface, returnSig = "LRecContainer;",
                             "createRatingMatrix", CombinedRowlength, CombinedColumnlength,
                             CombinedMatrix@i, CombinedMatrix@j, CombinedMatrix@x, silent=FALSE, check=TRUE)
      
      strings <- .jarray( c(param$method, "simple", "0.2", param$feature_count, param$regularizer, param$max_iteration))
      recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", JavaRatingMat, strings)
      #model <- c(list(description = "PREA: PMF", preaObject = recommenderJObject), param)
      r <- recommenderJObject
      
      #calls interface.createRatingMatrix(rowlength, columnlength, i[], j[], x[]);
      #basically transfers the matrix to a Java representation of a sparse matrix
      predictedValues <- sapply(.jcall(interface, returnSig = "[[D", "runRecommender", r), .jevalArray, silent=FALSE)
      predictedValues <- as(predictedValues, "realRatingMatrix")
      predictedValues@data@Dimnames <- CombinedColAndRowNames
      
      if (type=="topNList") {
        ratings <- predictedValues[1:newdata@data@Dim[1]+1,]
        return(getTopNLists(ratings, n))
        
      } else if (type == "ratings") {
        print("compiling ratings")
        return(predictedValues[1:newdata@data@Dim[1]+1,])
      }
      
      
    }
  }

 
  ## construct recommender object
  new("Recommender", method = "PREA_NMF", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}


## register recommender
# is now in onLoad.R


