#NOTE: MUST PRESS SOURCE OR SOURCE ON SAVE AND THEN BUILD AND RELOAD FOR CHANGES TO TAKE EFFECT!


##TO DO
# check for memory
#do for just one user
#create TopN
#make parameters a string and a string[], find way to display them to users
#implement stuff for more advanced recommenders
#determine if you actually need the split stuff if you want to learn from the whole database
#maybe use a 0 or 1 as the value 

#testThat

#ok. this is exactly what i need to do for this project
#need to add a getTopN function for a specific user (this gets the top N recommendations for that user)
#need to be able to predict for a specified set of users 
#need to add a parameter to allow this to work for multiple prea functions


#commands i need
#a<-REAL_PREA(DATA)
#predict(a,...DATA[1,])

#accepts a matrix of type 'realRatingMatrix'
REAL_PREA <- function(data, parameter= NULL) {
  
  param <- recommenderlab:::.get_parameters(list(
    k = 30, 
    method = "userbased",
    normalize = "center", 
    normalize_sim_matrix = FALSE,
    alpha = 0.5,
    na_as_zero = FALSE,
    minRating = NA
  ), parameter)
  
  method <- param$method

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
  
  #These are the parameters for the the PREA functions in java
  #creates a recommender object using the above settings stores it in 'r'
  #ACCEPTABLE alg arguments = "ItemAvg", "UserAvg"
  if(method == "userbased"){
    strings <- .jarray( c(method, "simple", "0.2"))
    recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
  }
  else if (method == "avg"){
    strings <- .jarray( c(method, "simple", "0.2"))
    recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
  }
  else if (method == "useravg"){
    strings <- .jarray( c(method, "simple", "0.2"))
    recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
  }
  else if (method == "itemavg"){
    strings <- .jarray( c(method, "simple", "0.2"))
    recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
  }
  else if (method == "random"){
    strings <- .jarray( c(method, "simple", "0.2"))
    recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
  }
  else if (method == "slopeone"){
    strings <- .jarray( c(method, "simple", "0.2"))
    recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
  }
  else if (method == "regsvd"){
    strings <- .jarray( c(method, "simple", "0.2", param$arg1, param$arg2, param$arg3, param$arg4))
    recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
  }
  else if (method == "nmf"){
    strings <- .jarray( c(method, "simple", "0.2", param$arg1, param$arg2, param$arg3))
    recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
  }
  else if (method == "pmf"){
    strings <- .jarray( c(method, "simple", "0.2", param$arg1, param$arg2, param$arg3, param$arg4, param$arg5))
    recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
  }
  else if (method == "bpmf"){
    strings <- .jarray( c(method, "simple", "0.2", param$arg1, param$arg2))
    recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
  }
  else if (method == "nlpmf"){
    strings <- .jarray( c(method, "simple", "0.2", param$arg1, param$arg2, param$arg3, param$arg4, param$arg5, param$arg6, param$arg7, param$arg8))
    recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
  }
  else if (method == "npca"){
    strings <- .jarray( c(method, "simple", "0.2", param$arg1, param$arg2))
    recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
  }

  
  #This describes the model for R.
  model <- c(list(description = "PREA: ", preaObject = recommenderJObject), param)
  
  #This is the predict function that will be used to
  #produce a top N list
  #and produce a matrix of ratings
  #TODO return triplets instead of dense matrix
  #convert to type ratingMatrix
  #get topnlist using that
  #ALSO implement doing predict for just a set range of users
  predict <- function(model, newdata, n = 10, data=NULL, type=c("topNList", "ratings"), ...) {
    print("predicting things")
    type <- match.arg(type)
    r <- model$preaObject
    predictedValues <- sapply(.jcall(interface, returnSig = "[[D", "runRecommender", r), .jevalArray, silent=FALSE)
    predictedValues <- as(predictedValues, "realRatingMatrix")
    
    if(type=="topNList"){
      ratings <- matrix(runif(nrow(newdata)*ncol(newdata), model$range[1], model$range[2]),nrow=nrow(newdata), ncol=ncol(newdata), 
                        dimnames=dimnames(newdata))
      
      ratings <- as(ratings, "realRatingMatrix")
      ratings <- removeKnownRatings(ratings, newdata)
      return(getTopNLists(ratings, n))

    }
    else if(type == "ratings"){
      print("compiling ratings")
      return(predictedValues)
    }
    
    
    
    #return matrix 1 rating for each item and 1 row for each user
    
  }
  
  ## construct recommender object
  new("Recommender", method = "PREA", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}


## register recommender
# is now in onLoad.R



