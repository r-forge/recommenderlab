#NOTE: MUST PRESS SOURCE OR SOURCE ON SAVE AND THEN BUILD AND RELOAD FOR CHANGES TO TAKE EFFECT!


##TO DO
# check for memory
#do for just one user
#create TopN
#make parameters a string and a string[], find way to display them to users
#implement stuff for more advanced recommenders
#determine if you actually need the split stuff if you want to learn from the whole database
#maybe use a 0 or 1 as the value


#ok. this is exactly what i need to do for this project
#need to add a getTopN function for a specific user (this gets the top N recommendations for that user)
#need to be able to predict for a specified set of users 
#need to add a parameter to allow this to work for multiple prea functions


#commands i need
#a<-REAL_PREA(DATA)
#predict(a,...DATA[1,])

REAL_PREA <- function(data, parameter= NULL) {
  
  p <- recommenderlab:::.get_parameters(list(
    k = 30, 
    method="Cosine",
    normalize = "center", 
    normalize_sim_matrix = FALSE,
    alpha = 0.5,
    na_as_zero = FALSE,
    minRating = NA
  ), parameter)
  
  #trip is a triplet (i, j, x) representation of a sparse matrix
  trip <- (as(as(MovieLense,"dgCMatrix"), "dgTMatrix")) #replace movie lense with data
  
  #interface is a Java object of the type CFInterface
  interface <- .jnew("CFInterface", check=TRUE, silent=FALSE)
  
  
  rowlength <- trip@Dim[1]
  columnlength <- trip@Dim[2]
  column <- (1:columnlength)
  row <- (1:rowlength)
    
  #calls interface.createRatingMatrix(rowlength, columnlength, i[], j[], x[]);
  #basically transfers the matrix to a Java reporesentation of a sparse matrix
  r <-.jcall(interface, returnSig = "LRecContainer;",
             "createRatingMatrix", rowlength, columnlength,
             trip@i, trip@j, trip@x, silent=FALSE, check=TRUE)
  
  #These are the parameters for the the PREA functions in java
  #TODO: get these from command line in the future
  strings <- .jarray( c("ItemAvg", "simple", "0.2"))
  
  
  #this creates a recommender object using the above settings
  #stores it in 'r'
  r <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", r, strings)
  
  #This describes the model for R.
  model <- c(list(description = "PREA: ", preaObject = r), p)
  
  #This is the predict function that will be 
  predict <- function(model, newdata, n = 10, data=NULL, type=c("topNList", "ratings"), ...) {
    "predicting things"
    r <- model$preaObject
    p <- sapply(.jcall(interface, returnSig = "[[D", "runRecommender", r), .jevalArray, silent=FALSE)
    
    #return matrix 1 rating for each item and 1 row for each user
    
  }
  
  ## construct recommender object
  new("Recommender", method = "PREA", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}


## for testing 
#recommenderRegistry$delete_entry( method="IBCF2", dataType = "realRatingMatrix")
#recommenderRegistry$set_entry(
#	method="IBCF2", dataType = "realRatingMatrix", fun=REAL_IBCF,
#	description="Recommender based on item-based collaborative filtering (real data).")


## register recommender
# is now in onLoad.R



