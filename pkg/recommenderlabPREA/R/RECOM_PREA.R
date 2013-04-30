
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
  
  
  trip <- (as(as(data,"dgCMatrix"), "dgTMatrix"))
  
  #.jinit()
  #options(java.parameters = "-Xmx2048m")
  #.jaddClassPath("/home/derek/Work/source/prea.jar")
  #.jaddClassPath("/inst/java/prea.jar")
  
  interface <- .jnew("CFInterface", check=TRUE, silent=FALSE)
  
  rowlength <- trip@Dim[1]
  columnlength <- trip@Dim[2]
  column <- (1:columnlength)
  row <- (1:rowlength)
    
  r <-.jcall(interface, returnSig = "LRecContainer;",
             "createRatingMatrix", rowlength, columnlength,
             trip@i, trip@j, trip@x, silent=FALSE, check=TRUE)
  
  strings <- .jarray( c("ItemAvg", "simple", "0.2"))
  
  
  #this creates a recommender object in java with above parameters
  r <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", r, strings)
  

  model <- c(list(description = "IBCF: Reduced similarity matrix", preaObject = r), p)
  
  
  #make already known values NA
  #create TopN
  #do for just one user
  
  predict <- function(model, newdata, n = 10, data=NULL, type=c("topNList", "ratings"), ...) {
    
    #this should go in predict
    r <- model$preaObject
    p <- sapply(.jcall(interface, returnSig = "[[D", "runRecommender", r), .jevalArray, silent=FALSE)
    
    p
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



