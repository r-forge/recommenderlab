
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
  
  
  if(!is.null(p$normalize))
    data <- normalize(data, method=p$normalize)
  
  ## this might not fit into memory! Maybe use a sample?
  sim <- as.matrix(similarity(data, method=p$method, which="items", 
                              args=list(alpha=p$alpha, na_as_zero=p$na_as_zero)))
  
  ## normalize rows to 1
  if(p$normalize_sim_matrix) sim <- sim/rowSums(sim, na.rm=TRUE)
  
  ## reduce similarity matrix to keep only the k highest similarities
  diag(sim) <- NA
  ##sim[!is.finite(sim)] <- NA
  
  for(i in 1:nrow(sim)) 
    sim[i,head(order(sim[i,], decreasing=FALSE, na.last=FALSE), 
               ncol(sim) - p$k)] <- NA
  
  ## make sparse
  sim <- dropNA(sim)
  
  
  model <- c(list(
    description = "IBCF: Reduced similarity matrix",
    sim = sim
  ), p
  )
  
  predict <- function(model, newdata, n = 10, 
                      data=NULL, type=c("topNList", "ratings"), ...) {
    
    type <- match.arg(type)
    
    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata,]
    }
    
    n <- as.integer(n)
    
    if(!is.null(model$normalize)) 
      newdata <- normalize(newdata, method=model$normalize)
    
    ## predict all ratings
    sim <- model$sim 
    u <- as(newdata, "dgCMatrix")
    
    ratings <- t(as(tcrossprod(sim,u) / tcrossprod(sim, u!=0), "matrix"))
    
    ratings <- new("realRatingMatrix", data=dropNA(ratings), 
                   normalize = getNormalize(newdata))
    ## prediction done
    
    ratings <- removeKnownRatings(ratings, newdata)
    
    if(!is.null(model$normalize)) 
      ratings <- denormalize(ratings)
    
    if(type=="ratings") return(ratings)
    
    getTopNLists(ratings, n=n, minRating=model$minRating)
    
  }
  
  ## construct recommender object
  new("Recommender", method = "IBCF", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}


## for testing 
#recommenderRegistry$delete_entry( method="IBCF2", dataType = "realRatingMatrix")
#recommenderRegistry$set_entry(
#	method="IBCF2", dataType = "realRatingMatrix", fun=REAL_IBCF,
#	description="Recommender based on item-based collaborative filtering (real data).")


## register recommender
# is now in onLoad.R



