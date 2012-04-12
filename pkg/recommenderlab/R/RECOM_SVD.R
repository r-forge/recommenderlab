### Contributed by Saurabh Bathnagar (sbhatnagar@book.com)

REAL_SVD <- function(data, parameter= NULL) {
  
  p <- .get_parameters(list(
    categories = 50,
    method="Cosine",
    normalize = "center",
    normalize_sim_matrix = FALSE,
    alpha = 0.5,
    na_as_zero = FALSE,
    minRating = NA
    ), parameter)
  
  
  if(!is.null(p$normalize))
    data <- normalize(data, method=p$normalize)
  
  model <- c(list(
    description = "full matrix",
    data = data
    ), p)
  
  predict <- function(model, newdata, n = 10,
                      type=c("topNList", "ratings"), ...) {
    
    type <- match.arg(type)
    n <- as.integer(n)
    
    if(!is.null(model$normalize))
      newdata <- normalize(newdata, method=model$normalize)

    # Perform SVD
    data <- model$data@data
    data <- rBind(data, newdata@data)
    
    ### FIXME: svd does as.matrix which sets all missing values to 0!

    s<-svd(data)
    # Get Diag
    D <- diag(s$d[1:p$categories])
    
    ratings <- s$u[,1:p$categories] %*% D %*% t(s$v[,1:p$categories])
    # Put back correct names
    rownames(ratings) <- rownames(data)
    colnames(ratings) <- colnames(data)
    # Only need to give back new users
    ratings <- ratings[(dim(model$data@data)[1]+1):dim(ratings)[1],]
    
    ratings <- new("realRatingMatrix", data=dropNA(ratings))
    ## prediction done
    
    ratings <- removeKnownRatings(ratings, newdata)
    
    if(!is.null(model$normalize))
      ratings <- denormalize(ratings)
    
    if(type=="ratings") return(ratings)
    
    getTopNLists(ratings, n=n, minRating=model$minRating)
    
  }
  
  ## construct recommender object
  new("Recommender", method = "SVD", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}

recommenderRegistry$set_entry(
  method="SVD", dataType = "realRatingMatrix", fun=REAL_SVD,
  description="Recommender based on SVD approximation (real data).")


