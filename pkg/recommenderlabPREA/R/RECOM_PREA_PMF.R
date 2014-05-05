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
  max_iteration = 20,
  lazy = FALSE
)


#accepts a matrix of type 'realRatingMatrix'
REAL_PREA_PMF<- function(data, parameter= NULL) {
  
  param <- .get_parameters(.REAL_PREA_PMF_PARAM, parameter)
  strings <- .jarray( c(param$method, "none", "0.2", param$feature_count,
                        param$learning_rate, param$regularizer, param$momentum, param$max_iteration))
  interface <- .jnew("CFInterface", check=TRUE, silent=FALSE)
  
  #sets up recommender. method found in AAA.R
  model <- recommender_setup(data, param, interface, strings)
  
  
  #This describes the model for R.
  #This is the predict function that will be used to
  #produce a top N list
  #and produce a matrix of ratings
  predict <- function(model, newdata, n = 10, data=NULL, type=c("topNList", "ratings"), ...) {
    strings <- .jarray( c(model$method, "none", "0.2", model$feature_count,
                          model$learning_rate, model$regularizer, model$momentum, model$max_iteration))
    
    predict_help(model, newdata, n, type, strings, model$interface)
    
   
  }
  
  ## construct recommender object
  new("Recommender", method = "PREA_PMF", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}


## register recommender
# is now in onLoad.R

