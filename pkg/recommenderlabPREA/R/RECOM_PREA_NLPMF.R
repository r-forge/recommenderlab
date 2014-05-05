#Derek Phanekham
#11-14-2013

.REAL_PREA_NLPMF_PARAM <- list(
  k = 30, 
  method = "nlpmf",
  normalize = "center", 
  normalize_sim_matrix = FALSE,
  alpha = 0.5,
  na_as_zero = FALSE,
  minRating = NA,
  feature_count = 10,
  learning_rate = 0.0001,
  momentum = 0.9,
  max_iteration = 2,
  kernel_inv_wid = 1,
  kernel_var_rbf = 1,
  kernel_var_bias = 0.11,
  kernel_var_wht = 5,
  lazy = FALSE
)


#accepts a matrix of type 'realRatingMatrix'
REAL_PREA_NLPMF<- function(data, parameter= NULL) {
  
  param <- .get_parameters(.REAL_PREA_NLPMF_PARAM, parameter)
  strings <- .jarray( c(param$method, "simple", "0.2", param$feature_count, param$learning_rate, param$momentum,
                        param$max_iteration, param$kernel_inv_wid, param$kernel_var_rbf,
                        param$kernel_var_bias, param$kernel_var_wht))
  interface <- .jnew("CFInterface", check=TRUE, silent=FALSE)
  
  #sets up recommender. method found in AAA.R
  model <- recommender_setup(data, param, interface, strings)
  
  
  #This describes the model for R.
  #This is the predict function that will be used to
  #produce a top N list
  #and produce a matrix of ratings
  predict <- function(model, newdata, n = 10, data=NULL, type=c("topNList", "ratings"), ...) {
    strings <- .jarray( c(model$method, "simple", "0.2", model$feature_count, model$learning_rate, model$momentum,
                          model$max_iteration, model$kernel_inv_wid, model$kernel_var_rbf,
                          model$kernel_var_bias, model$kernel_var_wht))
    result <- predict_help(model, newdata, n, type, strings, model$interface)
    
  }
  
  ## construct recommender object
  new("Recommender", method = "PREA_NLPMF", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}


## register recommender
# is now in onLoad.R
