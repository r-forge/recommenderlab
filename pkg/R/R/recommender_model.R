## extract predicitve models

recommender_model <- function(x) UseMethod("recommender_model")

recommender_model.default <- function(x) 
    stop("recommender_model not implemented for this class!")

recommender_model.recommender  <- function(x) x$model

recommender_model.evaluation_results <- function(x) {
    lapply(x, attr, "model")
}


