## create a recommender (find recommender and use data for learning)

recommender <- function(data, method, parameter = NULL) {
    recom <- recommender_registry$get_entry(method)
    if(is.null(recom)) stop("Recommender method not implemented.")
    
    recom$fun(data = data, parameter = parameter)
}


print.recommender <- function(x, ...) {
    cat("Recommender model of type", x$method, "learned using", x$n_train, 
        "objects\n")
}

