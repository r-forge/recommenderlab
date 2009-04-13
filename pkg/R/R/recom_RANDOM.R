## recommend random items
build_recom_RANDOM <- function(data=NULL, parameter=NULL) {

    predict <- function(model=NULL, newdata, n=10) {
        itemlist <- 1:nitems(newdata)
        
        ## remove known items and sample
        reclist <- lapply(LIST(newdata, decode = FALSE), 
            FUN = function(x) sample(itemlist[-x], n))
        
        ## encode as itemMatrix
        encode(reclist, itemLabels(newdata))
    }

    ## this recommender has no model
    structure(list(method = "RANDOM", n_train = length(data), 
            model = NULL, predict = predict),
        class="recommender")
}

## register recommender
recommender_registry$set_entry(method="RANDOM", fun=build_recom_RANDOM, 
        description="Produce random recommendations.")

