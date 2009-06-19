## always recommends the top-N popular items (without known items)
build_recom_POPULAR <- function(data, parameter = NULL) {

    model <- list(
        description = "Order of items by popularity",
        pop_order = order(itemFrequency(data), decreasing=TRUE)
        )

    predict <- function(model, newdata, n=10) {
        m <- LIST(newdata, decode = FALSE)
        pop_order <- model$pop_order

        ## remove known items and take highest
        reclist <- lapply(m, FUN = function(x) 
            head(pop_order[!pop_order %in% x],n))
        
        encode(reclist, itemLabels(newdata))
    }

    structure(list(method = "POPULAR", n_train = length(data),
            model = model, predict = predict),
        class="recommender")
}

## register recommender
recommender_registry$set_entry(method="POPULAR", fun=build_recom_POPULAR, 
        description="Recommender based on item popularity.")

