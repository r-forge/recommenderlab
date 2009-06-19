## create recommendations by clustering

build_recom_CLUSTER <- function(data, parameter = NULL) {
    
    p <- .get_parameters(list(
            k = 20,
            method="jaccard"
        ), parameter)
    
    d <- dissimilarity(data, method = p$method)
    clust <- pam(d, k=p$k)

    medoids <- data[clust$id.med]
    cl <- clust$clustering

    profiles <- lapply(1:p$k, FUN = function(i) order(
                itemFrequency(data[cl==i]), decreasing=TRUE))

    model <- c(list(
            description = "CLUSTER: cluster medoids and profiles",
            medoids = medoids, 
            profiles = profiles
        ), p
    )


    ## predict cluster
    predict <- function(model, newdata, n=10) {

        ## what is wrong with the NAMESPACE here?
        #cl <-  predict(model$medoids,  newdata)
        cl <-  getMethod("predict", "itemMatrix")(model$medoids,  newdata)
        
        reclist <- list()
        for(j in 1:length(newdata)) {
            user <- newdata[j]

            recom <- model$profiles[[cl[j]]]

            knows <- unlist(LIST(user, decode = FALSE))
            reclist[[j]] <- head(recom[!recom %in% knows], n)

        }

        encode(reclist, itemLabels(newdata))
    
    }

    structure(list(method = "CLUSTER", n_train = length(data),
            model = model, predict = predict),
        class="recommender")


}

## register recommender
recommender_registry$set_entry(method="CLUSTER", fun=build_recom_CLUSTER, 
        description="Recommender based on clustering.")

