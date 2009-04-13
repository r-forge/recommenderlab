## collaborative filtering  

## simple k-nearest neighbor
knn <- function(n, d) head(order(d, decreasing=FALSE), n+1)[-1]

build_recom_UBCF <- function(data, parameter = NULL){
    
    p <- .get_parameters(list( 
            method = "jaccard", 
            nn = 25, 
            weighted=FALSE,
            sample = FALSE
        ), parameter) 

    if(p$sample) data <- sample(data, p$sample)

    model <- c(list(
            description = "UBCF: contains full or sample of data set",
            data = data
        ), p 
    )

    predict <- function(model, newdata, n=10) {
        
        ## cross dissimilaries
        d_cross_all <- dissimilarity(newdata, model$data, 
            method = model$method)
        
        reclist <- list()
        for(i in 1:length(newdata)) {
            reclist[[i]] <- numeric(0)
            user <- newdata[i]

            ## cross dissimilarity
            d_cross <- d_cross_all[i,]
            neighbors <- knn(model$nn, d_cross)

            if(!model$weighted)
            recom <- order(itemFrequency(model$data[neighbors], type = "abs"), 
                    decreasing=TRUE)
            else{
                w <- drop(d_cross[neighbors])

                ## make it a similarity
                w <- max(w, 1) - w

                m <- as(data[neighbors], "matrix")
                res <- colSums(m * matrix(w, ncol=ncol(m),
                        nrow=nrow(m), byrow=TRUE))
                recom <- order(res, decreasing=TRUE)
            }


            ## remove known items
            knows <- unlist(LIST(user, decode = FALSE))
            reclist[[i]] <-head(recom[!recom %in% knows], n)

        }
        
        encode(reclist, itemLabels(newdata))
    
    }

    structure(list(method = "UBCF", n_train = length(data),
            model = model, predict = predict),
        class="recommender")

}

## register recommender
recommender_registry$set_entry(method="UBCF", fun=build_recom_UBCF, 
        description="Recommender based on user-based collaborative filtering.")

