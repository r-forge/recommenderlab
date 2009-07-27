## collaborative filtering  

## simple k-nearest neighbor
knn <- function(n, d) head(order(d, decreasing=FALSE), n+1)[-1]

BIN_UBCF <- function(data, parameter = NULL){
    
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
		d_cross_all <- dissimilarity(newdata, model$data, method = model$method)

        reclist <- list()
        for(i in 1:nrow(newdata)) {
            reclist[[i]] <- numeric(0)
            user <- newdata[i]

            ## cross dissimilarity
            d_cross <- d_cross_all[i,]
            neighbors <- knn(model$nn, d_cross)

            if(!model$weighted)
            recom <- order(colCounts(model$data[neighbors,]), decreasing=TRUE)
            else{
                w <- drop(d_cross[neighbors])

                ## make it a similarity
                w <- max(w, 1) - w

                m <- as(data[neighbors,], "matrix")
                res <- colSums(m * matrix(w, ncol=ncol(m),
                        nrow=nrow(m), byrow=TRUE))
                recom <- order(res, decreasing=TRUE)
            }


            ## remove known items
            knows <- unlist(LIST(user, decode = FALSE))
            reclist[[i]] <-head(recom[!recom %in% knows], n)

        }
        
		new("topNlist", items = reclist, itemLabels = colnames(newdata))
    }

	   ## construct recommender object
	   new("Recommender", method = "UBCF", dataType = "binaryRatingMatrix",
		   ntrain = nrow(data), model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(
	method="UBCF", dataType = "binaryRatingMatrix", fun=BIN_UBCF, 
	description="Recommender based on user-based collaborative filtering (binary data).")

