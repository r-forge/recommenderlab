## collaborative filtering  

## simple k-nearest neighbor
knn <- function(n, d) head(order(d, decreasing=FALSE, na.last=NA), n)

BIN_UBCF <- function(data, parameter = NULL){

    p <- .get_parameters(list( 
                    method = "jaccard", 
                    nn = 25, 
                    weighted=FALSE,
                    sample = FALSE
                    ), parameter) 

    if(p$sample) data <- sample(data, p$sample)

    model <- c(list(
                    description = "UBCF-Binary Data: contains full or sample of data set",
                    data = data
                    ), p 
            )

    predict <- function(model, newdata, n=10, ...) {
        n <- as.integer(n)

        ## cross dissimilaries
        
        ## fixme: add Weiss dissimilarity
        if(!is.na(pmatch(tolower(model$method), "weiss"))) {
            
            ## add bonus
            nd <- as(newdata, "dgCMatrix")+1/colSums(model$data)

            
            ## number of matching 1s is just the cross-product x %*% t(y)
            d_cross_all <- 1/t(tcrossprod(as(model$data, "dgCMatrix"), 
                    nd))
                            #as(newdata, "dgCMatrix")))
        }else{
            d_cross_all <- dissimilarity(newdata, model$data, 
                    method = model$method)
        }

        reclist <- list()
        for(i in 1:nrow(newdata)) {
            reclist[[i]] <- numeric(0)
            user <- newdata[i]

            ## find knn
            d_cross <- d_cross_all[i,]
            neighbors <- knn(model$nn, d_cross)

            if(!model$weighted)
                recom <- order(colCounts(model$data[neighbors,]), 
		    decreasing=TRUE)
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

        new("topNList", items = reclist, itemLabels = colnames(newdata), n = n)
    }

    ## construct recommender object
    new("Recommender", method = "UBCF", dataType = class(data),
            ntrain = nrow(data), model = model, predict = predict)
}

REAL_UBCF <- function(data, parameter = NULL){

    p <- .get_parameters(list( 
                    method = "cosine", 
                    nn = 25, 
                    weighted=FALSE,
                    sample = FALSE,
		    normalize="center",
		    min_rating = NA
                    ), parameter) 

    if(p$sample) data <- sample(data, p$sample)
    
    ## normalize data
    if(!is.null(p$normalize)) data <- normalize(data, method=p$normalize)

    model <- c(list(
                    description = "UBCF-Real data: contains full or sample of data set",
                    data = data
                    ), p 
            )

    predict <- function(model, newdata, n=10, 
	    type=c("topNList", "ratings"), ...) {

	type <- match.arg(type)
	n <- as.integer(n)
	
	if(!is.null(model$normalize)) 
	    newdata <- normalize(newdata, method=model$normalize)

	## similarities
	sim <- similarity(newdata, model$data, 
		method = model$method)

	neighbors <- apply(sim, MARGIN=1, FUN=function(x) head(
			order(x, decreasing=TRUE, na.last=TRUE), model$nn))

	## r_ui = r_u_bar + [sum_k s_uk * r_ai - r_a_bar] / sum_k s_uk
	## k is the neighborhood
	## r_ai - r_a_bar_ is normalize(r_ai) = newdata

	s_uk <- sapply(1:nrow(sim), FUN=function(x) 
		sim[x, neighbors[,x]])
	sum_s_uk <- colSums(s_uk)

	## calculate the weighted sum
	r_a_norms <- sapply(1:nrow(newdata), FUN=function(i) {
		    ## neighbors ratings of active user i
		    r_neighbors <- as(model$data[neighbors[,i]], "dgCMatrix") 
		    drop(as(crossprod(r_neighbors, s_uk[,i]), "matrix"))
		})	

	ratings <- t(r_a_norms)/sum_s_uk
	
	## remove known items
	ratings[as(as(newdata, "ngCMatrix"), "matrix")] <- NA
	ratings <- as(ratings, "realRatingMatrix")
	ratings@normalize <- newdata@normalize
	
	## denormalize
	if(!is.null(model$normalize)) 
	    ratings <- denormalize(ratings)

	if(type=="ratings") return(ratings)

	getTopNLists(ratings, n=n, min_rating=model$min_rating)
    }

    ## construct recommender object
    new("Recommender", method = "UBCF", dataType = class(data),
            ntrain = nrow(data), model = model, predict = predict)
}


## register recommender
recommenderRegistry$set_entry(
        method="UBCF", dataType = "binaryRatingMatrix", fun=BIN_UBCF, 
        description="Recommender based on user-based collaborative filtering (binary data).")


recommenderRegistry$set_entry(
	method="UBCF", dataType = "realRatingMatrix", fun=REAL_UBCF,
	description="Recommender based on user-based collaborative filtering (real data).")


