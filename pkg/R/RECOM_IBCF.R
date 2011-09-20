## item-based top N recomender (see Karypis 2001)

BIN_IBCF <- function(data, parameter= NULL) {
    
	p <- .get_parameters(list(
            k = 30, 
            method="Jaccard",
            normalize = FALSE, 
            alpha = 0.5), parameter)
    
    
    ## for other similarites: use 1/1+ dissimilarites defined in arules
    sim <- 1 / (1+as.matrix(dissimilarity(data, which = "items",
            method = p$method, args=list(alpha=p$alpha))))
    
    ## normalize rows to 1
    if(p$normalize) sim <- sim/rowSums(sim)
    
    ## reduce similarity matrix to keep only the k highest similarities
    diag(sim) <- 0
    for(i in 1:nrow(sim)) {
        o <- order(sim[i,], decreasing=TRUE)
        o <- o[(p$k+1):length(o)]
        sim[i,o] <- 0
    }

    ## normalize rows to 1 after
    #if(normalize) s_items <- s_items/rowSums(s_items)
    
    ## make sparse
    sim <- as(sim, "dgCMatrix")

    model <- c(list(
            description = "IBCF: Reduced similarity matrix",
            sim = sim
        ), p
    )

    predict <- function(model, newdata, n = 10, ...) {
	n <- as.integer(n)

	sim <- model$sim 

	reclist <- list()
	for(i in 1:nrow(newdata)) {
	    user <- newdata[i,] 
	    u <- as(user, "dgCMatrix")

	    known <- as.logical(u)
	    u <- u[, known]
	    sim_u <- sim[known,]

	    ## add similarites for known items
	    x <- crossprod(sim_u, u) / colSums(sim_u, na.rm=TRUE)

	    ## find N items with highest score
	    reclist[[i]] <- head(order(as(x, "vector"), decreasing=TRUE), n)
	}

	new("topNList", items = reclist, itemLabels = colnames(newdata), n = n)

    }

	## construct recommender object
	new("Recommender", method = "IBCF", dataType = class(data),
		ntrain = nrow(data), model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(
	method="IBCF", dataType = "binaryRatingMatrix", fun=BIN_IBCF, 
	description="Recommender based on item-based collaborative filtering (binary rating data).")

REAL_IBCF <- function(data, parameter= NULL) {

    p <- .get_parameters(list(
		    k = 30, 
		    method="Cosine",
		    normalize = FALSE, 
		    alpha = 0.5,
		    na_as_zero = FALSE
		    ), parameter)


    data <- normalize(data)
    
    ## this might not fit into memory! Maybe use a sample?
    sim <- as.matrix(1/(1+ 
	    dissimilarity(db, method=p$method, which="items", 
		args=list(alpha=p$alpha, na_as_zero=p$na_as_zero))))

    ## normalize rows to 1
    if(p$normalize) sim <- sim/rowSums(sim)

    ## reduce similarity matrix to keep only the k highest similarities
    diag(sim) <- NA
    ##sim[!is.finite(sim)] <- NA

    for(i in 1:nrow(sim)) 
	sim[i,head(order(sim[i,], decreasing=FALSE, na.last=FALSE), 
	    ncol(sim) - p$k)] <- 0
    
    ## make sparse
    sim <- as(sim, "dgCMatrix")


    model <- c(list(
		    description = "IBCF: Reduced similarity matrix",
		    sim = sim
		    ), p
	    )

    predict <- function(model, newdata, n = 10, 
	    type=c("topNList", "ratings"), ...) {
	
	type <- match.arg(type)
	n <- as.integer(n)
	sim <- model$sim 
	u <- as(newdata, "dgCMatrix")
	
	## predict all ratings
	ratings <- tcrossprod(sim,u) / tcrossprod(sim, u!=0)

	## remove known ratings
	ratings[as(t(u!=0), "matrix")] <- NA

	if(type=="ratings") {
	    return(as(as.matrix(ratings), "realRatingMatrix"))
	}

	reclist <- apply(ratings, MARGIN=2, FUN=function(x) 
		head(order(x, decreasing=TRUE, na.last=NA), n))
	
	if(!is(reclist, "list")) reclist <- lapply(1:ncol(reclist), 
		FUN=function(i) reclist[,i])

	new("topNList", items = reclist, itemLabels = colnames(newdata), n = n)

    }

    ## construct recommender object
    new("Recommender", method = "IBCF", dataType = class(data),
	    ntrain = nrow(data), model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(
	method="IBCF", dataType = "realRatingMatrix", fun=REAL_IBCF,
	description="Recommender based on item-based collaborative filtering (real data).")

