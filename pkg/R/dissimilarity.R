##*******************************************************************
## dissimilarity for binaryRatingMatrix
setMethod("dissimilarity", signature(x = "binaryRatingMatrix"),
	function(x, y = NULL, method = NULL, args = NULL, which = "users") {
	    
	    args <- .get_parameters(list(alpha=.5), args)
	   
	    method <- tolower(method)
	    which <- tolower(which)

	    ## conditional similarity (Karypis 2001)
	    ## returned as distance!
	    if(method == "conditional" 
		    && is.null(y) 
		    && which == "items") {
		sim <- .conditional_sim(as(x, "dgCMatrix"), args)
		return(as.dist(1/(1+sim)))
	    }

	    ## Karypis similarity
	    ## returned as distance!
	    if(method == "karypis" 
		    && is.null(y) 
		    && which == "items") {
		sim <- .karypis_sim(as(x, "dgCMatrix"), args)
		return(as.dist(1/(1+sim)))
	    }

	
	    ## dissimilarity is defined in arules for itemMatrix
	    if(which == "users") which <- "transactions" ## "items" is ok
	    x <- x@data
	    if(!is.null(y)) y <- y@data

	    dissimilarity(x, y, method, args, which)
	}
	)



##*******************************************************************
## wrapper for realRatingMatrix (transactions)
## by Christopher Köb

setMethod("dissimilarity", signature(x = "realRatingMatrix"),
	function(x, y = NULL, method = NULL, args = NULL, 
		which = "users") {

	    args <- .get_parameters(list(na_as_zero = FALSE, alpha=.5), args)

	    ### FIX this code!
	    ## shortcut for Cosine (compute sparse)
	    #if(tolower(method)=="cosine" && is.null(y)) {
	    #	x <- as(x, "dgCMatrix")
	    #	return(as.dist(1- crossprod(x / sqrt(rowSums(x ^ 2)))))
	    #}
	    
	    ## conditional similarity (Karypis 2001)
	    ## returned as distance!
	    if(method == "conditional" 
		    && is.null(y) 
		    && which == "items") {
		sim <- .conditional_sim(as(x, "dgCMatrix"), args)
		return(as.dist(1-sim))
	    }
	    
	    ## Karypis similarity
	    ## returned as distance!
	    if(method == "karypis" 
		    && is.null(y) 
		    && which == "items") {
		sim <- .karypis_sim(as(x, "dgCMatrix"), args)
		return(as.dist(1-sim))
	    }

	    ## do regular distances
	    
	    x <- as(x, "dgCMatrix")
	    if(tolower(which) == "items") x <- t(x) 
	    x <- as(x, "matrix")
	    ## 0 in the rating matrix means missing value!
	    if(!args$na_as_zero) x[x==0] <- NA


	    if(!is.null(y)) { 
		y <- as(y, "dgCMatrix")
		if(tolower(which) == "items") y <- t(y) 
		y <- as(y, "matrix")
		if(!args$na_as_zero) y[y==0] <- NA
	    }

	    ## dist in proxy
	    dist(x = x, y = y, method = method)
	})

setMethod("similarity", signature(x = "ratingMatrix"),
	function(x, y = NULL, method = NULL, args = NULL, 
		which = "users") {

	    d <- dissimilarity(x, y, method, args, which)
	    
	    ## FIXME: other measures in [0,1]
	    if(tolower(attr(d, "method")) %in% c("jaccard", "cosine")) {
		sim <- 1-d
	    }else {
		sim <- 1/(1+dissimilarity(x, y, method, args, which))
	    }
	    attr(sim, "type") <- "simil"
	    sim
	})

## conditional similarity (Karypis 2001)
.conditional_sim <- function(x, args=NULL){
    n <- ncol(x)

    ## sim(v,u) = freq(uv) / freq(v)
    uv <-  crossprod(x)
    v <- matrix(colSums(x), nrow = n, ncol = n, byrow = FALSE)

    sim <- uv/v

    ## fix if freq was 0
    sim[is.na(sim)] <- 0

    sim
}
	    
## Karypis similarity
.karypis_sim <- function(x, args=NULL) {
    
    ## get alpha
    args <- .get_parameters(list(alpha = .5), args)
    
    n <- ncol(x)

    ## normalize rows to unit length
    x <- x/rowSums(x)

    ## for users without items
    x[is.na(x)] <- 0

    ## sim(v,u) = 
    ##      sum_{for all i: r_i,v >0} r_i,u / freq(v) / freq(u)^alpha
    uv <-  crossprod(x, x>0)
    v <- matrix(colSums(x), nrow = n, ncol = n, byrow = FALSE)
    u <- t(v) 

    sim <- uv/v/u^args$alpha 

    ##  fix if freq = 0
    sim[is.na(sim)] <- 0
    sim

}

