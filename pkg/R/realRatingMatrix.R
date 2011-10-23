
setAs("matrix", "realRatingMatrix",
	function(from) new("realRatingMatrix", 
		data = dropNA(from)))

## from a data.frame with columns user, item, rating
## this perserves 0s
setAs("data.frame", "realRatingMatrix", function(from) {
		user	<- from[,1]
		item	<- from[,2]
		rating	<- as.numeric(from[,3])

		i <- factor(user)
		j <- factor(item)

		dgT <- new("dgTMatrix", i = as.integer(i)-1L, j = as.integer(j)-1L, 
			x = rating,
			Dim = c(length(levels(i)), length(levels(j))),
			Dimnames = list(levels(i),levels(j)))
	
		new("realRatingMatrix", data = as(dgT, "dgCMatrix"))
	})


setAs("realRatingMatrix", "data.frame",
	function(from) { 
	    dgT <- t(as(from, "dgTMatrix"))	
	    df <- data.frame(user=rownames(from)[dgT@i+1L], 
		    item=colnames(from)[dgT@j+1L], 
		    rating=dgT@x)
	    
	    ## sort by users
	    df[order(df[,1]),]
	})


setAs("realRatingMatrix", "matrix",
	function(from) dropNA2matrix(from@data))

setAs("realRatingMatrix", "dgCMatrix",
	function(from) from@data)

setAs("realRatingMatrix", "dgTMatrix",
	function(from) as(from@data, "dgTMatrix"))

setAs("realRatingMatrix", "ngCMatrix",
	function(from) as(from@data, "ngCMatrix"))

setMethod("LIST", signature(from = "realRatingMatrix"),
	function(from, decode = TRUE, ratings = TRUE,...) {
		trip <- as(from@data, "dgTMatrix")
		lst <- split(trip@j+1L, trip@i)
		rts <- split(trip@x, trip@i)

		if(decode) lst <- lapply(lst, function(y) colnames(from)[y])
		
		if(ratings) {
			for(i in 1:length(rts)) {
				names(rts[[i]]) <- lst[[i]]
			}
		}else{
			rts <- lst
		}
		
		names(rts) <- rownames(from)
		rts
	})


setMethod("binarize", signature(x = "realRatingMatrix"),
	function(x, min_rating, ...){
		x <- x@data
		x@x <- as.numeric(x@x>=min_rating)
		x <- drop0(x)
		if(is.null(colnames(x))) colnames(x) <- 1:ncol(x)
		x <- new("itemMatrix", data = t(as(x, "ngCMatrix")), 
			itemInfo = data.frame(labels=colnames(x)))
		new("binaryRatingMatrix", data = x)
	})


setMethod("getRatings", signature(x = "realRatingMatrix"),
	function(x) x@data@x )


setMethod("removeKnownRatings", signature(x = "realRatingMatrix"),
	function(x, known) {
	    if(!is(known, "realRatingMatrix")) stop("known needs to be a realRatingMatrix!")
	    
	    ## FIXME: make sparse
	    xm <- dropNA2matrix(x@data)
	    xm[as(as(known@data, "ngCMatrix"),"matrix")] <- NA
	    x@data <- dropNA(xm)
	    x
	})

setMethod("getTopNLists", signature(x = "realRatingMatrix"),
	function(x, n= 10, min_rating = NA){
	    n <- as.integer(n)
	    x.m <- as(x, "matrix")

	    if(!is.na(min_rating)) x.m[x.m<min_rating] <- NA

	    reclist <- lapply(1:nrow(x), FUN=function(i) {
			head(order(as(x.m[i,],"matrix"), 
					decreasing=TRUE, na.last=NA), n)
		    })
	    
	    new("topNList", items = reclist, itemLabels = colnames(x), n = n)
	})


### compute standard deviation

.dgC2list <- function(x, row=TRUE) {
 if(row) x <- t(x)   
 lapply(2:length(x@p), FUN = function(i) {
	     if(x@p[i-1L]==x@p[i]) numeric(0)
	     else x@x[(x@p[i-1L]+1L):x@p[i]]
	 })
}

setMethod("rowSds", signature(x = "realRatingMatrix"),
	function(x, ...) {
	    s <- sapply(.dgC2list(x@data, row=TRUE), sd)
	    names(s) <- rownames(x)
	    s
	})


setMethod("colSds", signature(x = "realRatingMatrix"),
	function(x, ...) {
	    s <- sapply(.dgC2list(x@data, row=FALSE), sd)
	    names(s) <- colnames(x)
	    s
	})



## create test data
setMethod(".splitKnownUnknown", signature(data="realRatingMatrix"),
	function(data, given) {

		## given might of length one or length(data)
		if(length(given)==1) given <- rep(given, nrow(data))

		## we create a logical mask via a triplet Matrix
		trip <- as(data, "dgTMatrix")
		items <- lapply(0:(nrow(data)-1), function(i) which(trip@i == i))
		take <- unlist(lapply(items, sample, given))

		tripUnknown <- trip
		tripUnknown@x <- tripUnknown@x[-take]
		tripUnknown@i <- tripUnknown@i[-take]
		tripUnknown@j <- tripUnknown@j[-take]
		tripKnown <- trip
		tripKnown@x <- tripKnown@x[take]
		tripKnown@i <- tripKnown@i[take]
		tripKnown@j <- tripKnown@j[take]

		known <- new("realRatingMatrix", 
			data = as(tripKnown, "dgCMatrix"))
		unknown <- new("realRatingMatrix", 
			data = as(tripUnknown, "dgCMatrix"))

		list(
			known = known,
			unknown = unknown
		)
	})


