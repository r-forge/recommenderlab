setMethod("normalize", signature(x = "realRatingMatrix"),
	function(x, row=TRUE, ...){
	    data <- t(x@data)
	    data@x <- data@x-rep(rowMeans(x), rowCounts(x))
	    x@data <- t(data)
	    x
	})
		
