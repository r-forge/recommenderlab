
setAs("topNlist", "dgTMatrix",
	function(from) {
		i <- rep(1:length(from@items), lapply(from@items, length))
		j <- unlist(from@items)
		x <- rep(1, length(j))

		new("dgTMatrix", i = i-1L, j = as.integer(j)-1L,
			x = x,
			Dim = c(length(from@items), length(from@itemLabels)),
			Dimnames = list(names(from@items), from@itemLabels))
	})


setAs("topNlist", "dgCMatrix",
	function(from) as(as(from, "dgTMatrix"),"dgCMatrix"))

setAs("topNlist", "ngCMatrix",
	function(from) as(as(from, "dgCMatrix"), "ngCMatrix"))

setAs("topNlist", "matrix",
	function(from) as(as(from, "dgTMatrix"),"matrix"))

setMethod("LIST", signature(from = "topNlist"),
	function(from, decode = TRUE)
	if(decode) lapply(from@items, function(y) from@itemLabels[y])
	else from@items)

setAs("topNlist", "list", function(from) LIST(from, decode = TRUE))

setMethod("show", signature(object = "topNlist"),
	function(object) {
		cat(class(object), 'for',
			length(object@items),'users.','\n')
		invisible(NULL)
	})

setMethod("bestN", signature(x = "topNlist"),
	function(x, n = 10) 
	new("topNlist", items = lapply(x, head, n), itemLabels = x@itemlabels))
	
setMethod("colCounts", signature(x = "topNlist"),
	function(x, ...) colSums(as(x, "ngCMatrix")))

setMethod("rowCounts", signature(x = "topNlist"),
	function(x, ...) sapply(x@items, length))


