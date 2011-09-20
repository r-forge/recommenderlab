### Define new S4 generics

setGeneric(".splitKnownUnknown",
	function(data, ...) standardGeneric(".splitKnownUnknown"))

setGeneric("nitems",
	function(x, ...) standardGeneric("nitems"))

setGeneric("nusers",
	function(x, ...) standardGeneric("nusers"))

setGeneric("nratings",
	function(x, ...) standardGeneric("nratings"))

setGeneric("normalize",
	function(x, ...) standardGeneric("normalize"))

setGeneric("getData",
	function(x, ...) standardGeneric("getData"))

setGeneric("getModel",
	function(x, ...) standardGeneric("getModel"))

setGeneric("getRuns",
	function(x, ...) standardGeneric("getRuns"))

setGeneric("getConfusionMatrix",
	function(x, ...) standardGeneric("getConfusionMatrix"))

setGeneric("evaluate",
	function(x, method, ...) standardGeneric("evaluate"))

setGeneric("avg",
	function(x, ...) standardGeneric("avg"))

setGeneric("binarize",
	function(x, ...) standardGeneric("binarize"))

setGeneric("colCounts",
	function(x, ...) standardGeneric("colCounts"))

setGeneric("rowCounts",
	function(x, ...) standardGeneric("rowCounts"))

setGeneric("bestN",
	function(x, ...) standardGeneric("bestN"))

setGeneric("calcPredictionError", 
	function(x, data, ...) standardGeneric("calcPredictionError"))

setGeneric("evaluationScheme",
	function(data, ...) standardGeneric("evaluationScheme"))

setGeneric("Recommender",
	function(data, ...) standardGeneric("Recommender"))

setGeneric("similarity",
	function(x, y = NULL, method = NULL, args = NULL, ...) 
	standardGeneric("similarity"))

