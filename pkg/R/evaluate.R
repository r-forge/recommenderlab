setMethod("evaluate", signature(x = "evaluationScheme", method = "character"),
	function(x, method, n=1:10, parameter=NULL, 
		progress = TRUE, keep_model=FALSE) {

		scheme <- x
		## fixme: do individual runs missing
		runs <- 1:scheme@k

		## fixme: parameter vector missing
		cm <- list()
		#  if(is.vector(n)) { 
			for(r in runs) {
				if(progress) cat("Run ", r, ": ", cat="")

				cm[[r]] <- .do_run_by_n(scheme, method, 
					run=r, n=n, parameter=parameter, 
					progress=progress, keep_model=keep_model)

				if(progress) cat("\n")

			}
			#  }

		new("evaluationResults", results = cm, 
			method=recommenderRegistry$get_entry(method)$method)
	})

setMethod("evaluate", signature(x = "evaluationScheme", method = "list"),
	function(x, method, n=1:10, parameter=NULL, 
		progress = TRUE, keep_model=FALSE) {
	
		## method is a list of lists
		#list(RANDOM = list(name = "RANDOM", parameter = NULL), 
		#	POPULAR = list(...
		
		conf_m <- lapply(method, FUN = function(a) evaluate(es, a$n,
				n = n , parameter = a$p))	
	
		as(conf_m, "evaluationResultList")
	})



.do_run_by_n <- function(scheme, method, run, n, parameter = NULL,
	progress=FALSE, keep_model=TRUE) {

	## prepare data
	train <- getData(scheme, type="train", run=run)
	test_known <- getData(scheme, type="known", run=run)
	test_unknown <- getData(scheme, type="unknown", run=run)

	## train recommender
	r <- Recommender(train, method, parameter=parameter)
	cm <- matrix(NA, nrow=length(n), ncol=9, 
		dimnames= list(n=n, 
			c("TP", "FP", "FN", "TN", "PP", "recall","precision","FPR","TPR")))

	for(i in 1:length(n)) {
		NN <- n[i]

		if(progress) cat(NN, " ")

		pred <- predict(r, test_known, n=NN)

		## create confusion matrix
		tp <- colSums(as(pred, "ngCMatrix")*as(test_unknown, "ngCMatrix"))
		tp_fn <- colCounts(test_unknown)
		tp_fp <- colCounts(pred)

		cm[i, "TP"] <- mean(tp)
		cm[i, "FP"] <- mean(tp_fp - tp)
		cm[i, "FN"] <- mean(tp_fn - tp)
		cm[i, "TN"] <- mean(ncol(train) - tp_fp - tp_fn + tp)
		cm[i, "PP"] <- mean(tp_fp)

		## calculate some important measures
		cm[i, "precision"] <- cm[i, "TP"] / (cm[i, "TP"] + cm[i, "FP"]) 
		cm[i, "recall"] <- cm[i, "TP"] / (cm[i, "TP"] + cm[i, "FN"]) 
		cm[i, "TPR"] <- cm[i, "recall"] 
		cm[i, "FPR"] <- cm[i, "FP"] / (cm[i, "FP"] + cm[i, "TN"]) 

	}

	new("confusionMatrix", cm = cm, model = 
		if(keep_model) getModel(r) else NULL)
}

