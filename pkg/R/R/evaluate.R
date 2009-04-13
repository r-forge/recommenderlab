evaluate <- function(scheme, method, n=1:10, parameter=NULL, 
    progress = TRUE, keep_model=FALSE) {
    if(!is(scheme, "evaluation_scheme"))
    stop("scheme not of class evaluation_scheme!")


    ## fixme: do individual runs missing
    runs <- 1:scheme$k
    
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
            
    class(cm) <- "evaluation_results"
    cm
}


.do_run_by_n <- function(scheme, method, run, n, parameter = NULL,
    progress=FALSE, keep_model=TRUE) {

    ## prepare data
    train <- evaluation_data(scheme, type="train", run=run)
    test_known <- evaluation_data(scheme, type="known", run=run)
    test_unknown <- evaluation_data(scheme, type="unknown", run=run)

    ## train recommender
    r <- recommender(train, method, parameter=parameter)
    cm <- matrix(NA, nrow=length(n), ncol=9, 
        dimnames= list(n=n, 
            c("TP", "FP", "FN", "TN", "PP", "recall","precision","FPR","TPR")))

    for(i in 1:length(n)) {
        NN <- n[i]

        if(progress) cat(NN, " ")

        pred <- predict(r, test_known, n=NN)

        ## create confusion matrix
        tp <- colSums(as(pred, "ngCMatrix")*as(test_unknown, "ngCMatrix"))
        tp_fn <- size(test_unknown)
        tp_fp <- size(pred)

        cm[i, "TP"] <- mean(tp)
        cm[i, "FP"] <- mean(tp_fp - tp)
        cm[i, "FN"] <- mean(tp_fn - tp)
        cm[i, "TN"] <- mean(nitems(train) - tp_fp - tp_fn + tp)
        cm[i, "PP"] <- mean(tp_fp)
    
        ## calculate some important measures
        cm[i, "precision"] <- cm[i, "TP"] / (cm[i, "TP"] + cm[i, "FP"]) 
        cm[i, "recall"] <- cm[i, "TP"] / (cm[i, "TP"] + cm[i, "FN"]) 
        cm[i, "TPR"] <- cm[i, "recall"] 
        cm[i, "FPR"] <- cm[i, "FP"] / (cm[i, "FP"] + cm[i, "TN"]) 
    
    }
    
    if(keep_model) attr(cm, "model") <- recommender_model(r)
    cm
}

print.evaluation_results <- function(x) {
    writeLines(sprintf("Evaluation results for %d runs:", length(x)))
    if(!is.null(attr(x[[1]], "model"))) {
        writeLines("Result contains predictive models!")
        x <- lapply(x, FUN = function(y) {attr(y, "model") <- NULL; y})
    }

    print(unclass(x))
}

recommender_model.evaluation_results <- function(x) {
    lapply(x, attr, "model")
}

mean.evaluation_results <- function(x) {
    avg <- x[[1]]
    for(i in 2:length(x)) avg <- avg+x[[i]]
    x <- avg/length(x)
    attr(x, "model") <- NULL
    x
}

plot.evaluation_results <- function(x, plot_type=c("ROC", "prec/rec"), 
    add=FALSE, type= "l", annodate = FALSE, ...) {
    plot_type <- match.arg(plot_type)

    ## if not ROC then prec/recall
    if(plot_type == "ROC") take <- c("FPR", "TPR")
    else take <- c("recall", "precision")

    if(length(x)>1) x <- mean(x)

    x <- x[,take]
    if(add) lines(x, type=type,...) 
    else plot(x, type=type, ...)

    ## add annodations
    if(annodate) text(x[,1], x[,2], pos=3, rownames(x))
}

