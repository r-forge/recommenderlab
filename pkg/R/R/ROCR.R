## create a ROCR prediction object
ROCR_prediction <- function(x) {

    if(!require("ROCR")) stop("Required package ROCR not installed!")
    if(!is(x, "prediction_results")) stop("Not a prediction results object!")

    pred <-new("prediction")
    cutoffs <- rownames(x[[1]])
    k <- length(x)
    pred@predictions <- rep(list(rep(1,length(cutoffs))), k)
    pred@labels <- rep(list(rep(1, length(cutoffs))), k)
    pred@cutoffs <- rep(list(as.numeric(cutoffs)), k)
    
    for(i in 1:k) {
        pred@tp[[i]] <- x[[i]][,"TP"]
        pred@fp[[i]] <- x[[i]][,"FP"]
        pred@fn[[i]] <- x[[i]][,"FN"]
        pred@tn[[i]] <- x[[i]][,"TN"]
    
        pred@n.pos[[i]] <- pred@tp[[i]] + pred@fn[[i]]
        pred@n.neg[[i]] <- pred@tn[[i]] + pred@fp[[i]]
        pred@n.pos.pred[[i]] <- pred@tp[[i]] + pred@fp[[i]]
        pred@n.neg.pred[[i]] <- pred@tn[[i]] + pred@fn[[i]]
    }

    pred
}
