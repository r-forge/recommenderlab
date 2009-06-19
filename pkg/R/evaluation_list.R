## convenience functions to work with a list of evaluation results

## construct a evaluation list
evaluation_list <- function(x) {
    if(!all(sapply(x, is, "evaluation_results"))) stop("All list elements have to be of class 'evaluation_results'")
    
    class(x) <- "evaluation_list"
    x
}

## plot
plot.evaluation_list <- function(x, plot_type=c("ROC", "prec/rec"), 
    xlim=NULL, ylim=NULL, col = NULL, pch = 1, lty = 1, 
    annodate= 0, legend="bottomright") {

    ## find best xlim, ylim
    plot_type <- match.arg(plot_type)
    take <- if(plot_type == "ROC") c("FPR", "TPR") else c("recall", "precision")

    max_lim <- apply(sapply(x, FUN = 
            function(y) apply(mean(y)[,take], MARGIN=2, max)), MARGIN=1, max)

    if(is.null(xlim)) xlim <- c(0, max_lim[1])
    if(is.null(ylim)) ylim <- c(0, max_lim[2])
    
    ## fix pch, lty and col
    if(length(pch)==1) pch <- rep(pch, length(x))
    if(length(lty)==1) lty <- rep(lty, length(x))
    
    if(is.null(col)) col <- 1:length(x)
    if(length(col)==1) col <- rep(col, length(x))
        

    plot(NA, xlab=take[1], ylab=take[2], ylim=ylim, xlim=xlim)
    legend(x=legend, legend=names(x), col=col, 
        pch = pch, lty=lty, bty="n")
    for(i in 1:length(x)) plot(x[[i]], plot_type=plot_type, 
        add=TRUE, col=col[i], type="o", annodate = i %in% annodate, 
        pch = pch[i], lty=lty[i])
}

## mean
mean.evaluation_list <- function(x) {
    lapply(x, mean)
}

print.evaluation_list <- function(x) {
    writeLines("Evaluation list:")
    print(unclass(x))
}

"[.evaluation_list" <- function(x, ...) {
    x <- "["(unclass(x), ...)
    class(x) <- "evaluation_list"
    x
}
