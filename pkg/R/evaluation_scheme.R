evaluation_scheme <- function(data, method="split", train=0.9, k=10, given=3) {

    k <- as.integer(k)
    n <- length(data)

    ## given can be one integer or a vector of length data
    given <- as.integer(given)
    if(length(given)!=1 && length(given)!=n)
        stop("length of given has to be one or length of data!")
    
    ## check size
    if(any(size(data)<given))
        stop("Some observations have size<given!")

    ## methods
    methods <- c("split", "cross-validation", "bootstrap")
    method_ind <- pmatch(method, methods)
    if(is.na(method_ind)) stop("Unknown method!")
    method <- methods[method_ind]
    
    ## split
    if(method_ind == 1) runs_train <- replicate(k, sample(1:n), 
            n*train, simplify = FALSE)

    ## cross-validation
    else if(method_ind == 2) {
        train <- NA
        
        times <- as.integer(n/k)
        fold_ids <- sample(rep(1:k, times), times*k)
        runs_train <- lapply(1:k, FUN = function(i) which(fold_ids!=i))
    }

    else if(method_ind == 3) runs_train <- replicate(k, sample(1:n, 
            n*train, replace = TRUE), simplify = FALSE)

    
    ## create test data
    .split_known_unknown <- function(data, given) {
        
        if(length(given)==1) given <- rep(given, length(data))
        
        l <- LIST(data, decode=FALSE)
        known_index <- lapply(1:length(l), 
            FUN = function(i) sample(1:length(l[[i]]), given[i]))
        list(
            known = encode(
                lapply(1:length(data), FUN = function(x) 
                    l[[x]][known_index[[x]]]),
                itemLabels = itemLabels(data)),
            unknown = encode(
                lapply(1:length(data), FUN = function(x) 
                    l[[x]][-known_index[[x]]]),
                itemLabels = itemLabels(data))
        )
    }
    
    test_data <- .split_known_unknown(data, given)

    structure(list(method=method, k=k, n=n, 
            train=train, given=given, 
            runs_train=runs_train,
            data=data, known=test_data$known, unknown=test_data$unknown), 
        class=c("evaluation_scheme"))
}


evaluation_data <- function(scheme, 
    type = c("train", "known", "unknown"), run=1) {
    if(!is(scheme, "evaluation_scheme")) stop("Not an evaluation scheme!")
    if(run > scheme$k) stop("Scheme does not contain that many runs!")

    type <- match.arg(type)
    switch(type,
        train = scheme$data[scheme$runs_train[[run]]],
        known = scheme$known[-scheme$runs_train[[run]]],
        unknown = scheme$unknown[-scheme$runs_train[[run]]]
    )
}

print.evaluation_scheme <- function(x) {
   
    if(length(x$given)==1) {
        writeLines(sprintf("Evaluation scheme with %d items given", x$given))
    }else{
        writeLines(c(
                "Evaluation scheme with multiple items given",
                "Summary:"
            ))
        print(summary(x$given))
    }

    writeLines(c(
            sprintf("Method '%s' with %d runs (training set proportion: %1.3f)", 
                x$method, x$k, x$train),
            sprintf("Data set size: %d objects", x$n)
    ))
}

