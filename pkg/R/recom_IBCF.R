## item-based top N recomender (see Karypis 2001)

build_recom_IBCF <- function(data, parameter= NULL) {
    
    p <- .get_parameters(list(
            k = 30, 
            method="jaccard",
            normalize = FALSE, 
            alpha = 0.5), parameter)
    
    
    ## conditional similarity (Karypis 2001)
    if(p$method == "conditional") {
        r <- as(data, "matrix")
        n <- nitems(data)

        ## sim(v,u) = freq(uv) / freq(v)
        uv <-  crossprod(r)
        v <- matrix(colSums(r), nrow = n, ncol = n, byrow = FALSE)

        sim <- uv/v

        ## fix if freq was 0
        sim[is.na(sim)] <- 0
    }
    
    else 
    
    ## Karypis similarity
    if(p$method == "karypis") {
        r <- as(data, "matrix")
        n <- nitems(data)

        ## normalize rows to unit length
        r <- r/rowSums(r)

        ## for users without items
        r[is.na(r)] <- 0

        ## sim(v,u) = 
        ##      sum_{for all i: r_i,v >0} r_i,u / freq(v) / freq(u)^p$alpha
        uv <-  crossprod(r, r>0)
        v <- matrix(colSums(r), nrow = n, ncol = n, byrow = FALSE)
        u <- t(v) 

        sim <- uv/v/u^p$alpha 

        ##  fix if freq = 0
        sim[is.na(sim)] <- 0
    }
    
    ## for other similarites: use 1 - dissimilarites defined in arules
    else sim <- 1 - as.matrix(dissimilarity(data, which = "items",
            method = p$method))
    
    ## normalize rows to 1
    if(p$normalize) sim <- sim/rowSums(sim)
    
    ## reduce similarity matrix to keep only the k highest similarities
    diag(sim) <- 0
    for(i in 1:nrow(sim)) {
        o <- order(sim[i,], decreasing=TRUE)
        o <- o[(p$k+1):length(o)]
        sim[i,o] <- 0
    }

    ## normalize rows to 1 after
    #if(normalize) s_items <- s_items/rowSums(s_items)
    
    ## make sparse
    sim <- as(sim, "dgCMatrix")

    model <- c(list(
            description = "IBCF: Reduced similarity matrix",
            sim = sim
        ), p
    )

    predict <- function(model, newdata, n = 10) {
        sim <- model$sim 
        U <- as(newdata, "dgCMatrix")

        ## add similarites for known items
        x <- crossprod(sim, U)
            
        ## remove known items
        x <- x*!U

        ## find N items with highest score
        reclist <- lapply(1:ncol(x), FUN = function(i) {
                u <- x[,i]
                ## kill 0's
                u[u==0] <- NA
                o <- head(order(u, decreasing=TRUE, na.last=NA), n)
            }
        )

        encode(reclist, itemLabels(newdata))
    }

    structure(list(method = "IBCF", n_train = length(data),
            model = model, predict = predict),
        class="recommender")
}

## register recommender
recommender_registry$set_entry(method="IBCF", fun=build_recom_IBCF, 
        description="Recommender based on item-based collaborative filtering.")

