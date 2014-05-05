.get_parameters <- function(p, parameter) {
  if(!is.null(parameter) && length(parameter) != 0) {
    o <- pmatch(names(parameter), names(p))
    
    if(any(is.na(o)))
      stop(sprintf(ngettext(length(is.na(o)),
                            "Unknown option: %s",
                            "Unknown options: %s"),
                   paste(names(parameter)[is.na(o)],
                         collapse = " ")))
    
    p[o] <- parameter
  }
  
  p
}


recommender_setup<- function(data, param, interface, strings) {
  
  #trip is a triplet (i, j, x) representation of a sparse matrix
  RatingsAsTripletMatrix <- (as(as(data,"dgCMatrix"), "dgTMatrix"))  #change name to something else STORE AS REALRATINGMATRIX
  colAndRowNames <- data@data@Dimnames
  
  rowlength <- RatingsAsTripletMatrix@Dim[1]
  columnlength <- RatingsAsTripletMatrix@Dim[2]
  param$range[1] = rowlength
  param$range[2] = columnlength
  
  recommenderJObject <- NULL
  model <- c(list(description = "PREA: PMF", preaObject = NULL, data=data, interface=interface), param)
  
  if(param$lazy == FALSE) {
    #calls interface.createRatingMatrix(rowlength, columnlength, i[], j[], x[]);
    #basically transfers the matrix to a Java reporesentation of a sparse matrix
    ratingMat <- .jcall(interface, returnSig = "LRecContainer;",
                        "createRatingMatrix", rowlength, columnlength,
                        RatingsAsTripletMatrix@i, RatingsAsTripletMatrix@j, RatingsAsTripletMatrix@x, silent=FALSE, check=TRUE)
    
    recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)
    
    model <- c(list(description = "PREA: PMF", preaObject = recommenderJObject, data=data, interface=interface), param)
    
  }
  
  
  return(model)
  
  
}


predict_help <- function(model, newdata, n, type, strings, interface) {
  
  r <- model$preaObject
  OriginalData <- model$data
  colAndRowNames <- OriginalData@data@Dimnames
  
  if(class(newdata) == "integer") {
    print("integer")
    if(model$lazy == TRUE) {
      RatingsAsTripletMatrix <- (as(as(OriginalData,"dgCMatrix"), "dgTMatrix"))
      rowlength <- RatingsAsTripletMatrix@Dim[1]
      columnlength <- RatingsAsTripletMatrix@Dim[2]
      ratingMat <- .jcall(interface, returnSig = "LRecContainer;", "createRatingMatrix", rowlength, columnlength,
                          RatingsAsTripletMatrix@i, RatingsAsTripletMatrix@j, RatingsAsTripletMatrix@x, silent=FALSE, check=TRUE)
      recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", ratingMat, strings)      
      r <- recommenderJObject
      
    }
    if(is.null(r)) print("r is null")
    else print("r is not null")
    predictedValues <- sapply(.jcall(interface, returnSig = "[[D", "runRecommender", r), .jevalArray, silent=FALSE)
    predictedValues <- as(predictedValues, "realRatingMatrix")
    predictedValues@data@Dimnames <- colAndRowNames
    
    if (type=="topNList") {
      ratings <- predictedValues[newdata,]
      return(getTopNLists(ratings, n))
      
    } else if (type == "ratings") {
      print("compiling ratings")
      return(predictedValues)
    }
    
  }
  
  else if(class(newdata) == "realRatingMatrix") {
    if(model$lazy == FALSE){ stop("To add new data when predicting, rebuild the recommender with param=c(lazy=TRUE)")}
    
    CombinedMatrix <- as(rbind2(as(newdata,"dgCMatrix"), as(OriginalData, "dgCMatrix")), "dgTMatrix")
    CombinedColAndRowNames <- newdata@data@Dimnames
    
    CombinedRowlength <- CombinedMatrix@Dim[1]
    CombinedColumnlength <- CombinedMatrix@Dim[2]
    class(CombinedRowlength)
    str(CombinedRowlength)
    
    
    #calls interface.createRatingMatrix(rowlength, columnlength, i[], j[], x[]);
    #basically transfers the matrix to a Java reporesentation of a sparse matrix
    JavaRatingMat <-.jcall(interface, returnSig = "LRecContainer;",
                           "createRatingMatrix", CombinedRowlength, CombinedColumnlength,
                           CombinedMatrix@i, CombinedMatrix@j, CombinedMatrix@x, silent=FALSE, check=TRUE)
    
    recommenderJObject <- .jcall(interface, returnSig = "LRecContainer;","createRecommender", JavaRatingMat, strings)
    #model <- c(list(description = "PREA: PMF", preaObject = recommenderJObject), param)
    r <- recommenderJObject
    
    #runs the recommendations
    predictedValues <- sapply(.jcall(interface, returnSig = "[[D", "runRecommender", r), .jevalArray, silent=FALSE)
    predictedValues <- as(predictedValues, "realRatingMatrix")
    predictedValues@data@Dimnames <- CombinedColAndRowNames
    
    if (type=="topNList") {
      ratings <- predictedValues[1:newdata@data@Dim[1]+1,]
      return(getTopNLists(ratings, n))
      
    } else if (type == "ratings") {
      print("compiling ratings")
      return(predictedValues[1:newdata@data@Dim[1]+1,])
    }
    
    
  }
  
  
  
}