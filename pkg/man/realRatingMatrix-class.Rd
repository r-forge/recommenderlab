\name{realRatingMatrix-class}
\Rdversion{1.1}
\docType{class}
\alias{realRatingMatrix-class}
\alias{coerce,matrix,realRatingMatrix-method}
\alias{coerce,realRatingMatrix,matrix-method}
\alias{coerce,realRatingMatrix,dgTMatrix-method}
\alias{coerce,realRatingMatrix,ngCMatrix-method}
\alias{coerce,realRatingMatrix,dgCMatrix-method}
\alias{coerce,data.frame,realRatingMatrix-method}
\alias{rowSds}
\alias{colSds}
\alias{rowSds,realRatingMatrix-method}
\alias{colSds,realRatingMatrix-method}
\alias{coerce,data.frame,realRatingMatrix-method}
%\alias{LIST,realRatingMatrix-method}
\alias{binarize,realRatingMatrix-method}
\alias{binarize}
\alias{getTopNLists,realRatingMatrix-method}
\alias{getTopNLists}

\title{Class "realRatingMatrix": Real-valued Rating Matrix}
\description{A matrix containing ratings (typically 1-5 stars, etc.). 
Note that 0 codes for a missing rating.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("realRatingMatrix", data = m)}, where \code{m} is sparse matrix of class \code{dgCMatrix} 
(see package~\pkg{Matrix}), or by coercion from a regular matrix.
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"dgCMatrix"}, a sparse matrix
	defined in package \pkg{Matrix}.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{ratingMatrix}"}, directly.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "matrix", to = "realRatingMatrix")}}
    \item{coerce}{\code{signature(from = "realRatingMatrix", to = "matrix")}}
    \item{coerce}{\code{signature(from = "realRatingMatrix", to = "dgTMatrix")}}
    \item{coerce}{\code{signature(from = "realRatingMatrix", to = "ngCMatrix")}}
    \item{coerce}{\code{signature(from = "realRatingMatrix", to = "dgCMatrix")}}
    \item{coerce}{\code{signature(from = "data.frame", to = "realRatingMatrix")}}
    \item{hist}{\code{signature(x = "realRatingMatrix")}: create a histogram
	for the ratings in \code{x}. }
    \item{binarize}{\code{signature(x = "realRatingMatrix")}: create a 
        \code{"binaryRatingMatrix"} by setting all ratings larger or equal to 
        the argument \code{threshold} (default: 3) as 1 and all others to 0.}
    \item{getTopNLists}{\code{signature(x = "realRatingMatrix")}: create 
        top-N lists from the ratings in x. Arguments are \code{n} 
	and \code{threshold}. Items with a rating below \code{min_rating} will 
	not be part of the top-N list.}
    \item{rowSds}{\code{signature(x = "realRatingMatrix")}: calculate 
	the standard deviation of ratings for rows (users).}
    \item{colSds}{\code{signature(x = "realRatingMatrix")}: calculate 
	the standard deviation of ratings for colums (items).}
	 }
}
\seealso{
	\code{\link[Matrix:dgCMatrix-class]{dgCMatrix}} in \pkg{Matrix}
	%\code{\linkS4class[Matrix]{dgCMatrix}}
}
\examples{
## create a matrix with ratings
m <- matrix(sample(c(NA,1:5),100, replace=TRUE), nrow=10, ncol=10)
m

## coerce into a realRatingMAtrix
r <- as(m, "realRatingMatrix")
r

## coerce it back to see if it worked
as(r, "matrix")

## binarize into a binaryRatingMatrix with all 4+ rating a 1
b <- binarize(r, threshold=4)
b
}
\keyword{classes}
