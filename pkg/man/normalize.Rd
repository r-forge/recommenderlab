\name{normalize}
\alias{normalize}
\alias{denormalize}
\alias{denormalize,realRatingMatrix-method}
\alias{normalize,realRatingMatrix-method}
\title{Normalize the ratings}
\description{
Provides the generic for normalize/denormalize and 
a method to normalize/denormalize the ratings in a realRatingMatrix.}
\usage{
normalize(x, ...)
\S4method{normalize}{realRatingMatrix}(x, method="center", row=TRUE,)
denormalize(x, ...)
\S4method{normalize}{realRatingMatrix}(x, method=NULL, row=NULL, 
factors=NULL, ...)
}
\arguments{
  \item{x}{a realRatingMatrix.}
  \item{method}{normalization method. Currently "center" or "Z-score".}
  \item{row}{should the rows be normalized or the columns?}
  \item{factors}{a list with the factors used for normalizing 
  (elements are "mean" and "sds").}
}
\details{
Normalization tries to reduce the individual rating bias by subtracting 
from each available rating the mean of the ratings of that user (row). 
Normalization can also be done on columns.
Z-score in addition divides by the standard deviation of the row/column.

Denormalization reverses normalization. It uses the normalization
information stored in x unless the user specifies method, row, etc.
}
\value{
A normalized realRatingMatrix.
}
%% \seealso{}
\examples{
data(Jester5k)

db <- Jester5k[1:5,1:5]
## original ratings
as(db, "matrix")


## normalized ratings
as(normalize(db), "matrix")
as(normalize(db, method="Z-score"), "matrix")

}
\keyword{manipulation}
