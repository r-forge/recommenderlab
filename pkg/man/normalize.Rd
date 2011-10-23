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
\S4method{normalize}{realRatingMatrix}(x, method="center", row=TRUE)
denormalize(x, ...)
\S4method{denormalize}{realRatingMatrix}(x, method=NULL, row=NULL, 
    factors=NULL)
}
\arguments{
  \item{x}{a realRatingMatrix.}
  \item{method}{normalization method. Currently "center" or "Z-score".}
  \item{row}{should the rows be normalized or the columns?}
  \item{factors}{a list with the factors used for normalizing 
  (elements are "mean" and "sds").}
  \item{...}{further arguments (currently unused).}
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
## create a matrix with ratings
    m <- matrix(sample(c(NA,0:5),100, replace=TRUE, prob=c(.7,rep(.3/6,6))),
	    nrow=10, ncol=10, dimnames = list(
		users=paste('u', 1:10, sep=''),
		items=paste('u', 1:10, sep='')
		))

## do normalization
r <- as(m, "realRatingMatrix")
r_n1 <- normalize(r) 
r_n2 <- normalize(r, method="Z-score")

r
r_n1
r_n2

## show normalized data
image(r, main="Raw Data")
image(r_n1, main="Normalized: Centered")
image(r_n2, main="Normalized: Z-Score")

## check if denormalization works
identical(r, denormalize(r_n1))
identical(r, denormalize(r_n2))
}
\keyword{manipulation}
