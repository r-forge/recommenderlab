\name{normalize}
\alias{normalize}
\title{Normalize the ratings}
\description{
Provides the generic for normalize and 
a method to normalize the ratings in a realRatingMatrix.}
\usage{
normalize(x, ...)
%% \S4method{normalize}{realRatingMatrix}(x, row=TRUE, ...)
}
\arguments{
  \item{x}{a realRatingMatrix.}
%% \item{row}{should the rows be normalized or the columns?}
  \item{\dots}{unused.}
}
\details{
Normalization tries to reduce the individual rating bias by subtracting 
from each available rating the mean of the ratings of that user (row). 
}
\value{
A normalized realRatingMatrix.
}
%% \seealso{}
\examples{
data(MovieLense)

## original ratings
LIST(MovieLense[1:2,1:5])

## normalized ratings
LIST(normalize(MovieLense)[1:2,1:5])

}
\keyword{manipulation}
