\name{recommenderlabPREA}
\Rdversion{1.1}
\alias{recommenderlabPREA}
\alias{PREA}
\alias{Recommender}
\alias{predict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recommenderlab Interface to PREA
}
\description{
Provides an interface to recommender algorithms implemented in PREA.
}
\usage{
Recommender(data, method="PREA_...")
## S4 method for signature 'ratingMatrix'
Recommender(data, method, parameter=NULL)
Recommender(data, method, parameter = c(...))
}
\arguments{
  \item{data}{rating matrix}
	\item{method}{a character string defining the method to use. (see details)}
	\item{parameter}{parameters for the recommender algorithm in a list}
}
\details{
  RecommenderlabPREA is an extension of the package 'recommenderlab' that provides an interface to the recommender algorithms implemented in the java package, PREA
}
%\value{
%}
\seealso{
Recommender, ratingMatrix
http://prea.gatech.edu/documentation.html
}

\examples{
data("MovieLense")
MovieLense100 <- MovieLense[rowCounts(MovieLense) >100, 1:50]
rec <- Recommender(MovieLense100, method = "PREA_PMF")

## create top-N recommendations for users 1 to 10
pre <- predict(rec, 1:10, type="topN")
pre

## predict ratings for users 1 to 10
pre <- predict(rec, 1:10, type="ratings")
pre

}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
