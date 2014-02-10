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
%\usage{
%}
%- maybe also 'usage' for other objects documented here.
%\arguments{
%}
%\details{
%}
%\value{
%}
%\seealso{

%}
\arguments{
  \item{data}{training data.}
	\item{method}{a character string defining the recommender method to use
	(see details).}
	\item{parameter}{parameters for the recommender algorithm.}
	\item{\dots}{further arguments. }
}
\examples{
data("MovieLense")
MovieLense100 <- MovieLense[rowCounts(MovieLense) >100, 1:50]
a <- Recommender(MovieLense100, method = "PREA_PMF")

## create top-N recommendations for new users
pre <- predict(a, 1:10, type="topN")
pre
as(pre, "list")

## predict ratings for new users
pre <- predict(a, MovieLense[400:401], data=a, type="ratings")
pre
as(pre, "matrix")

}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
