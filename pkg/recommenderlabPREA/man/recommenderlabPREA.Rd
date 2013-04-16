\name{recommenderlabPREA}
\Rdversion{1.1}
\alias{recommenderlabPREA}
\alias{PREA}
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
\examples{
data("MovieLense")
MovieLense100 <- MovieLense[rowCounts(MovieLense) >100,1:50]
train <- MovieLense100[1:100]

rec <- Recommender(train, method = "PREA")
rec

## create top-N recommendations for new users
pre <- predict(rec, MovieLense100[101:102], n = 10)
pre
as(pre, "list")

## predict ratings for new users
pre <- predict(rec, MovieLense100[101:102], type="ratings")
pre
as(pre, "matrix")


## create recommendations using user ids with ids 1..10 in the
## training data
pre <- predict(rec, 1:10 , data = train, n = 10)
pre
as(pre, "list")
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
