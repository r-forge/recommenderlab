\name{recommenderlabPREA}
\Rdversion{1.1}
\alias{recommenderlabPREA}
\alias{PREA}
\alias{Recommender}
\alias{predict}
\title{
Recommenders from PREA (Personalized Recommendation Algorithms Toolkit)
}
\description{
Provides an interface to recommender algorithms implemented in PREA v1.1
}
\usage{
Recommender(data, method, parameter=NULL)
}
\arguments{
  \item{data}{rating matrix}
	\item{method}{a character string defining the method to use. 
    PREA metods start with the prefix \code{PREA_} (see details).}
	\item{parameter}{parameters for the recommender algorithm in a list}
}
\details{
  RecommenderlabPREA is an extension of the package 'recommenderlab' that provides an interface to the recommender algorithms implemented in the java package, PREA v1.1
}
%\value{
%}
\seealso{
\code{\link[pkg:recommenderlab]{Recommender}},
\code{\linkS4class{ratingMatrix}}.
}
\references{
PREA Documentation \url{http://prea.gatech.edu/documentation.html}

Joonseok Lee, Mingxuan Sun, Guy Lebanon. PREA: Personalized Recommendation Algorithms Toolkit, Journal of Machine Learning Research, 13(Sep):2699-2703, 2012. \url{http://jmlr.org/papers/v13/lee12b.html}
}
\examples{
data("MovieLense")
MovieLense100 <- MovieLense[rowCounts(MovieLense) >100, 1:50]
rec <- Recommender(MovieLense100, method = "PREA_PMF")
rec
  
## create top-N recommendations for users 1 to 10
pre <- predict(rec, 1:10, type="topN")
pre
as(pre, "list")

## predict ratings for users 1 to 10
pre <- predict(rec, 1:10, type="ratings")
as(pre,"matrix")[1:5,1:5]

## list all available recommender algorithms with prefix PREA_
grep("^PREA_", recommenderRegistry$get_entry_names(), value=TRUE)
  
## find avaialble parameters for PREA method PMF 
recommenderRegistry$get_entry("PREA_PMF")

## create recommender with own parameters
rec <- Recommender(MovieLense100, method = "PREA_PMF", 
  param=list(feature_count = 150, learning_rate=45))

## create recommender with lazy prediction (use new data in predict)
train <- MovieLense100[1:100]
test <- MovieLense100[101:105]
rec <- Recommender(data=train, method = "PREA_PMF", param=list(lazy=TRUE))
pre <- predict(rec, test, type="topNList")
pre
}
%\keyword{ ~kwd1 }
