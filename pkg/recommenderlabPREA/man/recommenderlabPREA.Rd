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
MovieLense100 <- MovieLense[rowCounts(MovieLense) >100,1:100]
rec <- Recommender(MovieLense100, method = "PREA_PMF")
rec <- Recommender(MovieLense100, method = "PREA_USERBASED")
rec
  
## create top-N recommendations for users 1 to 10
##FIXME
pre <- predict(rec, 1:10, type="topNList")
pre
as(pre, "list")

## predict ratings for users 1 to 10
pre <- predict(rec, 1:10, type="ratings")
as(pre,"matrix")[1:5,1:5]

## list all available recommender algorithms with prefix PREA_
grep("^PREA_", recommenderRegistry$get_entry_names(), value=TRUE)
  
## find avaialble parameters for PREA method PMF 
recommenderRegistry$get_entry("PREA_PMF")

## create recommender with you custom parameters
rec <- Recommender(MovieLense100, method = "PREA_PMF", 
  param=list(feature_count = 150, learning_rate=45))

## create recommender with lazy prediction (use new data in predict)
train <- MovieLense100[1:100]
test <- MovieLense100[101:105]
rec <- Recommender(data=train, method = "PREA_PMF", parameter=list(lazy=TRUE))
pre <- predict(rec, test, type="topNList")
pre

#use an evaluation scheme 
MovieLense10 <- sample(MovieLense[rowCounts(MovieLense) >10,], 50)

## create an evaluation scheme
es <- evaluationScheme(MovieLense10, method="cross-validation",
                       k=4, given=3, goodRating=3)

## run evaluation
ev <- evaluate(es, "PREA_PMF", type="topNList", parameter=list(lazy=TRUE))
ev
avg(ev)
plot(ev, annotate=TRUE)

### Evaluate using a rating data set and predicted ratings
data("Jester5k")
es <- evaluationScheme(Jester5k[1:100], method="cross-validation",
                       k=4, given=10, goodRating=5)

ev <- evaluate(es, "PREA_PMF", type="ratings", param=list(lazy=TRUE))
avg(ev)
plot(ev)

ev <- evaluate(es, "PREA_PMF", type="topNList", param=list(lazy=TRUE))
avg(ev)
plot(ev, annotate=TRUE)
  
## now run evaluate with a list
algorithms <- list(
  RANDOM = list(name = "RANDOM", param = NULL),
  PMF = list(name = "PREA_PMF", param = list(lazy=TRUE)),
  NMF = list(name = "PREA_NMF", param = list(lazy= TRUE))
)

evlist <- evaluate(es, algorithms, type="topNList")
avg(evlist)
plot(evlist)

evlist <- evaluate(es, algorithms, type="ratings")
avg(evlist)
plot(evlist)
}
%\keyword{ ~kwd1 }
