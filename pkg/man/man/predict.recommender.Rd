\name{predict.recommender}
\alias{predict.recommender}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Model Predictions for Recommenders}
\description{
Generate predictions for new data using a recommender prediction model.
}
\usage{
predict.recommender(object, newdata, n = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ a recommender prediction model of class \code{recommender} }
  \item{newdata}{ new data encoded as \code{transactions} or
    \code{itemMatrix} (see package \pkg{arules} for details)}
  \item{n}{ maximum number items in recommendation }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Recommendations encoded as an \code{itemMatrix}
}
\seealso{ \code{
    \link{recommender}},
    \code{\link[arules]{itemMatrix-class}},
    \code{\link[arules]{transactions-class}}
    }
\examples{
## example with custom data
data_list <- list(
        c("A","B", "D", "E"),
        c("A","C", "D"),
        c("A","B"),
        c("C", "D", "E"),
        c("A", "C"),
        c("D")
        )

## create data set
data_trans <- as(data_list, "transactions") ## see package arules for details

## data_trans could also be created with as(data_matrix, "transactions")
## from a matrix looking like that
data_matrix <- as(data_trans,"matrix")
data_matrix

## create recommender and generate recommendation
r <- recommender(data_trans[1:4], method = "UBCF")
recom <- predict(r, data_trans[5:6], n=2) 

## export recommendations
as(recom, "list")
as(recom, "matrix")
as(recom, "ngCMatrix")

## example with MovieLense data
data ("MovieLenseBin")

ML <- sample(MovieLenseBin[size(MovieLenseBin) >10], 505)

## create a recommender using 500 users
r <- recommender(ML[1:500], method = "POP")

## generate recommendations for 5 users
recom <- predict(r, ML, 5[501:505], n = 15) 
recom

## coerce recommendations to list or matrix represenation 
as(recom, "list")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
