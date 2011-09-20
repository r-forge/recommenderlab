\name{calcPredictionError}
\Rdversion{1.1}
\alias{calcPredictionError}
\alias{calcPredictionError,realRatingMatrix,realRatingMatrix-method}
\title{
  Calculate the Prediction Error for a Recommendation
}
\description{
  Calculates MAD, MSE and RMSE for predicted ratings given the actual 
  ratings by the user.
}
\usage{
calcPredictionError(x, data, ...)
\S4method{calcPredictionError}{realRatingMatrix,realRatingMatrix}(x, data, byUser=TRUE)
}
\arguments{
  \item{x}{ Predicted ratings as a "realRatingMatrix"}
  \item{data}{ Actual ratings by the user as a "realRatingMatrix" }
  \item{byUser}{ logical; Should the errors be averaged by user or over all
  recommendations?}
  \item{...}{ further arguments.}
}
%\details{
%}
\value{
Returns a matrix with columns for the different error measures. For 
\code{byUser=TRUE} each user is a row in the matrix, otherwise the matrix
has only a single row with the global averages.
}
\seealso{
\code{\linkS4class{binaryRatingMatrix}}.
}
\examples{
data(MovieLense)

## create 90/10 split for the data
e <- evaluationScheme(MovieLense[1:500], method="split", train=0.9, 
    k=1, given=15)
e

## create a user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF")

## create predictions for the test data using known ratings (see given above)
p <- predict(r, getData(e, "known"), type="ratings")
p

## compute error metrics averaged per user and then averaged over all 
## recommendations
calcPredictionError(p, getData(e, "unknown"))
calcPredictionError(p, getData(e, "unknown"), byUser=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
