\name{evaluation_scheme}
\alias{evaluation_scheme}
\alias{print.evaluation_scheme}
\alias{evaluation_data}
\title{Create an Evaluation Scheme}
\description{
Creates from a data set an evaluation scheme. The scheme can be a simple split into training and test data, k-fold cross-evaluation or using k 
bootstrap samples.
}
\usage{
evaluation_scheme(data, method="split", train=0.9, k=10, given=3)
evaluation_data(scheme, type = c("train", "known", "unknown"), run=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ the data set encoded as \code{transactions} or 
  \code{itemMatrix}.}
  \item{method}{ "split", "cross-validation" or "bootstrap" 
  for simple split into 
  training and test set, k-fold cross-validation or k bootstrap samples.}
  \item{train}{ portion of data used for training for "split" and
  "bootstrap". }
  \item{k}{ number of runs for validation. }
  \item{given}{ single number of items given for evaluation or
  a vector of length of data giving the number of items given for each 
  observation. }
  \item{scheme} {an object of class \code{evaluation_scheme}}
  \item{type} {type of data requested}
  \item{run} { for which run is data requested?}
}
\details{
%randomly splits the data into k equally sized data sets (folds). For evaluation
%from each object are chosen known items (the number is defined by \code{given})
%and all other items are unknown. 
\code{evaluation_scheme} creates an evaluation scheme with \code{k} runs
following the given method:

\code{"split"} randomly assigns
the proportion of objects given by code{train} to the training set and
the rest is used for the test set.

\code{"cross-validation"} creates a k-fold cross-validation scheme. The data
is randomly split into k parts and in each run k-1 parts are used for
training and the remaining part is used for testing. After all runs each
part was used as test set once.

\code{"bootstrap"} creates the training set by taking a bootstrap sample 
(sampling with replacement) of size \code{train} times size of the data set.
All objects not in the training set are used for testing.

\code{evaluation_data} allows for simple access to training and test data
stored in an evaluation scheme.
}
\value{
Returns an
object of class \code{evaluation_scheme}.
}
\references{
Kohavi, Ron (1995). "A study of cross-validation and bootstrap for accuracy
estimation and model selection". Proceedings of  the Fourteenth International
Joint Conference on Artificial Intelligence, pp. 1137–1143. 
}

\seealso{ \code{\link{evaluate}}, 
    \code{\link[arules]{itemMatrix-class}},
    \code{\link[arules]{transactions-class}}
    }
\examples{
data ("MovieLenseBin")
ML <- sample(MovieLenseBin[size(MovieLenseBin) >10], 200)

## create a 4-fold cross-evaluation scheme given 5 items
es <- evaluation_scheme(ML, "cross", k=4, given=5)
es

## retrieve data used for run 2 of the evaluation
evaluation_data(es, "train", run=2)
evaluation_data(es, "known", run=2)
evaluation_data(es, "unknown", run=2)

## create 10 bootstrap samples with an all-but one scheme
es <- evaluation_scheme(ML, "boot", k=10, given=size(ML)-1)
es

## now we only need to predict one item given all other items for
## each trest user
table(size(evaluation_data(es, "unknown")))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ }
