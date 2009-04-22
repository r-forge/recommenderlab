\name{evaluation_list}
\alias{evaluation_list}
\alias{print.evaluation_list}
\alias{plot.evaluation_list}
\alias{mean.evaluation_list}
\alias{[.evaluation_list}
\title{List of Evaluation Results}
\description{
A list of evaluation results of different recommender algorithms of
for different parameter settings. 
}
\usage{
evaluation_list(x)
mean.evaluation_list(x)
plot.evaluation_list(x, plot_type=c("ROC", "prec/rec"),
    xlim = NULL, ylim = NULL, annodate = 0, legend = "bottomright")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{evaluation_list}. For the 
  \code{evaluation_list} creator, a list of \code{evaluation_results}.}
  \item{plot_type} {type of plot}
  \item{xlim, ylim} {limits for the x and y-axes (see \code{plot.default}).
  For \code{NULL} the optimal limits are calculated.}
  \item{annodate} {vector with indices of the results that should 
  have N annodated}
  \item{legend} { position of the legend (see \code{legend}).}
}
%\details{
%}
\value{
\code{evaluation_list} and \code{mean} return an
object of class \code{evaluation_list}.
}
%\references{
%}

\seealso{ \code{\link{evaluation_results}}}
\examples{
## take a very small sample of the MovieLense dataset
data ("MovieLenseBin")
ML <- sample(MovieLenseBin[size(MovieLenseBin) >10], 100)

## create a 4-fold cross-evaluation scheme
es <- evaluation_scheme(ML, "cross", k=4, given=3)
es

## define algorihms (algorith name, parameters)
algorithms <- list(
        RANDOM = list(name = "RANDOM", para = NULL),
        POPULAR = list(name = "POPULAR", param = NULL),
        IBCF = list(name = "IBCF", 
            param = list(method="jaccard", k=25, normalize=TRUE))
        )

Ns <- c(1,5,10)

## run algorithms
res <- lapply(algorithms, FUN = function(a) evaluate(es, a[["name"]], N=Ns,
            parameter=a[["param"]]))

## make list a evaluation list object
res <- evaluation_list(res)

## ROC plot
plot(res, annodate=1:3, legend="topright", pch=3, lty=1:3)

## precision/recall plot of first two algorithms
plot(res[1:2], plot_type="prec/rec")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ }
