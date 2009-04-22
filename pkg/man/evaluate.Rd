\name{evaluate}
\alias{evaluate}
\alias{evaluation_results}
\alias{print.evaluation_results}
\alias{mean.evaluation_results}
\alias{plot.evaluation_results}
\title{Recommender Evaluation}
\description{
Applies an evaluation scheme to a recommender method. 
The result consists of confusion matrices.
}
\usage{
evaluate(scheme, method, n = 1:10, parameter = NULL, progress = TRUE, 
    keep_model = FALSE)
mean.evaluation_results(x)
plot.evaluation_results(x, plot_type=c("ROC", "prec/rec"),
    add=FALSE type="l", annodate=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scheme}{ an evaluation validation scheme created by 
    \code{cross_validation_scheme}. }
  \item{method}{ name of recommender method (see \code{recommender}).}
  \item{n}{ range for the number of recommended items (see \code{recommender}).}
  \item{parameter}{ parameters for the recommender algorithm 
  (see \code{recommender}) }
  \item{progress}{report progress.}
  \item{keep_model}{keep the used recommender models and 
    append them to the output.
    Note, this might use a significant amount of memory.}
  \item{x}{an object of class \code{evaluation_results}}
  \item{plot_type}{type of plot}
  \item{type}{type for plot (in package \pkg{graphics}). E.g, "p" for
  points, "l" for line and "o" for overplotted.}
  \item{add}{add lines to an existing plot?}
  \item{annodate}{annodate plot with N values.}
  \item{...}{further arguments passed on to plot (in package \pkg{graphics})}
}
\details{
%\code{cross_validate} uses a cross-validation scheme to evaluate a recommender
%method. For each fold i, all but the ith data set are used for creating
%the recommender. Then, given the known items for fold i, recommendations
%are generated and compared to the unknown items. Averaged confusion matrices
%are returned.

\code{mean} calculates the average for all evaluation runs stored in 
\code{x} (e.g., all k runs of k-fold evaluation).

\code{plot}  is a convenience function to generate ROC or precision/recall
plots from a object of class \code{evaluation_results}. For
more visualization the package \pkg{ROCR} can be used (for details see 
function \code{evaluation_scheme}).
}
\value{
  \code{evaluation} returns an object of class 
  \code{evaluation_results} which is a list of matrices. Each list
  element corresponds to one run (e.g., one fold of k-fold cross-validation) 
  and contains a matrix. Each matrix
  contains values from the confusion matrix in the first four columns
  and some derived measures in the remaining columns. Rows correspond
  to different values for n.
}
\references{
John S. Breese, David Heckerman, and Carl Kadie (1998). Empirical analysis of
predictive algorithms for collaborative filtering. In Uncertainty in Artificial
Intelligence. Proceedings of the Fourteenth Conference, pp. 43-52.

Kohavi, Ron (1995). "A study of cross-validation and bootstrap for accuracy
estimation and model selection". Proceedings of  the Fourteenth International
Joint Conference on Artificial Intelligence, pp. 1137–1143. 

Fawcett, T. (2006). An introduction to ROC analysis. Pattern Recognition Letters, 27, 861-874.
}
\seealso{\code{\link{evaluation_scheme}}, 
        \code{\link{ROCR_prediction}} 
        }
\examples{
data ("MovieLenseBin")
ML <- sample(MovieLenseBin[size(MovieLenseBin) >10], 200)

## create a 4-fold cross-validation scheme with 3 items given
es <- evaluation_scheme(ML, method="cross", k=4, given=3)

## vary N
confusion <- evaluate(es, "POP", n=c(1, 5, 10, 20, 50), keep_model = TRUE)
confusion

## look at the predictive models used for evaluation
str(recommender_model(confusion))

## calculate averages
mean(confusion)

## visualize results with convenience function
plot(confusion)
plot(confusion, plot_type="prec/rec", type="o", annodate=TRUE)

## direct visualization of mean and all runs
plot(mean(confusion)[, c("recall", "precision")], 
    type="l", col="red", lwd=2)
tmp <- lapply(confusion, FUN=function(x) 
    lines(x[, c("recall", "precision")]))

## for more visualization options see ROCR_prediction
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ }
