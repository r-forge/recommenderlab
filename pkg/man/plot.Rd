\name{plot}
\Rdversion{1.1}
\alias{plot,evaluationResults-method}
\alias{plot,evaluationResultList-method}
\title{
Plot Evaluation Results
}
\description{
Creats precision-recall or ROC plots for recommender evaluation results.
}
\usage{
\S4method{plot}{evaluationResults}(x, y=c("ROC", "prec/rec"),
        avg = TRUE, add=FALSE, type= "l", annodate = FALSE, ...)
\S4method{plot}{evaluationResultList}(x, y=c("ROC", "prec/rec"),
        xlim=NULL, ylim=NULL, col = NULL, pch = 1, lty = 1,
		    annodate= 0, legend="bottomright", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ the object to be plotted.}
  \item{y}{a character string indicating the type of plot.}
  \item{avg}{ plot average of runs?}
  \item{add}{ add to a plot?}
  \item{type}{ line type (see \code{plot}).}
  \item{annodate}{ annodate N (recommendation list size) to plot. }
  \item{xlim,ylim}{plot limits (see \code{plot}).}
  \item{col}{ colors (see \code{plot}).}
  \item{pch}{ point symbol to use (see \code{plot}).}
  \item{lty}{ line type (see \code{plot})}
  \item{legend}{ where to place legend (see \code{legend}).}
  \item{...}{ further arguments passed on to \code{plot}.}
}
%\value{
%}
\seealso{
\code{\linkS4class{evaluationResults}},
\code{\linkS4class{evaluationResultList}}.
See 
\code{\link{evaluate}} for examples.
}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
