\name{recommender_model}
\alias{recommender_model}
\alias{recommender_model.default}
\alias{recommender_model.recommender}
\alias{recommender_model.evaluation_results}
\title{Extract a Predictive Model}
\description{
Extract a predictive model from a recommender or evaluation results.
}
\usage{
recommender_model(x)
}
\arguments{
  \item{x}{ an object of class \code{recommender} or 
    \code{evaluation_results} (the result of \code{evaluate}
    with \code{keep_model = TRUE}).}
}
%\details{
%}
\value{
For an object of class \code{recommender} a single model is returned.
For an object of class \code{evaluation_results} a list of models is
returned, one for each run in the results.
}
%\references{ ~put references to the literature/web site here ~ }
\seealso{ 
    \code{\link{recommender}}, \code{\link{evaluate}} 
}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
