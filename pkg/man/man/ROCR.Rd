\name{ROCR_prediction}
\alias{ROCR_prediction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Interface to Package ROCR}
\description{
Creates a \pkg{ROCR} \code{prediciton} object from an \code{evaluation_results}
object.
}
\usage{
ROCR_prediction(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{evaluation_results} as created by
  \code{evaluate}. }
}
\details{
Needs suggested package \pkg{ROCR} installed.
}
\value{
A \pkg{ROCR} \code{prediciton} object.
}
\seealso{ \code{\link{predict.recommender}}}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
