\name{MovieLenseBin}
\alias{MovieLenseBin}
\docType{data}
\title{Binarized Version of the MovieLense Dataset}
\description{
This data set is a binarized version of the 100k MovieLense 
ratings data set. The data was collected through the MovieLens web site
(movielens.umn.edu) during the seven-month period from September 19th,
1997 through April 22nd, 1998.

The data set contains 100,000 ratings (1-5) 
from 943 users on 1682 movies. Ratings of 3+ are coded as 1 all other
ratings (including missing ratings) are codes as 0.
}
\usage{data(Movielense)}
\format{
  The format is:
Formal class 'itemMatrix' [package "arules"]
}
%\details{
%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
GroupLens Research, \url{http://www.grouplens.org/node/73}
}
\references{
Herlocker, J., Konstan, J., Borchers, A., Riedl, J.. An Algorithmic
Framework for Performing Collaborative Filtering. Proceedings of the
1999 Conference on Research and Development in Information
Retrieval. Aug. 1999.
}
\seealso{ 
    \code{\link[arules]{itemMatrix-class}}
}

\examples{
data(MovieLenseBin)
MovieLenseBin

summary(MovieLenseBin)
}
\keyword{datasets}
