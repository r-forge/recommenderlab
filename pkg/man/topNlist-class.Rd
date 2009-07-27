\name{topNlist-class}
\Rdversion{1.1}
\docType{class}
\alias{topNlist-class}
\alias{show,topNlist-method}
\alias{topNlistFromList}

\title{Class "topNlist": Top-N List}
\description{Recommendations a Top-N list. }
\section{Objects from the Class}{
Objects can be created by 
\code{predict} with a recommender model and a new data.
}
\section{Slots}{
	 \describe{
		 \item{\code{i}:}{Object of class \code{"integer"}}
		 \item{\code{p}:}{Object of class \code{"integer"}}
		 \item{\code{Dim}:}{Object of class \code{"integer"}}
		 \item{\code{Dimnames}:}{Object of class \code{"list"}}
		 \item{\code{x}:}{Object of class \code{"numeric"}}
		 \item{\code{factors}:}{Object of class \code{"list"}}
  }
}
\section{Extends}{
	Class \code{"\linkS4class{ratingMatrix}"}, directly.
}
\section{Methods}{
No methods defined with class "topNlist" in the signature.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\note{
	All methods defined for "ratingMatrix" can be used.
	
	\code{topNlistFromList(from, itemLabels, userLabels = NULL) can be used
to create a top-N list from a list with item IDs (column numbers).}}
\seealso{
%	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
	\code{\linkS4class{ratingMatrix}}
}
\examples{
data("MovieLense")
MovieLense10 <- MovieLense[rowCounts(MovieLense) >10,]
rec <- Recommender(MovieLense10[1:500], method = "POPULAR")
pre <- predict(rec, MovieLense10[501:502])
pre

getList(pre, decode=FALSE)
getList(pre, decode=TRUE)
rowSums(pre)

as(pre, "ngCMatrix")
}
\keyword{classes}
