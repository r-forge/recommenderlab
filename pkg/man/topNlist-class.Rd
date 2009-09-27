\name{topNlist-class}
\Rdversion{1.1}
\docType{class}
\alias{topNlist-class}
\alias{bestN,topNlist-method}
\alias{coerce,topNlist,dgTMatrix-method}
\alias{coerce,topNlist,dgCMatrix-method}
\alias{coerce,topNlist,ngCMatrix-method}
\alias{coerce,topNlist,matrix-method}
\alias{coerce,topNlist,list-method}
\alias{colCounts,topNlist-method}
%\alias{LIST,topNlist-method}
\alias{rowCounts,topNlist-method}
\alias{show,topNlist-method}
%% generics
\alias{bestN}

\title{Class "topNlist": Top-N List}
\description{Recommendations a Top-N list. }
\section{Objects from the Class}{
Objects can be created by 
\code{predict} with a recommender model and a new data.
}
\section{Slots}{
	\describe{
		\item{\code{items}:}{Object of class \code{"list"}.
		Each represents a top-N recommendation (an integer vector) 
		with item IDs 
		(column numbers in the rating matrix). The items are ordered in
		each vector.}
		\item{\code{itemLabels}:}{Object of class \code{"character"}}
		\item{\code{n}:}{Object of class \code{"integer"} specifying the
		number of items in each recommendation. Note that the actual number
		on recommended items can be less depending on the data and the used
		algorithm.}
	}
}
\section{Methods}{
	\describe{
		\item{bestN}{\code{signature(x = "topNlist")}: returns only the best
		n recommendations (second argument is n which defaults to 10). }
		\item{coerce}{\code{signature(from = "topNlist", to = "dgTMatrix")}}
		\item{coerce}{\code{signature(from = "topNlist", to = "dgCMatrix")}}
		\item{coerce}{\code{signature(from = "topNlist", to = "ngCMatrix")}}
		\item{coerce}{\code{signature(from = "topNlist", to = "matrix")}}
		\item{coerce}{\code{signature(from = "topNlist", to = "list")}}
		\item{colCounts}{\code{signature(x = "topNlist")}: in how many top-N 
		does each item occur? }
%		\item{LIST}{\code{signature(from = "topNlist")}: ... }
		\item{rowCounts}{\code{signature(x = "topNlist")}: number of recommendations per user. }
		\item{show}{\code{signature(object = "topNlist")}}
	}
}
\seealso{
\code{\link{evaluate}}
}
%\examples{
%}
\keyword{classes}
