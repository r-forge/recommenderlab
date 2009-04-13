\name{recommender}
\alias{recommender}
\title{Create a Recommender Model}
\description{
Create a recommender for a given training data set. 
}
\usage{
recommender(data, method, parameter = NULL)
}
\arguments{
  \item{data}{ training data encoded as \code{transactions} or
    \code{itemMatrix} (see package \pkg{arules} for details)}
  \item{method}{ name of recommender method. }
  \item{parameter}{ parameter list for the recommender algorithm.}
}
\details{
FIXME: Registry and methods missing

The predictive model can be extracted from the recommender using 
\code{recommender_model}.
}
\value{
\code{recommender} returns an object of class \code{recommender} 
which contains a predictive model.
}
%\references{ ~put references to the literature/web site here ~ }
\seealso{ 
    \code{\link{predict.recommender}}, 
    \code{\link{recommender_model},
    \code{\link[arules]{itemMatrix-class}},
    \code{\link[arules]{transactions-class}}
    }
}
\examples{
    ## look at available recommender methods in registry
    recommender_registry$get_entry_names()
    recommender_registry$get_entry("IB")

    ## create an item-based collaborative filtering recommender for
    ## the movielense data set
    data("MovieLenseBin")
    ML <- MovieLenseBin[size(MovieLenseBin) >10]

    r <- recommender(ML, method = "IBCF", 
        parameter = list(method = "Jaccard"))

    r

    ## get the predictive model (for IBCF a reduced similarity matrix)
    str(recommender_model(r))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
