library(recommenderlabPREA)

#PMF

data("MovieLense")
MovieLense100 <- MovieLense[rowCounts(MovieLense) >100, 1:50]
rec <- Recommender(MovieLense100, method = "PREA_PMF")
if(class(rec) != "Recommender") stop("create PMF recommender failed")

## create top-N recommendations for users 1 to 10
pre <- predict(rec, 1:10, type="topN")
if(class(pre) != "topNList") stop("PMF predict topNList failed")

## predict ratings for users 1 to 10
pre <- predict(rec, 1:10, type="ratings")
if(class(pre) != "realRatingMatrix") stop("PMF predict ratings failed")

#SLOPEONE

rec <- Recommender(MovieLense100, method = "PREA_SLOPEONE")
if(class(rec) != "Recommender") stop("create SLOPEONE recommender failed")

## create top-N recommendations for users 1 to 10
pre <- predict(rec, 1:10, type="topN")
if(class(pre) != "topNList") stop("SLOPEONE predict topNList failed")

## predict ratings for users 1 to 10
pre <- predict(rec, 1:10, type="ratings")
if(class(pre) != "realRatingMatrix") stop("SLOPEONE predict ratings failed")

#USERAVG

rec <- Recommender(MovieLense100, method = "PREA_USERAVG")
if(class(rec) != "Recommender") stop("create USERAVG recommender failed")

## create top-N recommendations for users 1 to 10
pre <- predict(rec, 1:10, type="topN")
if(class(pre) != "topNList") stop("USERAVG predict topNList failed")

## predict ratings for users 1 to 10
pre <- predict(rec, 1:10, type="ratings")
if(class(pre) != "realRatingMatrix") stop("USERAVG predict ratings failed")

"REGSVD"

rec <- Recommender(MovieLense100, method = "PREA_REGSVD")
if(class(rec) != "Recommender") stop("create REGSVD recommender failed")

## create top-N recommendations for users 1 to 10
pre <- predict(rec, 1:10, type="topN")
if(class(pre) != "topNList") stop("REGSVD predict topNList failed")

## predict ratings for users 1 to 10
pre <- predict(rec, 1:10, type="ratings")
if(class(pre) != "realRatingMatrix") stop("REGSVD predict ratings failed")

"NPCA"

rec <- Recommender(MovieLense100, method = "PREA_NPCA")
if(class(rec) != "Recommender") stop("create NPCA recommender failed")

## create top-N recommendations for users 1 to 10
pre <- predict(rec, 1:10, type="topN")
if(class(pre) != "topNList") stop("NPCA predict topNList failed")

## predict ratings for users 1 to 10
pre <- predict(rec, 1:10, type="ratings")
if(class(pre) != "realRatingMatrix") stop("NPCA predict ratings failed")





