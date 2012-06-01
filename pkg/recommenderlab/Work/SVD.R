### try Jester
library(recommenderlab)
data(Jester5k)

d<-Jester5k
es <- evaluationScheme(d, method="cross-validation", goodRating=5,
	        k=4, given=20)


algorithms <- list(
	RANDOM = list(name = "RANDOM", param = NULL),
	#	POPULAR = list(name = "POPULAR", param = NULL),
	#UBCF = list(name = "UBCF", param = NULL),
	#IBCF = list(name = "IBCF", param = NULL),
	#PCA = list(name = "PCA", param = NULL),
		SVD_0 = list(name = "SVD", param = list(treat_na="0"))
	)

evlist <- evaluate(es, algorithms)
plot(evlist, legend="topright")

plot(evlist, "prec", legend="topright")


### try MovieLense
data(MovieLense)
d<-MovieLense
es <- evaluationScheme(d, method="cross-validation", goodRating=4,
	        k=4, given=10)


algorithms <- list(
	RANDOM = list(name = "RANDOM", param = NULL),
	POPULAR = list(name = "POPULAR", param = NULL),
	UBCF = list(name = "UBCF", param = NULL),
	IBCF = list(name = "IBCF", param = NULL),
	PCA = list(name = "PCA", param = NULL),
	SVD = list(name = "SVD", param = NULL)
	)

evlist <- evaluate(es, algorithms)
plot(evlist, legend="topright")

plot(evlist, "prec", legend="topright")

