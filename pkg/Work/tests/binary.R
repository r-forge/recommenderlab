library(recommenderlab)

data(MSWeb)
MSWeb_s <- MSWeb[rowCounts(MSWeb)>10,]
MSWeb_s

methods <- list(
	RANDOM = list(name = "RANDOM", param = NULL),
	POPULAR = list(name = "POPULAR", param = NULL),
	UBCF = list(name = "UBCF", param = NULL),
	IBCF = list(name = "IBCF", param=NULL)
#	AR = list(name = "AR", param=NULL)
	)

# given = 1
scheme <- evaluationScheme(MSWeb_s, method='cross',k=2, given=1)
results <- evaluate(scheme, method=methods, n=c(1,3,5,10))
plot(results)

# given = 10
scheme <- evaluationScheme(MSWeb_s, method='cross',k=2, given=10)
results <- evaluate(scheme, method=methods, n=c(1,3,5,10))
plot(results)


r <- Recommender(MSWeb_s, "IBCF")
model <- r@model
n <- 10L
newdata <- MSWeb[1,]




