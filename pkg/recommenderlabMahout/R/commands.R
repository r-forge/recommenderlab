
library(recommenderlabMahout)
data(MovieLense)

### use write.table
write.csv(file = "data.csv",  as(MovieLense[1:20,], "data.frame"))



testSlopeOne <- function(file = "data.csv") {

data <- .jnew("java/lang/String", file)

#recsystreeclustering <- .jnew("com/movieRecommender/test")
#result = .jcall(recsystreeclustering,"[I","createRecommender","treeClusteringRecommender",test, as.integer(6012))
#
user <- 6012

obj <- .jnew("com/movieRecommender/test")
result = .jcall(obj,"[I","createRecommender","slopeOneRecommender",data, as.integer(user))
result

#
#recsysrandom <- .jnew("com/movieRecommender/test")
#result = .jcall(recsysrandom,"[I","createRecommender","randomRecommender",test, as.integer(6012))
#
#recsyssvd <- .jnew("com/movieRecommender/test")
#result = .jcall(recsyssvd,"[I","createRecommender","svdRecommender",test, as.integer(6012))
#
#recsysknn <- .jnew("com/movieRecommender/test")
#result = .jcall(recsysknn,"[I","createRecommender","knnRecommender",test, as.integer(6012))
#
#
#
#
#
#
#recsysubcf <- .jnew("com/movieRecommender/test")
#result = .jcall(recsysubcf,"[I","createRecommender","userBasedRecommender",test)
#
#rmsrec <- .jnew("com/movieRecommender/rmsRecommender")
#result = .jcall(rmsrec,"D","rmsRec",test)
#
#usrrec <- .jnew("com/movieRecommender/UserRecommender")
#result = .jcall(usrrec,"Ljava/lang/String;","userRec",test, as.integer(6012), as.integer(1), as.integer(12))
#
#osvd <- .jnew("com/movieRecommender/svdRecommender")
#result = .jcall(osvd,"D","svdRec",test)
#
#knnrec <- .jnew("com/movieRecommender/knnRecommender")
#result = .jcall(knnrec,"D","knnRec",test)
#
#irstats <- .jnew("com/movieRecommender/IRStatsEvalRecommender")
#result = .jcall(irstats,"Ljava/lang/String;","irstatseval",test)
#
#recsys <- .jnew("com/movieRecommender/recommender")
#result = .jcall(recsys,"Ljava/lang/String;","getRecommendations","userBasedRecommender",test)
#
#recsys1 <- .jnew("com/movieRecommender/recommender")
#result = .jcall(recsys1,"Ljava/lang/String;","getRecommendations","SlopeOneRecommender",test)
#

}



#java -cp target/mahoutrec-1.0-SNAPSHOT.jar:targets/* com.movieRecommender.knnRec
