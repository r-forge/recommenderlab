library(rJava)
.jinit("mahoutrec-1.0-SNAPSHOT.jar", parameters="-Xmx512m")
.jaddClassPath("../targets/mahout-core-0.4.jar")
.jaddClassPath("../targets/slf4j-api-1.6.0.jar")
.jaddClassPath("../targets/google-collections-1.0-rc2.jar")
.jaddClassPath("../targets/mahout-collections-1.0.jar")
.jaddClassPath("../targets/mahout-utils-0.4.jar")
.jaddClassPath("../targets/mahout-math-0.4.jar")
.jaddClassPath("../targets/commons-math-1.2.jar")
.jaddClassPath("../targets/slf4j-jcl-1.6.0.jar")
.jaddClassPath("../targets/commons-logging-1.1.1.jar")
.jaddClassPath("../targets/uncommons-maths-1.2.jar")
.jaddClassPath("../targets/uncommons-math-1.0.2.jar")
.jaddClassPath("../targets/commons-cli-2.0-mahout.jar")
test <- .jnew("java/lang/String","/usr/local/recommender/userRec/datasets/movieRatings.dat")

recsystreeclustering <- .jnew("com/movieRecommender/test")
result = .jcall(recsystreeclustering,"[I","createRecommender","treeClusteringRecommender",test, as.integer(6012))

recsysslopeone <- .jnew("com/movieRecommender/test")
result = .jcall(recsysslopeone,"[I","createRecommender","slopeOneRecommender",test, as.integer(6012))

recsysrandom <- .jnew("com/movieRecommender/test")
result = .jcall(recsysrandom,"[I","createRecommender","randomRecommender",test, as.integer(6012))

recsyssvd <- .jnew("com/movieRecommender/test")
result = .jcall(recsyssvd,"[I","createRecommender","svdRecommender",test, as.integer(6012))

recsysknn <- .jnew("com/movieRecommender/test")
result = .jcall(recsysknn,"[I","createRecommender","knnRecommender",test, as.integer(6012))






recsysubcf <- .jnew("com/movieRecommender/test")
result = .jcall(recsysubcf,"[I","createRecommender","userBasedRecommender",test)

rmsrec <- .jnew("com/movieRecommender/rmsRecommender")
result = .jcall(rmsrec,"D","rmsRec",test)

usrrec <- .jnew("com/movieRecommender/UserRecommender")
result = .jcall(usrrec,"Ljava/lang/String;","userRec",test, as.integer(6012), as.integer(1), as.integer(12))

osvd <- .jnew("com/movieRecommender/svdRecommender")
result = .jcall(osvd,"D","svdRec",test)

knnrec <- .jnew("com/movieRecommender/knnRecommender")
result = .jcall(knnrec,"D","knnRec",test)

irstats <- .jnew("com/movieRecommender/IRStatsEvalRecommender")
result = .jcall(irstats,"Ljava/lang/String;","irstatseval",test)

recsys <- .jnew("com/movieRecommender/recommender")
result = .jcall(recsys,"Ljava/lang/String;","getRecommendations","userBasedRecommender",test)

recsys1 <- .jnew("com/movieRecommender/recommender")
result = .jcall(recsys1,"Ljava/lang/String;","getRecommendations","SlopeOneRecommender",test)








java -cp target/mahoutrec-1.0-SNAPSHOT.jar:targets/* com.movieRecommender.knnRec
