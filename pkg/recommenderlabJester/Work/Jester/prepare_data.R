library(recommenderlab)

set.seed(1234)

J1 <- read.csv("jester-data-1.csv", header=FALSE)
dim(J1)
J1[J1 == 99] <- NA

#J2 <- read.csv("jester-data-2.csv", header=FALSE)
#dim(J2)
#J2[J2 == 99] <- NA

#J3 <- read.csv("jester-data-3.csv", header=FALSE)
#dim(J3)
#J3[J3 == 99] <- NA

### first row is number of votes
Jester <- J1[,-1]
#Jester <- rbind(J1,J2,J3)


#Jester <- as(as.matrix(Jester+11), "realRatingMatrix")
Jester <- as(as.matrix(Jester), "realRatingMatrix")
colnames(Jester) <- paste('j',1:ncol(Jester),sep='')
rownames(Jester) <- paste('u',1:nrow(Jester),sep='')
save(Jester, file="Jester.rda")


### a 5k sample of Jester
Jester5k <- sample(Jester, 5000)

save(Jester5k, file="Jester5k.rda")



