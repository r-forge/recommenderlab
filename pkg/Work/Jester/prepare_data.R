library(arules)

J1 <- read.csv("jester-data-1.csv", header=FALSE)
dim(J1)
J1[J1 == 99] <- NA

J2 <- read.csv("jester-data-2.csv", header=FALSE)
dim(J2)
J2[J2 == 99] <- NA

J3 <- read.csv("jester-data-3.csv", header=FALSE)
dim(J3)
J3[J3 == 99] <- NA

Jester <- rbind(J1,J2,J3)
save(Jester, file="Jester.rda")

## binarize

q75 <- quantile(as.vector(Jester), 0.75, na.rm=T)

jj <- Jester>=q75
JesterBin <- as(jj, "transactions")
JesterBin[size(JesterBin)>0]

## get 25% favorite jokes
#q75 <- apply(Jester, MARGIN=1, quantile, 0.75, na.rm=TRUE)
#jj <- Jester > q75
#JesterBin <- as(jj, "transactions")



save(JesterBin, file = "JesterBin.rda")


