library("recommenderlab")


## read db (3+ stars is good)
data <- read.table("ml-data/u.data", 
    col.names=c("user", "item", "rating", "time"))


db <- as(data, "realRatingMatrix")

## add movie labels
genres <- read.table("ml-data/u.genre", sep ="|", quote="\"")
colnames(genres) <- c("genre", "id")

movies <- read.table("ml-data/u.item", sep ="|", quote="\"")
colnames(movies) <- c("id", "name", "date", "NA", "URL", 
as.character(genres$genre))


m <- match(colnames(db), movies$id)
movies <- movies[m,]
ilabels <- movies$name
gen <- movies[6:24]

## remove duplicated movies
dup <- which(duplicated(ilabels))

# movies$name[dup]

ilabels <- ilabels[-dup]
db <- db[,-dup]
gen <- gen[-dup,]

colnames(db) <- ilabels

#itemInfo(db) <- cbind(itemInfo(db), gen)


MovieLense <- db
MovieLense


save(MovieLense, file = "MovieLense.rda")

