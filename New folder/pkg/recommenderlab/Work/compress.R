### recompress all data files

library(tools)

d <- "../data/"

f <- dir(d)

for(file in f) {
    file <- paste(d, file, sep ='')
    print(checkRdaFiles(file))
    resaveRdaFiles(file, compress = "auto")
    print(checkRdaFiles(file))
}
