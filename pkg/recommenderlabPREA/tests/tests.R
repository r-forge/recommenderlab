library(recommenderlabPREA)
library(testthat)

data("MovieLense")
MovieLense100 <- MovieLense[rowCounts(MovieLense) >100, 1:50]
train <- MovieLense100[1:50,] 
test <- MovieLense100[51:53,] 


methods <- grep("^PREA_", recommenderRegistry$get_entry_names(), value=TRUE)
methods <- gsub("_realRatingMatrix", "", methods)
methods

for (m in methods) {
  test_that(m, rec <- Recommender(train, method = m))
  expect_is(rec, "Recommender")
  
  test_that(m, pre <- predict(rec, 1:3, type="topNList"))
  expect_is(pre, "topNList")
  
  test_that(m, pre <- predict(rec, 1:3, type="ratings"))
  expect_is(pre, "ratings")
  
  test_that(m, pre <- predict(rec, test[1:3,], type="topNList"))
  expect_is(pre, "topNList")
  
  test_that(m, pre <- predict(rec, test[1:3,], type="ratings"))
  expect_is(pre, "ratings")
}


