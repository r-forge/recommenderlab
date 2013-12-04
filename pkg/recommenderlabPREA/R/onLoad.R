## for Java
.onLoad <- function(libname, pkgname) {
  options(java.parameters="-Xrs -Xmx2048m") ### check for memory
  .jpackage(pkgname, jars="prea.jar", lib.loc = libname)
  
}


.onAttach <- function(...) {
  recommenderRegistry$set_entry(
    method="PREA", dataType = "realRatingMatrix", fun=REAL_PREA,
    description="Recommender based on PREA")

  recommenderRegistry$set_entry(
    method="PREA_RANDOM", dataType = "realRatingMatrix", fun=REAL_PREA_RANDOM,
    description="Recommender based on Random method in PREA")

  recommenderRegistry$set_entry(
    method="PREA_REGSVD", dataType = "realRatingMatrix", fun=REAL_PREA_REGSVD,
    description="Recommender based on Random method in PREA") #, parameters=.REAL_PREA_REGSVD_PARAM
  
  recommenderRegistry$set_entry(
    method="PREA_ITEMAVG", dataType = "realRatingMatrix", fun=REAL_PREA_ITEMAVG,
    description="Recommender based on Item Average method in PREA")
  
  recommenderRegistry$set_entry(
    method="PREA_AVG", dataType = "realRatingMatrix", fun=REAL_PREA_AVG,
    description="Recommender based on Average method in PREA")
  
  recommenderRegistry$set_entry(
    method="PREA_USERAVG", dataType = "realRatingMatrix", fun=REAL_PREA_USERAVG,
    description="Recommender based on User Average method in PREA")
  
  recommenderRegistry$set_entry(
    method="PREA_SLOPEONE", dataType = "realRatingMatrix", fun=REAL_PREA_SLOPEONE,
    description="Recommender based on Slope One method in PREA")
}


