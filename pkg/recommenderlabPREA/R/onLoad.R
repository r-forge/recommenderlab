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
    description="Recommender based on Random method in PREA", parameters=.REAL_PREA_RANDOM_PARAM)

  recommenderRegistry$set_entry(
    method="PREA_REGSVD", dataType = "realRatingMatrix", fun=REAL_PREA_REGSVD,
    description="Recommender based on Reg SVD method in PREA", parameters=.REAL_PREA_REGSVD_PARAM)
  
  recommenderRegistry$set_entry(
    method="PREA_ITEMAVG", dataType = "realRatingMatrix", fun=REAL_PREA_ITEMAVG,
    description="Recommender based on Item Average method in PREA", parameters=.REAL_PREA_ITEMAVG_PARAM)
  
  recommenderRegistry$set_entry(
    method="PREA_AVG", dataType = "realRatingMatrix", fun=REAL_PREA_AVG,
    description="Recommender based on Average method in PREA", parameters=.REAL_PREA_AVG_PARAM)
  
  recommenderRegistry$set_entry(
    method="PREA_USERAVG", dataType = "realRatingMatrix", fun=REAL_PREA_USERAVG,
    description="Recommender based on User Average method in PREA", parameters=.REAL_PREA_USERAVG_PARAM)
  
  recommenderRegistry$set_entry(
    method="PREA_SLOPEONE", dataType = "realRatingMatrix", fun=REAL_PREA_SLOPEONE,
    description="Recommender based on Slope One method in PREA", parameters=.REAL_PREA_SLOPEONE_PARAM)
  
  recommenderRegistry$set_entry(
    method="PREA_NMF", dataType = "realRatingMatrix", fun=REAL_PREA_NMF,
    description="Recommender based on NMF method in PREA", parameters=.REAL_PREA_NMF_PARAM)
  
  recommenderRegistry$set_entry(
    method="PREA_PMF", dataType = "realRatingMatrix", fun=REAL_PREA_PMF,
    description="Recommender based on PMF method in PREA", parameters=.REAL_PREA_PMF_PARAM)
  
  recommenderRegistry$set_entry(
    method="PREA_BPMF", dataType = "realRatingMatrix", fun=REAL_PREA_BPMF,
    description="Recommender based on BPMF method in PREA", parameters=.REAL_PREA_BPMF_PARAM)
  
  recommenderRegistry$set_entry(
    method="PREA_NLPMF", dataType = "realRatingMatrix", fun=REAL_PREA_NLPMF,
    description="Recommender based on NLPMF method in PREA", parameters=.REAL_PREA_NLPMF_PARAM)
  
  recommenderRegistry$set_entry(
    method="PREA_NPCA", dataType = "realRatingMatrix", fun=REAL_PREA_NPCA,
    description="Recommender based on fast NPCA method in PREA", parameters=.REAL_PREA_NPCA_PARAM)
  
  recommenderRegistry$set_entry(
    method="PREA_USERBASED", dataType = "realRatingMatrix", fun=REAL_PREA_USERBASED,
    description="Recommender based on the userbased method in PREA", parameters=.REAL_PREA_USERBASED_PARAM)
}


