## for Java
.onLoad <- function(libname, pkgname) {
  options(java.parameters="-Xrs")
  .jpackage(pkgname, lib.loc = libname)
  
}

.onAttach <- function(...) {
  recommenderRegistry$set_entry(
    method="PREA", dataType = "realRatingMatrix", fun=REAL_PREA,
    description="Recommender based on PREA")
}

