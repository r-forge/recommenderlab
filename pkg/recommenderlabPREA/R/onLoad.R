## for Java
.onLoad <- function(libname, pkgname) {
  options(java.parameters="-Xrs")
	.jpackage(pkgname, lib.loc = libname)
}



