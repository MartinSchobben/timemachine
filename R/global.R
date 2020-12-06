globalVariables(c("clim_trends", "Age", "Proxy", "y", "xmin", "xmax", "xmid",
                  "lon", "lat"))

#' @noRd
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))
}

#' @noRd
.onAttach <- function(libname, pkgname) {
  shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))
}
