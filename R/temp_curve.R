#' Composite temperature curve ranging the Cretaceous up to the Holocene
#'
#' A dataset containing temperature trends from several records for millions of
#' years
#'
#' @format A data frame with 25,166 rows and 5 variables:
#' \describe{
#'   \item{Proxy}{temperature in degree celcius}
#'   \item{Age}{Age in Myr before present}
#'   \item{uncer}{uncertainty in Proxy when given}
#'   \item{record}{type of archive; sediments, instrumental, or model}
#'   \item{scenario}{emmision scenarios for modelled data}
#' }
#' @source compilation in directory data-raw
"temp_curve"
