#' duluthBasemap data
#'
#' A polygon map of the city of Duluth, Minnesota. Each hex shape is approximately 1000 by 1000 feet
#'
#'
#' @format ## `duluthBasemap`
#' A data frame with 45 rows and 5 columns:
#' \describe{
#'   \item{TRACTFIPS}{FIPS Code}
#'   \item{NRI_FEMA}{FEMA risk index for all hazards}
#'   \item{SVI_CDC}{CDC/ATSDR overall socical vulnerability index (SVI)}
#'   \item{geometry}{geometry}
#'   ...
#' }
#' @name duluthBasemap
#' @usage data(duluthBasemap)
#' @source <https://hazards.fema.gov/nri/map>
#' @source <https://www.atsdr.cdc.gov/placeandhealth/svi/interactive_map.html>
NULL
