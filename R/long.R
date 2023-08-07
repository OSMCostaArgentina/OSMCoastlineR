#' Calculate the length of successive segments along a coastline portion
#'
#' @param line A way tagged with natural=coastline
#'
#' @return A vector of the segments length in meters
#' @importFrom lwgeom st_geod_distance
#' @importFrom sf st_cast
#' @importFrom lwgeom st_startpoint
#' @importFrom units set_units
#' @export
#'
#' @examples
#'  # Download the coastline from the Río Negro province in Argentina
#' RN.coast <- getosmcoastline("Río Negro")
#'
#' # Lengths of the first way
#' long(RN.coast[1,])
long <- function(line){

  points <- st_cast(line$geometry,"POINT")

  startpoints <- st_startpoint(points)

  lengths <- 1:length(startpoints)-1

  for (k in 1:length(startpoints)-1){
    lengths[k] <- st_geod_distance(startpoints[k],startpoints[k+1])
  }

  lengths <- set_units(lengths,"m")

  lengths

}

