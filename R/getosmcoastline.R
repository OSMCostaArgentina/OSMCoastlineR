#' Get the OSM Coastline
#'
#' This function gets from OpenStreetMat the line tagged with "natural=coastline"
#' in a selected country
#'
#' @param place_name The name of the administrative boundary from which
#' to extract the coastline.
#'
#'
#' @return An object of class 'sf' with the selected coastline
#' @importFrom osmdata opq
#' @importFrom osmdata getbb
#' @importFrom osmdata add_osm_feature
#' @importFrom osmdata osmdata_sf
#' @export
#'
#' @examples
#' RN <- getosmcoastline("Río Negro")
getosmcoastline <- function(place_name){

bb <- getbb(place_name) # Obtains the bounding box

coast <- opq(bbox = bb)
coast <- add_osm_feature(coast, key = "natural",value = "coastline")
coast <- osmdata_sf(coast)

coast <- coast$osm_lines

coast
}
