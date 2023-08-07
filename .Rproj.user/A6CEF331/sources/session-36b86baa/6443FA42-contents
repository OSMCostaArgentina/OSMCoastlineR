#' Calculate the angle between successive segments along a coastline
#'
#' @param line A way tagged with natural=coastline
#'
#' @return a vector of angles
#' @importFrom sf st_cast
#' @importFrom lwgeom st_geod_azimuth
#' @importFrom units set_units
#' @export
#'
#' @examples
#'  # Download the coastline from the Río Negro province in Argentina
#' RN.coast <- getosmcoastline("Río Negro")
#'
#' # Angles of the first way
#' ang(RN.coast[1,])
ang <- function(line){

  points <- st_cast(line$geometry,"POINT")

  az <- st_geod_azimuth(points)
  az <- as.numeric(set_units(az,"degrees"))

  ang <- rep(1,length(az)-1)

  if(length(az) > 1){

    for (i in 1:(length(az)-1)){
      if (az[i] >= 0){
        if (az[i+1] > 0){
          ang[i] <- az[i] + (180-az[i+1])
        }
        else{
          if (az[i] > 90){
            if (abs(az[i+1]) > 90){
              ang[i] <- az[i] - (180 - abs(az[i+1]))
            }
            else{
              ang[i] <- abs(az[i+1]) - (180 - az[i])
              if (ang[i]<0){
                ang[i] <- az[i]+180+abs(az[i+1])
              }
            }
          }
          else{
            if (abs(az[i+1]) < 90){
              ang[i] <- az[i] + 180 + abs(az[i+1])
            }
            else{
              ang[i] <- az[i] - (180 - abs(az[i+1]))
            }
          }
        }
      }
      else{
        if (az[i+1] < 0){
          ang[i] <- abs(az[i+1]) + (180 - abs(az[i]))
        }
        else{
          if (abs(az[i]) < 90){
            if(az[i+1] > 90){
              ang[i] <- (180 - az[i+1])- abs(az[i])
              if (ang[i] < 0){
                ang[i] <- ang[i]+360
              }
            }
            else{
              ang[i] <- abs(az[i]) + 180 + (180 - az[i+1])
            }
          }
          else{
            if(az[i+1] > 90){
              ang[i] <- (180-abs(az[i]))- az[i+1]
              if (ang[i] < 0){
                ang[i] <- ang[i]+360
              }
            }
            else{
              ang[i] <- (180-abs(az[i])) + 180 + (180-az[i+1])
            }
          }
        }
      }
    }
  }
  ang
}

