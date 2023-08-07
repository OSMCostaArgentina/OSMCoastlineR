#' Analize a coast section
#'
#' @param osmcoastline Output of the function 'getosmcoastline' or a character vector with place name
#'
#' @param thrangle A threshold angle in degrees
#'
#' @param thrlong A threshold length in meters
#'
#' @param pgs Logical. It must to check segments with PGS source?
#'
#' @param thrperc Percentage of segments above which the coastline way needs revision
#'
#' @return An sf object
#' @importFrom units set_units
#' @importFrom stringr str_detect
#' @export
#'
#' @examples
#' # Download the coastline from the Río Negro province in Argentina
#' RN.coast <- getosmcoastline("Río Negro")
#'
#' # Check three segments
#' RN.check <- checkosmcoast(RN.coast[3:5,])
#'
#' # Map the selected segments
#' plot(RN.check)
checkosmcoast <- function(osmcoastline, thrangle = 30, thrlong = 500, pgs = TRUE, thrperc = 10){

  if (inherits(osmcoastline,"sf")){
    coast <- osmcoastline
    }
  else if (inherits(osmcoastline,"character")){
    coast <- getosmcoastline(osmcoastline)
  }


  thrlong <- set_units(thrlong,"m")

  checkangles <- rep(FALSE, nrow(coast))
  checklong <- checkangles

  # Check PGS
  if (pgs==TRUE){
    checkPGS <- checkangles
    checkPGS=str_detect(coast$source,"PGS")
  }

  for(j in 1:nrow(coast)){

      # Check angles

     angles <- ang(coast[j,])


     nangles <- sum(angles < thrangle) + sum(angles > (360 - thrangle))
     percangles <- nangles*100/length(ang)

     if (percangles > thrperc){
       checkangles[j] <- TRUE
     }
      #Check length

      lengths <-  long(coast[j,])

      nlengths <- sum(lengths>thrlong)
      perclengths <- nlengths*100/length(lengths)

      if (perclengths > thrperc){
        checklong[j] <- TRUE
      }
  }

  coast <- cbind(coast,checkangles,checklong,checkPGS)

  out <- subset(coast,checklong == TRUE | checkangles == TRUE | checkPGS == TRUE,
                              select = c("source","checklong","checkangles","checkPGS"))
  out
}
