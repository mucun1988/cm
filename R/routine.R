#' Square a number
#'
#' Take in any numeric value and square it
#' @param x A numeric value to be squared
#' @return The square of the input
#' @export
square <- function(x){
  return(x^2)
}

#' @export
fastPOSIXctWrapper <- function(x, from_tz = NULL, to_tz = NULL, required.components = 3L){

  xx <- Sys.time()
  yy <- as.character(as.POSIXlt(xx, tz = "UTC"))%>%
    substr(1, 19)%>%
    as.POSIXct(tz = from_tz)

  tdff <- round(as.numeric(yy - xx)/.5)*.5*60*60  # seconds

  return(fasttime::fastPOSIXct(x, tz = to_tz, required.components = 3L) + tdff)

}

# fastPOSIXctWrapper("2017-04-29 20:26:11", from_tz = "America/New_York", to_tz = "UTC")


## difference btw UTC and New York Time
xx <- Sys.time()
yy <- as.character(as.POSIXlt(xx, tz = "UTC" ))%>%
  substr(1, 19)%>%
  as.POSIXct(tz = "America/New_York")
tdff <- round(as.numeric(yy - xx))*60*60  # seconds


fastPOSIXct(time, tz = "UTC") + tdff
