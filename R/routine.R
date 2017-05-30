#' Square a number
#'
#' Take in any numeric value and square it
#' @param x A numeric value to be squared
#' @return The square of the input
#' @export
square <- function(x){
  return(x^2)
}

#' An extention of fasttime::fastPOSIXct
#'
#' adds flexibity to both input and output timezones
#' @param x strig vector to interpret as timestamps
#' @param from_tz the time zone x lies in
#' @param the time zone we are converting to
#' @param required.components same as fasttime::fastPOSIXct
#' @example fastPOSIXctWrapper("2017-04-29 20:26:11", from_tz = "America/New_York", to_tz = "UTC")
#' @export
fastPOSIXctWrapper <- function(x, from_tz = NULL, to_tz = NULL, required.components = 3L){

  xx <- Sys.time()
  yy <- as.character(as.POSIXlt(xx, tz = "UTC"))%>%
    substr(1, 19)%>%
    as.POSIXct(tz = from_tz)

  tdff <- round(as.numeric(yy - xx)/.5)*.5*60*60  # seconds

  return(fasttime::fastPOSIXct(x, tz = to_tz, required.components = 3L) + tdff)

}


