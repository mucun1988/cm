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
#' Adds flexibity to both input and output timezones
#' @param x strig vector to interpret as timestamps
#' @param from_tz the time zone x lies in
#' @param the time zone we are converting to
#' @param required.components same as fasttime::fastPOSIXct
#' @examples
#' fastPOSIXctWrapper("2017-04-29 20:26:11", from_tz = "America/New_York", to_tz = "UTC")
#' @export
fastPOSIXctWrapper <- function(x, from_tz = NULL, to_tz = NULL, required.components = 3L){

  xx <- Sys.time()
  yy <- as.character(as.POSIXlt(xx, tz = "UTC"))%>%
    substr(1, 19)%>%
    as.POSIXct(tz = from_tz)

  tdff <- round(as.numeric(yy - xx)/.5)*.5*60*60  # seconds

  return(fasttime::fastPOSIXct(x, tz = to_tz, required.components = 3L) + tdff)

}

#' Apply a boolean function to each entry of x
#'
#' [f(x_1), f(x_2), ..., f(x_n)]
#' @param f a boolean function
#' @param x a numeric vector
#' @return [(f(x_1), f(x_2), ..., f(x_n))]
#' @examples
#' where(function(x) x>0, -2:2)
#' @export
where <- function(f, x) {
  vapply(x, f, logical(1))
}

#' CJ.dt CJ on data.table
#'
#' @param X the first data table
#' @param Y the second data table
#' @examples
#' CJ.dt(data.table(X = 1:2), data.table(Y = c('a','b')))
#' @export
CJ.dt <- function(X, Y) {

  stopifnot(is.data.table(X), is.data.table(Y))

  X = X[, c(k = 1L, .SD)]%>%
  setkey(k)

  Y = Y[, c(k = 1L, .SD)]%>%
  setkey(NULL)

  return( X[Y, allow.cartesian=TRUE][, k := NULL][] )

}




