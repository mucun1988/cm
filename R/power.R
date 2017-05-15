#' Square a number
#'
#' Takes in any numeric value and squares it.
#' @param x: A numeric value to be squared
#' @return The square of the input
#' @export
square <- function(x){
  return(x^2)
}

#' Cube a number
#' @param x: Number to be cubed
#' @return The cube of the input
cube <- function(x){
  return(x^3)
}

#' Concatenate two strings
#'
#' @param a: string in front
#' @param b: string in back
#' @return concatenate string
#' @export
'%+%' <- function(a, b){
  paste0(a, b)
}
