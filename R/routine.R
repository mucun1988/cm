#' Concatenate two strings
#'
#' @param a: string in front
#' @param b: string in back
#' @return concatenate string
#' @export
'%+%' <- function(a, b){
  paste0(a, b)
}
