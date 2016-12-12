#' Order the observations of an acs object
#'
#' A convenience function for ordering acs objects
#'
#' @param acs An \code{acs} object
#' @param FUN A function for extracting information from an \code{acs} object (e.g. \code{estimate}, \code{standard.error})
#' @param col The column to display, as a numeric index or as a character string
#' @import acs
#' @export

acsOrder <- function(acs,FUN = acs::standard.error,col = 1){acs[order(FUN(acs[,col])),col]}
