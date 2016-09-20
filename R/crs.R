#' Coordinate reference systems
#'
#' A set of coordinate reference systems.
#'
#' @import sp
#' @name crs
NULL

#' @rdname crs
#' @export
crs_proj <- CRS("+init=epsg:4326") # This project will use WGS 84 projected coordinate system

#' @rdname crs
#' @export
crs_wspn <- CRS("+init=epsg:2285") # Washington State plane CRS
