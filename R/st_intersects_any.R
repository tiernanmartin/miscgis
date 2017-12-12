#' @title Do any spatial intersections exist?
#' @description A function that determines whether any spatial intersections exist between two \code{sf} objects.
#' @name st_intersects_any
#' @param x,y \code{sf} objects
#' @return logical
#' @import sf purrr
#' @importFrom magrittr "%>%"
#' @export
st_intersects_any <- function(x,y){
  st_intersects(x,y) %>%
    map_lgl(~ length(.x)>0)
}

