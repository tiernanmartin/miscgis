#' @title Create an sf object with metadata contained in a nested tibble
#' @description A function that returns an sf object with metadata columns contained in a nested tibble (by row).
#' @name st_nest_sf
#' @param x an \code{sf} object
#' @return an \code{sf} object with metadata columns contained in a nested tibble (by row).
#' @import tibble
#' @import sf
#' @import purrr
#' @import dplyr
#' @import tidyr
#' @examples
#'
#' library(sf)
#' nc <- st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)
#'
#' \dontrun{
#'   st_nest_sf(nc)
#' }
#' @export
st_nest_sf <- function(x){

  x %>%
    rename_if(.predicate = function(x) any(class(x) %in% 'sfc'), ~ 'geometry') %>%
    rownames_to_column('ROW') %>%
    nest(-ROW) %>%
    select(-ROW) %>%
    mutate(geometry = map(data, "geometry") %>% flatten %>% st_sfc,
           data = map(data, miscgis::st_drop_geometry)) %>%
    st_sf %>%
    st_set_crs(st_crs(x))
}

