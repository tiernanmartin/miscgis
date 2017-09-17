#' A pleasant leaflet basemap
#'
#' A pleasant-looking leaflet basemap to begin a leaflet pipechain with.
#'
#' Note: because this uses tiles from a Mapbox URL, this function will not work correctly
#' in a PDF document - use \code{myLflyGrey} instead.
#'
#' @name myLflt
#' @param tile_opts list, override arguments to the defaults in \code{\link[tileOptions]{leaflet}}
#' @param chinatown logical, whether to use Hing Hay Park as the default \code{\link[setView]{leaflet}}
#' @import leaflet
#' @import purrr
#' @export
myLflt <- function(tile_opts = tileOptions(), chinatown = FALSE){
        l <- leaflet() %>%
                addTiles(
                        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = '',
                        options = list_modify(tileOptions(), !!! tile_opts))
        if(!chinatown){l}else{l %>% setView(-122.3254, 47.59853, zoom = 15)}


}


