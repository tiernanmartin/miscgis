#' A subtle grayscale leaflet basemap
#'
#' A subtle grayscale leaflet basemap to begin a leaflet pipechain with.
#'
#' @name myLfltGrey
#' @import leaflet
#' @export

myLfltGrey <- function(){
        leaflet() %>%
                addProviderTiles(provider = "Esri.WorldGrayCanvas")
}
