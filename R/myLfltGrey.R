#' A subtle grayscale leaflet basemap
#'
#' A subtle grayscale leaflet basemap to begin a leaflet pipechain with.
#'
#' @param data A data object (currently supported objects are matrices, data frames, and spatial objects from the sp package of classes)
#' @name myLfltGrey
#' @import leaflet
#' @export


myLfltGrey <- function(data = NULL, width = NULL, height = NULL){

        leaflet(data, width = width, height = height) %>%
                addProviderTiles(providers$CartoDB.PositronNoLabels)

}


