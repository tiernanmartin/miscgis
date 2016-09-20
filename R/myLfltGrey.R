#' A subtle grayscale leaflet basemap
#'
#' A subtle grayscale leaflet basemap to begin a leaflet pipechain with.
#'
#' @param labels Include OSM labels (styled by CartoDB) above all other layers (including paths).
#' @param controls Show leaflet control elements (e.g., zoom buttons, attribution, etc.)
#' @name myLfltGrey
#' @import leaflet
#' @export


myLfltGrey <- function(){
        leaflet() %>%
                addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
                addProviderTiles(providers$CartoDB.PositronOnlyLabels,
                                 layerId = 'labels_layer' )

}


