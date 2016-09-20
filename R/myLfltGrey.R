#' A subtle grayscale leaflet basemap
#'
#' A subtle grayscale leaflet basemap to begin a leaflet pipechain with.
#'
#' @param bumpLabels Include OSM labels (styled by CartoDB) above all other layers (including paths).
#' @param hideControls Show leaflet control elements (e.g., zoom buttons, attribution, etc.)
#' @name myLfltGrey
#' @import leaflet
#' @export


myLfltGrey <- function(bumpLabels = TRUE, hideControls = TRUE){
        lbl <- if(bumpLabels == TRUE){"var shadowPane = myMap.getPanes().shadowPane;shadowPane.appendChild(myMap.layerManager._byLayerId['tile\\nlabels_layer'].getContainer());"}else{""}


        cntrl <- if(hideControls == TRUE){"myMap.removeControl(myMap.attributionControl);myMap.removeControl(myMap.zoomControl);"}else{""}


        jqry <- paste0("function(el, t){var myMap = this;",lbl,cntrl,"}")


        leaflet() %>%
                addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
                addProviderTiles(providers$CartoDB.PositronOnlyLabels,
                                 layerId = 'labels_layer' ) %>%
                htmlwidgets::onRender(jqry)

}


