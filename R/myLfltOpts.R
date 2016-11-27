#' Options for styling a leaflet map
#'
#' A set of style options to be applied to a leaflet htmlwidget using a JS script.
#'
#' @param bumpLabels Include OSM labels (styled by CartoDB) above all other layers (including paths).
#' @param hideControls Show leaflet control elements (e.g., zoom buttons, attribution, etc.)
#' @name myLfltOpts
#' @import leaflet htmlwidgets
#' @export

myLfltOpts <- function(map, bumpLabels = TRUE, hideControls = TRUE){

        lbl <- if(bumpLabels == TRUE){"var shadowPane = myMap.getPanes().shadowPane;shadowPane.style.pointerEvents = 'none';shadowPane.appendChild(myMap.layerManager._byLayerId['tile\\nlabels_layer'].getContainer());"}else{""}


        cntrl <- if(hideControls == TRUE){"myMap.removeControl(myMap.attributionControl);myMap.removeControl(myMap.zoomControl);"}else{""}


        jqry <- paste0("function(el, t){var myMap = this;",lbl,cntrl,"}")

        map %>% htmlwidgets::onRender(jqry)


}
