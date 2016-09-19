#' A subtle grayscale leaflet basemap
#'
#' A subtle grayscale leaflet basemap to begin a leaflet pipechain with.
#'
#' @param labels Include OSM labels (styled by CartoDB) above all other layers (including paths).
#' @param controls Show leaflet control elements (e.g., zoom buttons, attribution, etc.)
#' @name myLfltGrey
#' @import leaflet
#' @export

myLfltGrey <- function(labels = TRUE, controls = TRUE){

        setup <- ifelse(labels|controls,
                        "var myMap = this;",
                        NULL)

        lbl <- ifelse(labels,
                      "var shadowPane = myMap.getPanes().shadowPane;
                shadowPane.appendChild(
                myMap.layerManager._byLayerId['tile\\nlabels_layer'].getContainer());",
                      NULL
        )

        cntrl <- ifelse(controls,
                        NULL,
                        "myMap.removeControl(myMap.attributionControl);
                                      myMap.removeControl(myMap.zoomControl);")

        jqry <- paste0(
                "function(el, t){",
                setup,lbl,cntrl,"}"
        )

        leaflet() %>%
                addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
                addProviderTiles(providers$CartoDB.PositronOnlyLabels,
                                 layerId = 'labels_layer' ) %>%
                htmlwidgets::onRender(jqry)
}
