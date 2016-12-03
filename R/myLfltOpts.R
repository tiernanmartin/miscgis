#' Options for styling a leaflet map
#'
#' A set of frequently-used leaflet map options.
#'
#' @param map The map to add the tile layer to.
#' @param tileLabels If \code{tileLabels} is \code{TRUE}, a tile layer of OSM labels (styled by CartoDB) will be added.
#' @param fullScreenBtn If \code{fullScreenBtn} is \code{TRUE}, a fullscreen control button will be added.
#' @param bumpTileLabels If \code{bumpTileLabels} is \code{TRUE}, the OSM labels will sit above all other map layers.
#' @param hideControls If \code{hideControls} is \code{TRUE}, control elements will be hidden.
#' @name myLfltOpts
#' @import leaflet leaflet.extras htmlwidgets
#' @export

myLfltOpts <- function(map, tileLabels = TRUE, fullScreenBtn = TRUE, bumpTileLabels = TRUE, hideControls = TRUE){

        # Arguments to pass to the chain

        fs <- if(fullScreenBtn){
                function(x)addFullscreenControl(x)
        }else{function(x)x}

        tileLbl <- if(tileLabels){
                function(x)addProviderTiles(x,
                                            providers$CartoDB.PositronOnlyLabels,
                                            layerId = 'labels_layer' )
        }else{function(x)x}

        # JS strings

        lbl <- if(bumpTileLabels == TRUE){"var shadowPane = myMap.getPanes().shadowPane;shadowPane.style.pointerEvents = 'none';shadowPane.appendChild(myMap.layerManager._byLayerId['tile\\nlabels_layer'].getContainer());"}else{""}

        cntrl <- if(hideControls == TRUE){"myMap.removeControl(myMap.attributionControl);myMap.removeControl(myMap.zoomControl);"}else{""}

        jqry <- paste0("function(el, t){var myMap = this;",lbl,cntrl,"}")


        # Final function

        map %>%
                tileLbl()%>%
                fs() %>%
                htmlwidgets::onRender(jqry)

}
