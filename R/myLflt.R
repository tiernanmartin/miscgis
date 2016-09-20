#' A pleasant leaflet basemap
#'
#' A pleasant-looking leaflet basemap to begin a leaflet pipechain with.
#'
#' Note: because this uses tiles from a Mapbox URL, this function will not work correctly
#' in a PDF document - use \code{myLflyGrey} instead.
#'
#' @name myLflt
#' @import leaflet ggthemes
#' @export
NULL

#` @rdname myLflt
#` @export
myLflt <- function(){
        leaflet() %>%
                addTiles(
                        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                )
}

#` @rdname myLflt
#` @export
mapPalCat <- ggthemes_data$tableau$colors$tableau20[!grepl("light",names(ggthemes_data$tableau$colors$tableau20))]



