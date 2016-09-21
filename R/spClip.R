#' Clips a SP Object with Another SP Object
#'
#' \code{sp_clip} converts a SpatialPolygons object into a SpatialPolygonsDataFrames.
#'
#' @import sp
#' @name spClip
#' @export

spClip <- function(orig, sp){

        new <-
                gDifference(spgeom1 = orig, spgeom2 = sp,
                            byid = TRUE) %>%
                mySptlPolyDF()

        new@data <-
                gCentroid(new,byid = T) %>%
                over(., orig) %>%
                as.data.frame()

        new

}
