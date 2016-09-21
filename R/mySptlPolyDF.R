#' Convert a SpatialPolygons object into a SpatialPolygonsDataFrame
#'
#' \code{mySptlPolyDF} converts a SpatialPolygons object into a SpatialPolygonsDataFrames.
#'
#' @import sp
#' @name mySptlPolyDF
#' @export

mySptlPolyDF <- function(sp){

        sp_rn <- row.names(sp)

        sp_len <- sp@polygons %>% length()

        if(length(sp_rn) != sp_len){
                return(message("The `sp` object does not have the same number of row names as the list of polygons"))
        }

        else{
                nodata <- rep(NA, times = sp_len) %>% as.data.frame()

                rownames(nodata) <- sp_rn

                sp %<>%
                        SpatialPolygonsDataFrame(data = nodata)

                return(sp)
        }


}
