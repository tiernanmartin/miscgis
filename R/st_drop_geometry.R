#' Read a file from Google Drive.
#'
#'
#' @param x an \code{sf} object
#'
#' @return a \code{tibble} object without the \code{geometry} column
#' @export

st_drop_geometry <- function(x) {
        if(inherits(x,"sf")) {
                x <- st_set_geometry(x, NULL)
                class(x) <- 'data.frame'
                x <- as_tibble(x)
        }
        return(x)
}
