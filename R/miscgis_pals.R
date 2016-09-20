#' Mapping Color Palettes
#'
#' A set of color palettes.
#'
# '@format A \code{list}.
#' @import ggthemes RColorBrewer
#' @name miscgis_pals
#' @export
miscgis_pals <- {
        x <- list()
        x$tableau_cat <- ggthemes::ggthemes_data$tableau$colors$tableau20[!grepl("light",names(ggthemes_data$tableau$colors$tableau20))]

        x
        }
