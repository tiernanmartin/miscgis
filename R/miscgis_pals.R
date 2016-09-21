#' Mapping Color Palettes
#'
#' A set of color palettes.
#'
# '@format A \code{list}.
#' @import ggthemes RColorBrewer purrr dplyr
#' @name miscgis_pals
#' @export
miscgis_pals <- {
        x <- list()
        library(purrr)
        library(dplyr)
        # cols_vctr <- ggthemes::ggthemes_data$tableau$colors$tableau20[!grepl("light",names(ggthemes_data$tableau$colors$tableau20))]

        x$tableau_cat <-
                ggthemes::ggthemes_data$tableau$colors[["tableau20"]] %>%
                map(list) %>% subset(!grepl("light",names(.)))

        x
        }
