#' An sf implementation of sp::over()
#'
#' A one-to-one spatial overlay function, similar to sp::over()
#'
#' @param x,y An sf-class object
#' @name over_sf
#' @import sf purrr
#' @export

over_sf <- function(x,y){

        st_intersects(x,y) %>%
                map(~ if(length(.x) == 0){NA_integer_ }else .x[1]) %>%
                map_df( ~ y[.x,]) %>%
                as_tibble %>%
                select(-matches('geom')) %>%
                bind_cols(x,.) %>%
                as_tibble %>%
                st_as_sf
}


