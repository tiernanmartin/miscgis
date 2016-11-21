#' A set of functions for working with sf objects
#'
#' Functions for working with sf objects.
#'
#' @import sf
#' @param x A sf-class data object
#' @param FUN A function for creating a sfg-class object (e.g., sf::multipolygon)
#' @param crs A crs-class object (coordinate reference system)
#' @name coerce_geom
NULL

#' @rdname coerce_geom
#' @export
coerce_from_geometrycollection <- function(x, crs = NULL){

        crs <- {
                if(is.null(crs)){
                        st_crs(x)
                }else{
                        crs
                }
        }

        geom <- st_geometrycollection() %>% st_sfc()

        for(i in 1:nrow(x)){

                geom[[i]] <- {
                        if(
                                st_geometry(x)[[i]] %>% unlist(recursive = FALSE) %>% summary() %>% .[1,3] == "list"
                        ){
                                st_geometry(x)[[i]] %>% unlist(recursive = FALSE) %>% st_multipolygon()
                        }else{
                                st_geometry(x)[[i]] %>% unlist(recursive = FALSE) %>% st_polygon()
                        }
                }
        }

        x$geom <- geom
        st_crs(x) <- crs
        return(x)

}

#' @rdname coerce_geom
#' @export
coerce_to_geom <- function(x, FUN,crs = NULL){

        is_sf <- any(class(x) == 'sf')

        if(!is_sf){
                stop("Not an sf object")
        }
        crs <- {
                if (is.null(crs)) {
                        st_crs(x)
                }
                else {
                        crs
                }
        }
        geom <- FUN() %>% st_sfc()
        coerce_ <- function(x) {
                is_list <- st_geometry(x)[[i]] %>% summary %>% .[1, 3]
                if (is_list == "list") {
                        st_geometry(x)[[i]] %>% FUN()
                }
                else {
                        st_geometry(x)[[i]] %>% list %>% FUN()
                }
        }
        for (i in 1:nrow(x)) {
                geom[[i]] <- coerce_(x)
        }
        new_sf <- x %>%
                select(-matches("geom")) %>%
                mutate(geometry = geom) %>%
                st_as_sf() %>%
                st_set_crs(crs)

        return(new_sf)

}
