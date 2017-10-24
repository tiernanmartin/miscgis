#' @title Convert a structured character string into a named vector.
#' @description A function for converting a character string into a named vector.
#' @param string A character string containing codes and their corresponding values.
#' @param col_sep A character string indicating a new column (e.g, \code{"\\s=\\s"}).
#' @param row_sep A character string indicating a new row (e.g, \code{"\n"}).
#' @name parse_lookup_string
#' @import purrr
#' @import dplyr
#' @import stringr
#' @import tibble
#' @importFrom magrittr "set_rownames"
#' @export
parse_lookup_string <- function(string, col_sep, row_sep){
  str_split(string, pattern = row_sep) %>%
    flatten %>%
    keep(~ str_detect(.x,"")) %>%
    str_replace_all("\\\n","") %>%
    map_chr(c) %>%
    map_df(~.x %>% str_split(pattern = col_sep) %>% as.data.frame %>% t %>% as_data_frame ) %>%
    magrittr::set_rownames(NULL) %>%
    mutate_all(as.character) %>%
    pmap(~ set_names(..2, ..1)) %>%
    unlist
}


