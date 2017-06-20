#' Make a unique character string for anonymous record identification
#'
#' Make a unique character string for anonymous record identification
#'
#' @param rn number of records (e.g. \code{nrow(df)})
#' @param string a vector of characters to form the identifier string
#' @param .shuffle logical, whether to shuffle the ids or keep them in order
#' @name make_id
#' @import plyr purrr
#' @export

make_id <- function(rn, string = LETTERS, .shuffle = FALSE){
        # browser()
        times <- plyr::round_any(log(rn,length(string)),1,ceiling)

        res <- map_chr(.x = purrr::cross(rep(list(string),times)),
                       .f = ~ paste(rev(.x), collapse = ""))

        if(.shuffle) return(res[sample(rn,replace = FALSE)])

        return(res[1:rn])

}

