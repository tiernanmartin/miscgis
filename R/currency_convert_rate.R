#' An currency conversion function
#'
#' A one-to-one spatial overlay function, similar to sp::over()
#'
#' @param target_year The year to convert the dollar value to
#' @param base_year The year to convert the dollar value to
#' @name currency_convert_rate
#' @import acs
#' @export

currency_convert_rate <- function(target_year,base_year){
        .env = environment()
        data("cpi", envir = .env)
        target_rate <-  cpi[as.character(target_year)]
        base_rate <-  cpi[as.character(base_year)]
        rate <-  target_rate/base_rate
        rate[[1]]

}

