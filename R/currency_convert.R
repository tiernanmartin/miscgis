#' An currency conversion function
#'
#' A one-to-one spatial overlay function, similar to sp::over()
#'
#' @param x A dollar amount
#' @param target_year The year to convert the dollar value to
#' @param base_year The year to convert the dollar value to
#' @name currency_convert
#' @import acs
#' @export

currency_convert <- function(x,target_year,base_year){
        .env = environment()
        data("cpi", envir = .env)
        target_rate <-  cpi[as.character(target_year)]
        base_rate <-  cpi[as.character(base_year)]
        rate <-  target_rate/base_rate
        x*rate[[1]]

}

