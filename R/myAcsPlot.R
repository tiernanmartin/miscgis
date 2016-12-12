#' Ordered Dotplot with Error Bars
#'
#' An ordered dotplot with error bars and labels
#'
#' @param acs An \code{acs} object
#' @param sort_fun A function for extracting information from an \code{acs} object (e.g. \code{estimate}, \code{standard.error}), which will be used to sort the \code{acs} object.
#' @param col The column to display, as a numeric index or as a character string
#' @param ylab = A label for the y-axis (should indicated the units, as in 'People of Color (%)')
#' @param .pct = T/F, whether the units are a percentage or not
#' @import acs
#' @export

myAcsPlot <- function(acs,sort_fun,col = 1,ylab,.pct = TRUE){
        par(mar = c(1,4,1,1))

        acs_obj <- acsOrder(acs,FUN = sort_fun,sort_col = col,result_col = col)

        acs.colnames(acs_obj) <- col

        plot(acs_obj,ylab = ylab,yaxt = 'n',xaxt = 'n',bty = 'n')

        yticks_val <- scales::pretty_breaks(n=5)(estimate(acs[,col]))

        yticks <- scales::pretty_breaks(n=5)(estimate(acs[,col]))

        y_labs <- {if(.pct){100*yticks}else{scales::comma(yticks)}}

        axis(2, at=yticks_val,labels = y_labs)
}
