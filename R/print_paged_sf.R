#' Print sf objects with paged tables
#'
#' Print sf objects with paged tables
#'
#' @import sf rmarkdown
#' @param x A sf-class data object
#' @param n Number of records to show in the printed table
#' @name print_paged_sf
NULL

#' @rdname print_paged_sf
#' @export
print_paged_sf <- function (x, n = ifelse(options("max.print")[[1]] == 99999,
                                               10, options("max.print")[[1]]))
{
        # browser()
        nf = length(x) - 1
        app = paste("and", nf, ifelse(nf == 1, "field", "fields"))
        if (any(!is.na(st_agr(x))))
                app = paste0(app, "\n", "Attribute-geometry relationship: ",
                             summarize_agr(x))
        print(st_geometry(x), n = 0, what = "Simple feature collection with",
              append = app)
        if (n > 0) {
                y <- x
                if (nrow(y) > n) {
                        cat(paste("First", n, "features:\n"))
                        y <- x[1:n, , drop = FALSE]
                }
                rmarkdown::paged_table(y)
        }
        rmarkdown::paged_table(x)
        # invisible(x)
}
