#' Round a vector of numerics while preserving their sum
#'
#' @param x a vector of numeric values
#' @param digits the number of digits to round each value to
#'
#' @note This function comes from a \href{https://stackoverflow.com/a/35930285/5170276}{post on Stack Overflow}.
#' @export

smart_round <- function(x, digits = 0) {
        up <- 10 ^ digits
        x <- x * up
        y <- floor(x)
        indices <- tail(order(x-y), round(sum(x)) - sum(y))
        y[indices] <- y[indices] + 1
        y / up
}
