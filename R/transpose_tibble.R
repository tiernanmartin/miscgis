#' Transpose a \code{tibble}
#'
#' @param x a tibble
#' @name transpose_tibble
#' @importFrom tibble "as_tibble"
#' @export
transpose_tibble <- function(x){as_tibble(cbind(nms = names(x), t(x)))}


