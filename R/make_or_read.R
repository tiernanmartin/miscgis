#' @title Read or create a file
#' @description A function that determines whether to read or create a file based on whether that file currently exists.
#' @name make_or_read
#' @param fp filepath
#' @param make_expr expression used to create the file
#' @param read_expr expression used to read the file
#' @return The object that is read or created is returned silently.
#' @import purrr
#' @export
make_or_read <- function(fp, make_expr, read_expr){

  fun <- function(){
  if(!file.exists(fp)){make_expr}else{read_expr}
  }

  result <- fun()

  invisible(result)

}
