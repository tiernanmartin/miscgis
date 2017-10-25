#' @title Read a file from Google Drive.
#' @description Read a file from Google Drive.
#' @name drive_read
#' @param dr_id a Google Drive object id (use \code{\link[googledrive]{as_id}}).
#' @param .tempfile logical, Whether to save the file to a tempfile
#' @param .tempdir logical, Whether to save the \code{.zip} file to a temp directory
#' @param path The file path to save the file to.
#' @param read_fun a function to read the object into memory.
#' @param target_name If the target is \code{.zip} file, what is the name of the folder or file within the compressed \code{.zip} file.
#' @param ... Arguments to pass to the `read_fun` function.
#' @return an object, format depends on which `read_fun` is provided.
#' @import googledrive
#' @importFrom magrittr "%>%"
#' @export
drive_read <- function (dr_id, .tempfile = TRUE, path = NULL, read_fun, ...)
{

  d <- drive_get(id = dr_id)

  f <- if(.tempfile){tempfile()}else{path}

  d %>% drive_download(path = f, overwrite = TRUE)

  result <- read_fun(f, ...)

  if(.tempfile){unlink(f, recursive = TRUE)}

  invisible(result)
}

#' @rdname drive_read
#' @export
drive_read_zip <- function (dr_id, .tempdir = TRUE, dir_path = NULL, read_fun, target_name, ...)
{

  d <- drive_get(id = dr_id)

  f <- tempfile()

  d %>% drive_download(path = f, overwrite = TRUE)

  dir <- if(.tempdir){tempdir()}else{dir_path}

  unzip(f, exdir = dir, overwrite = TRUE)

  fp <- paste(dirname(dir),basename(dir), target_name, sep = "/")

  result <-read_fun(fp, ...)

  unlink(f, recursive = TRUE);

  if(.tempdir){unlink(list.files(dir, recursive = TRUE), recursive = TRUE)}

  invisible(result)
}

