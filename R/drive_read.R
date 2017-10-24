#' @title Read a file from Google Drive.
#' @description Read a file from Google Drive.
#' @name drive_read
#' @param dr_id a Google Drive object id (use \code{\link[googledrive]{as_id}}).
#' @param read_fun a function to read the object into memory.
#' @param .unzip A logical scalar, does the file need to be decompressed?
#' @param folder_name If the target is \code{.zip} file, what is the name of the folder within the file.
#' @param ... Arguments to pass to the `read_fun` function.
#' @return an object, format depends on which `read_fun` is provided.
#' @import googledrive
#' @importFrom magrittr "%>%"
#' @export
drive_read <- function (dr_id, read_fun, .unzip = FALSE, folder_name = NULL, ...)
{

  d <- drive_get(id = dr_id)

  f <- tempfile()

  d %>% drive_download(path = f)

  tmp_dir <- tempdir()

  if (.unzip) {

    unzip(f, exdir = tmp_dir, overwrite = TRUE)

    fp <- paste(dirname(tmp_dir),basename(tmp_dir), folder_name, sep = "/")
  }
  else {
    fp <- f
  }
  read_fun(fp, ...)
}
