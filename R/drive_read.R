#' Read a file from Google Drive.
#'
#'
#' @param dribble a `dribble` object containing the googledrive information.
#' @param read_fun a function to read the object into memory.
#' @param .unzip A logical scalar, does the file need to be decompressed?
#' @param ... Arguments to pass to the `read_fun` function.
#'
#' @return an object, format depends on which `read_fun` is provided.
#' @export

drive_read <- function(dribble, read_fun, .unzip = FALSE, ...){

        # Download the file

        f <- tempfile()

        url <- paste0("https://docs.google.com/uc?export=download&id=",dribble[["id"]])

        download.file(url,f)

        # Unzip (if necessary)

        if(.unzip){

                tmp_dir <- tempdir()

                unzip(f,exdir = tmp_dir,overwrite = TRUE)

                name_without_zip <- str_replace_all(dribble[["name"]], ".zip","")

                tmp_dir_files <- list.files(tmp_dir)

                name_with_fileformat <- tmp_dir_files[str_detect(tmp_dir_files,name_without_zip) & !str_detect(tmp_dir_files,".zip")]

                fp <- paste(tmp_dir,name_with_fileformat, sep = "/")
        }else{
                fp <- f
        }

        # Read the file
        read_fun(fp,...)

}
