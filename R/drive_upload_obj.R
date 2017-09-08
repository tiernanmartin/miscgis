#' Upload a object in the Working Environment to Google Drive.
#'
#'
#' @param x an object in the working environment.
#' @param filename Character, name of the file (including appropriate extension) that
#'   should appear on Google Drive.
#' @param write_fun a function to write the object into a temporary file
#' @param .zip logical, do you want to compress the file into a `.zip` file?
#' @param .share logical, do you want to share the file?
#' @param folder A `dribble` object indicating the Drive folder to upload the file to.
#'
#' @return `dribble` object
#' @export

drive_upload_obj <- function(x, filename, write_fun, .zip = FALSE, .share = TRUE, folder = NULL){

  tempdir <- tempdir()

  dest_file_orig <- paste(tempdir,filename,sep = "/")

  write_fun(x,dest_file_orig)

  f <- tools::file_path_sans_ext(filename)

  if(.zip){
    dest_file_zip <- paste(tempdir,f,sep = "/") %>% paste0(".zip")

    zip(dest_file_zip, dest_file_orig)

    dribble <- drive_upload(media = dest_file_zip,
                            path = folder,
                            name = paste0(f,".zip"))
  }else{
    dribble <- drive_upload(media = dest_file_orig,
                            path = folder,
                            name = filename)
  }

  if(.share){drive_share(dribble, role = "commenter",type = "anyone")}

  return(dribble)
}
