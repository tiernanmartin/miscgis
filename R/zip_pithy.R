#' @title Compress files into a 'zip' archive (without folder directory)
#' @description \code{zip_pithy} creates a new zip archive file without the target file(s)' folder directory.
#' @name zip_pithy
#' @param zipfile The zip file to create. If the file exists, \code{zip} overwrites it, but \code{zip_append} appends to it.
#' @param files List of file to add to the archive. Absolute path names will be added as absolute path names, relative path names stay relative in the archive.
#' @param recurse logical, Whether to add the contents of directories recursively.
#' @param compression_level A number between 1 and 9. 9 compresses best, but it also takes the longest.
#' @return The name of the created zip file, invisibly.
#' @import zip
#' @import purrr
#' @export
zip_pithy <- function(zipfile, files, recurse = TRUE, compression_level = 9){

        if(!dir.exists(dirname(zipfile))){stop("The zipfile filepath is invalid (it doesn't exist).")}

        old_wd <- getwd()

        setwd(dirname(zipfile))

        zip_fp <- basename(zipfile)

        files_fp <- map_chr(files, basename)

        zip(zip_fp, files_fp, recurse, compression_level)

        setwd(old_wd)
}

