#' @title If a Drive file doesn't exist, get it
#' @description A function that determines whether a file exists in Google Drive, and execture
#' @name make_or_read2
#' @param fp filepath
#' @param dr_id a Google Drive ID, S3 class \code{drive_id} (use \code{\link[googledrive]{as_id}}).
#' @param get_expr expression used to get the file from its original source
#' @param make_expr expression used to create the file
#' @param read_expr expression used to read the file
#' @param ... other objects to pass to the expressions
#' @return The object that is read or created is returned silently.
#' @import purrr
#' @import googledrive
#' @import magrittr
#' @import glue
#' @export
make_or_read2 <- function(fp = NA_character_, dr_id = NA_character_, get_expr = NULL, make_expr = NULL, read_expr = NULL, ...){

        safe_as_dribble <- safely(as_dribble)

        get_fun <- function(dr_id, get_expr){

                dr_id_is_na <- is.na(dr_id)

                dr_id_exists <- safe_as_dribble(dr_id) %>% pluck("error") %>% is_null()

                if(dr_id_is_na){
                        NULL
                }else if(dr_id_exists){
                                message(glue("The Drive file with id '{as_id(dr_id)}' already exists."))
                }else{
                        message(glue("The Drive file with id '{as_id(dr_id)}' does not exist - executing the `get_expr()` function."))
                                get_expr
                        }
        }

        get_fun(dr_id, get_expr)


        fun <- function(fp, make_expr, read_expr){
                if(!file.exists(fp)){
                        message(glue("Downloading and loading Drive file with id '{as_id(dr_id)}'."))
                        make_expr
                }else{
                        message(glue("Reading local file: '{fp}'."))
                                read_expr
                        }
        }

        result <- fun(fp, make_expr, read_expr)

        invisible(result)

}
