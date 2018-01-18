#' @title If a Drive file doesn't exist, get it
#' @description A function that determines whether a file exists in Google Drive, and execture
#' @name make_or_read2
#' @param fp A file path character string.
#' @param dr_id A Google Drive ID, S3 class \code{drive_id} (use \code{\link[googledrive]{as_id}}).
#' @param get_expr An expression used to get the file from its original source.
#' @param make_expr An expression used to create the file.
#' @param read_expr An expression used to read the file.
#' @param skip_get_expr Skip the \code{get_expr} step (default is \code{FALSE}).
#' @return The object that is read or created is returned silently.
#' @import purrr
#' @import googledrive
#' @import magrittr
#' @import glue
#' @export
make_or_read2 <- function (fp = NA_character_, dr_id = "", skip_get_expr = FALSE,
    get_expr = NULL, make_expr = NULL, read_expr = NULL)
{

  # browser()
    safe_as_dribble <- safely(as_dribble)

    dr_id_null <- is.null(dr_id)

    dr_id_error <- safe_as_dribble(dr_id) %>% pluck("result") %>%
        is_null()

    get_fun <- function(fp, dr_id, get_expr) {
        get_expr(fp)
    }
    fun <- function(fp, dr_id, make_expr, read_expr) {
        if (!file.exists(fp)) {
            message(glue("Downloading and loading Drive file with id '{as_id(dr_id)}'."))
            make_expr(fp, dr_id)
        }
        else {
            message(glue("Reading local file: '{fp}'."))
            read_expr(fp)
        }
    }
    if (dr_id_null) {
        stop("`dr_id` cannot be `NULL`.")
    }
    else if (dr_id_error & skip_get_expr) {
        stop("`skip_get = TRUE` but `as_dribble(dr_id)` returns an error.\nProvide a valid `dr_id` or set `skip_get = FALSE`.")
    }
    else if (skip_get_expr | (!dr_id_error)) {
        result <- fun(fp, dr_id, make_expr, read_expr)
        return(result)
    }
    else {
        new_dribble <- get_fun(fp, dr_id, get_expr)
        new_dr_id <- as_id(new_dribble)
        result <- fun(fp, dr_id = new_dr_id, make_expr, read_expr)
        return(result)
    }
}

