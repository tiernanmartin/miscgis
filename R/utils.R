#' A set of generic helpful functions
#'
#' General purpose functions to make working in R easier.
#'
#' @name utils
#' @export

mk_proj_dir <- function(){
        inputs <- paste0("./1_inputs/",
                         c("1_raw",
                           "2_intermediate",
                           "3_tidy"))
        lapply(inputs,dir.create,showWarnings = FALSE,recursive = TRUE)

        dir.create(path = "./2_analysis")

        comms <- paste0("./3_communication/",
                        c("html",
                          "images",
                          "msword",
                          "other",
                          "pdf",
                          "shiny",
                          "sp"
                        ))

        lapply(comms,dir.create,showWarnings = FALSE,recursive = TRUE)

        return(NULL)
}


