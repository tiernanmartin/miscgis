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

        rm(inputs)
        rm(comms)

        return(NULL)
}

injectHighlightHandler <- function() {

        code <- "
        Shiny.addCustomMessageHandler('highlight-code', function(message) {
        var id = message['id'];
        setTimeout(function() {
        var el = document.getElementById(id);
        hljs.highlightBlock(el);
        }, 100);
        });
        "

        tags$script(code)
}

includeHighlightJs <- function() {
        resources <- system.file("www/shared/highlight", package = "shiny")
        list(
                includeScript(file.path(resources, "highlight.pack.js")),
                includeCSS(file.path(resources, "rstudio.css")),
                injectHighlightHandler()
        )
}

highlightCode <- function(session, id) {
        session$sendCustomMessage("highlight-code", list(id = id))
}

rCodeContainer <- function(...) {
        code <- HTML(as.character(tags$code(class = "language-r", ...)))
        div(pre(code))
}

renderCode <- function(expr, env = parent.frame(), quoted = FALSE) {
        func <- NULL
        installExprFunction(expr, "func", env, quoted)
        markRenderFunction(textOutput, function() {
                paste(func(), collapse = "\n")
        })
}

stableColumnLayout <- function(...) {
        dots <- list(...)
        n <- length(dots)
        width <- 12 / n
        class <- sprintf("col-xs-%s col-md-%s", width, width)
        fluidRow(
                lapply(dots, function(el) {
                        div(class = class, el)
                })
        )
}

isErrorMessage <- function(object) {
        inherits(object, "error_message")
}

errorMessage <- function(type, message) {
        structure(
                list(type = type, message = message),
                class = "error_message"
        )
}
