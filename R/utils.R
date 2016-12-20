#' A set of generic helpful functions
#'
#' General purpose functions to make working in R easier.
#'
#' @import dplyr operator.tools
#' @name utils
NULL

#' @rdname utils
#' @export
first_not_na <- function(x){

        x[!is.na(x)][1]

}

#' @rdname utils
#' @export
replace_empty <- function(x){
        ifelse(nchar(x)==0,NA,x)
}

#' @rdname utils
#' @export
subset_duplicated <- function(x,nm,notin = FALSE){
        if(notin){
                subset(x, x[[nm]] %!in% x[[nm]][duplicated(x[[nm]])])
        }else{
                subset(x, x[[nm]] %in% x[[nm]][duplicated(x[[nm]])])
        }
        }

#' @rdname utils
#' @export
cbind_fill <- function(...){
        nm <- list(...)
        nm <- lapply(nm, as.matrix)
        n <- max(sapply(nm, nrow))
        do.call(cbind, lapply(nm, function (x)
                rbind(x, matrix(nrow = n-nrow(x), ncol = ncol(x))))) %>% as.data.frame()
}

#' @rdname utils
#' @export
mk_proj_dir <- function(){
        inputs <- paste0("./1-data/", c("1-notebooks",
                                        "2-raw",
                                        "3-external",
                                        "3-external/manual",
                                        "4-interim",
                                        "5-tidy"))
        lapply(inputs, dir.create, showWarnings = FALSE, recursive = TRUE)
        dir.create(path = "./2-analysis")
        comms <- paste0("./3-communication/", c("1-bookdown", "2-shinyapps","other"))
        lapply(comms, dir.create, showWarnings = FALSE, recursive = TRUE)
        dir.create(path = "./proj-setup-scripts")
        return(NULL)
}

#' @rdname utils
#' @export
is_pct <- function(x){
        if(!is.double(x)){FALSE}else if(max(x,na.rm = TRUE)<=1 & min(x, na.rm = TRUE) >=0){TRUE}else{FALSE}
        }

#' @rdname utils
#' @export
label_pct <- function(breaks){
        b <- breaks
        b_max <- max(b) %>% scales::percent()
        b_others <- b[(!(b %in% max(b)))]*100 %>% as.integer()
        b_final <- c(b_others,b_max)
        return(b_final)
}

#' @rdname utils
#' @export
label_yr <- function(breaks){
        b <- breaks
        b_min <- min(b)
        b_others <- b[(!(b %in% b_min))] %>% str_sub(3,4) %>% paste0("'",.)
        b_final <- c(b_min,b_others)
        return(b_final)
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
