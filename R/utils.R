#' @title A set of generic helpful functions
#' @description General purpose functions to make working in R easier.
#' @import dplyr operator.tools
#' @name utils
#' @import purrr
NULL

#' @rdname utils
#' @export
not_sfc <- function(x) !any(class(x) %in% 'sfc')


#' @rdname utils
#' @export
maybe_make <- function(fp, expr){

  fun <- function(fp, expr){
  if(!file.exists(fp)){expr}
  }

  fun_possibly <- purrr::possibly(.f = fun, otherwise = NULL)

  result <- fun_possibly(fp,expr)

  invisible(result)

}

#' @rdname utils
#' @export
first_not_na <- function(x){
        if(all(sapply(x,is.na))){
                as(NA,class(x))
                }else{
                x[!sapply(x,is.na)][1]
        }


}

#' @rdname utils
#' @export
first_not_null <- function(x){
        if(all(sapply(x,is.null))){
                as(NA,class(x))
        }else{
                x[!sapply(x,is.null)][1]
        }
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
        nm <- c(...)
        if(!is.list(nm)){nm <- list(...)}
        nm <- lapply(nm, as.matrix)
        n <- max(sapply(nm, nrow))
        do.call(cbind, lapply(nm, function(x) rbind(x, matrix(nrow = n - nrow(x), ncol = ncol(x))))) %>% as.data.frame()
}

#' @rdname utils
#' @export
mk_proj_dir <- function(){



        # Create directories

        inputs <- paste0("./1-data/", c("1-raw",
                                        "2-external",
                                        "3-interim",
                                        "4-ready"))

        lapply(inputs, dir.create, showWarnings = FALSE, recursive = TRUE)

        comms <- paste0("./2-communication/", c("1-rnotebooks", "1-rnotebooks/archive",
                                                "1-rnotebooks/tmp", "2-bookdown", "3-rmarkdown", "4-shinyapps",
                                                "others", "others/images", "others/msword", "others/pdf",
                                                "others/spatial"))

        lapply(comms, dir.create, showWarnings = FALSE, recursive = TRUE)

        dir.create(path = "./3-resources")

        dir.create(path = "./proj-setup-scripts")

        # Create .gitignore and .gitkeep files

        gitignore_cmd <- paste('echo "*\n!.gitignore" >',inputs,"/.gitignore",sep = "")

        input_gitkeep_cmd <- paste('touch ',inputs,"/.gitkeep",sep = "")

        comms_gitkeep_cmd <- paste('touch ',comms,"/.gitkeep",sep = "")

        lapply(gitignore_cmd,system)

        lapply(input_gitkeep_cmd,system)

        lapply(comms_gitkeep_cmd,system)

        system("touch ./3-resources/.gitkeep")

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
