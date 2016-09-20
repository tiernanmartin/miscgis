#' Prepend CSS Styling to a HTML Widget
#'
#' Prepends a string of CSS styles to an HTML widget
#'
#' Source: http://widgets1092.rssing.com/browser.php?indx=61900561&last=1&item=19
#'
#' @param hw An HTML widget
#' @param style A string of CSS style choices
#' @param addl_selector An additional selector to target a specific element
#' @name styleWidget
#' @import htmlwidgets htmltools
#' @export

styleWidget <- function(hw=NULL, style="", addl_selector="") {
        stopifnot(!is.null(hw), inherits(hw, "htmlwidget"))

        # use current id of htmlwidget if already specified
        elementId <- hw$elementId
        if(is.null(elementId)) {
                # borrow htmlwidgets unique id creator
                elementId <- sprintf(
                        'htmlwidget-%s',
                        htmlwidgets:::createWidgetId()
                )
                hw$elementId <- elementId
        }

        htmlwidgets::prependContent(
                hw,
                htmltools::tags$style(
                        sprintf(
                                "#%s %s {%s}",
                                elementId,
                                addl_selector,
                                style
                        )
                )
        )
}
