insertYCCmapTemplate <- function() {
        library(miniUI)
        library(shiny)
        ui <- miniPage(
                # includeHighlightJs(),
                gadgetTitleBar("Insert MakeShp() Code"),
                miniContentPanel(
                        stableColumnLayout(
                                radioButtons(inputId = "type",label = "Map data type",choices = c("numeric","factor"),selected = "numeric"),
                                radioButtons(inputId = "geo",label = "Geography type",choices = c("tr","ycc"),selected = "tr"),
                                textInput("indName","Indicator Name (camel case)",value = "indName"),
                                selectizeInput(inputId = "colName",label = "Col. name from 'acsData'",choices = colnames(acsData))
                                # checkboxInput("brace.newline", "Place left braces '{' on a new line?", FALSE),
                                # numericInput("indent", "Indent size: ", 2),
                                # numericInput("width", "Column width: ", 60),
                        ),
                        uiOutput("document", container = rCodeContainer)
                )
        )

        server <- function(input, output, session) {

                # Get the document context.
                context <- rstudioapi::getActiveDocumentContext()


                reactiveDocument <- reactive({

                        type <- input$type
                        indName <- input$indName
                        geo <- input$geo
                        colName <- input$colName

                        pasteTemplate <- function(){
                                        if(type == "numeric"){
                                                paste0(
                                                        "view_",indName,"_sea_",geo,"_",colName," <- function() {
                                                        tr <- YCCBA_sea_",geo,"

                                                        myYlOrRd <- RColorBrewer::brewer.pal(9, 'YlOrRd')[2:7]
                                                        max <- max(tr@data$",colName,") %>% round_any(10, ceiling)
                                                        pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))

                                                        myLflt() %>% addPolygons(data = tr, smoothFactor = 0,
                                                        color = col2hex('white'), weight = 1.5, opacity = 0.5,
                                                        fillColor = pal(tr@data$",colName,"), fillOpacity = 0.75) %>%
                                                        addLegend(position = 'topright', title = 'LEGEND',
                                                        pal = pal, values = range(0:max), opacity = 0.75,
                                                        labFormat = labelFormat(suffix = '%'))

                                        }
                                                        "
                                                )
                                        } else {
                                                paste0(
                                                        "view_",indName,"_sea_",geo,"_",colName," <- function() {
                                                        tr <- YCCBA_sea_",geo,"

                                                        myYlOrRd <- RColorBrewer::brewer.pal(9, 'YlOrRd')[2:7]
                                                        pal <- colorFactor(palette = 'Set2', domain = as.factor(tr@data$",colName,"))


                                                        myLflt() %>%
                                                        addPolygons(data = tr, smoothFactor = 0,
                                                        color = col2hex('white'),weight = 1.5,opacity = .5,
                                                        fillColor = pal(tr@data$",colName,"),fillOpacity = .75) %>%
                                                        addLegend(position = 'topright', title = 'LEGEND', pal
                                                        = pal, values = as.factor(tr@data$",colName,"), opacity = .75,
                                                        labFormat = labelFormat())

                                        }
                                                        "
                                                )
                                        }
                        }

                        template <- pasteTemplate()


                        # Build formatted document
                        formatted <- formatR::tidy_source(
                                text = template,
                                output = FALSE,
                                width.cutoff = 100,
                                indent = 4,
                                brace.newline = FALSE
                        )$text.tidy

                        formatted
})

                output$document <- renderCode({
                        document <- reactiveDocument()
                        # highlightCode(session, "document")
                        document
                })

                observeEvent(input$done, {
                        contents <- paste(reactiveDocument(), collapse = "\n")
                        rstudioapi::insertText(contents, id = context$id)
                        # rstudioapi::setDocumentContents(contents, id = context$id)
                        invisible(stopApp())
                })

                }

        viewer <- paneViewer()
        runGadget(ui, server, viewer = viewer)

        }
