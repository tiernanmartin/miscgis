insertMakeShpAddin <- function() {
        library(miniUI)
        library(shiny)
        ui <- miniPage(
                # includeHighlightJs(),
                gadgetTitleBar("Insert MakeShp() Code"),
                miniContentPanel(
                        stableColumnLayout(
                                textInput("shpName","Shape Name (camel case)",value = "shpName")
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


                        shpName <- input$shpName

                        pasteTemplate <- function(){
                                paste0(
                                        shpName," <- {
                                        if(!file.exists('./2_inputs/",shpName,".shp')){
                                        make_",shpName," <- function(){
                                        ...
                                        ",shpName," <- CHANGE_THIS
                                        writeOGR(obj = ",shpName,",
                                        dsn = './2_inputs/',
                                        layer = '",shpName,"',
                                        driver = 'ESRI Shapefile', overwrite_layer = TRUE)

                                        colnames(",shpName,"@data) %>% data_frame() %>% write_csv(path = './2_inputs/",shpName,"_cn.csv')

                                        view_",shpName," <<- function(){
                                        abbr <- ",shpName,"

                                        myYlOrRd <- RColorBrewer::brewer.pal(9,'YlOrRd')[2:7]
                                        max <- max(abbr@data$IND) %>% round_any(10,ceiling)
                                        pal <- colorNumeric(palette = myYlOrRd,domain = range(0:max))
                                        #pal <- colorFactor(palette = 'Set2', domain = as.factor(abbr@data$IND))

                                        myLflt() %>%
                                        addPolygons(data = abbr,
                                        smoothFactor = 0,
                                        color = col2hex('white'),weight = 1.5,opacity = .5,
                                        fillColor = pal(abbr@data$IND),fillOpacity = .75) %>%
                                        addLegend(position = 'topright',
                                        title = 'CHANGE_THIS',
                                        pal = pal,
                                        values = range(0:max),
                                        opacity = .75,
                                        labFormat = labelFormat())

                                        #myLflt() %>%
                                        #addPolygons(data = abbr,
                                        #smoothFactor = 0,
                                        #color = col2hex('white'),weight = 1.5,opacity = .5,
                                        #fillColor = pal(abbr@data$IND),fillOpacity = .75) %>%
                                        #addLegend(position = 'topright',
                                        #title = 'CHANGE_THIS',
                                        #pal = pal,
                                        #values = as.factor(abbr@data$IND),
                                        #opacity = .75,
                                        #labFormat = labelFormat())

                                        }
                                        ",
                                        shpName,"

                                        }

                                        ",
                                        shpName," <- make_",shpName,"()
                                        rm(make_",shpName,")
                                        ",
                                        shpName,"
                                        }
                                        else{
                                        make_",shpName," <- function(){
                                        ",
                                        shpName," <- readOGR(dsn = './2_inputs/',
                                        layer = '",shpName,"') %>%
                                        spTransform(CRSobj = crs_proj)
                                        cn <- read_csv('./2_inputs/",shpName,"_cn.csv') %>% unlist(use.names = FALSE)

                                        colnames(",shpName,"@data) <- cn
                                        view_",shpName," <<- function(){
                                        abbr <- ",shpName,"

                                        myYlOrRd <- RColorBrewer::brewer.pal(9,'YlOrRd')[2:7]
                                        max <- max(abbr@data$IND) %>% round_any(10,ceiling)
                                        pal <- colorNumeric(palette = myYlOrRd,domain = range(0:max))
                                        #pal <- colorFactor(palette = 'Set2', domain = as.factor(abbr@data$IND))

                                        myLflt() %>%
                                        addPolygons(data = abbr,
                                        smoothFactor = 0,
                                        color = col2hex('white'),weight = 1.5,opacity = .5,
                                        fillColor = pal(abbr@data$IND),fillOpacity = .75) %>%
                                        addLegend(position = 'topright',
                                        title = 'CHANGE_THIS',
                                        pal = pal,
                                        values = range(0:max),
                                        opacity = .75,
                                        labFormat = labelFormat())

                                        #myLflt() %>%
                                        #addPolygons(data = abbr,
                                        #smoothFactor = 0,
                                        #color = col2hex('white'),weight = 1.5,opacity = .5,
                                        #fillColor = pal(abbr@data$IND),fillOpacity = .75) %>%
                                        #addLegend(position = 'topright',
                                        #title = 'CHANGE_THIS',
                                        #pal = pal,
                                        #values = as.factor(abbr@data$IND),
                                        #opacity = .75,
                                        #labFormat = labelFormat())

                                        }
                                        ",
                                        shpName,"
                                        }
                                        ",shpName," <- make_",shpName,"()
                                        rm(make_",shpName,")
                                        ",
                                        shpName,"
                                        }
                        }
                                        "
                                )
}
                        template <- pasteTemplate()


                        # Build formatted document
                        formatted <- formatR::tidy_source(
                                text = template,
                                output = FALSE,
                                width.cutoff = 60,
                                indent = 2,
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
