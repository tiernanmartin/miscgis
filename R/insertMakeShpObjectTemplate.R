#' An RStudio add-in for inserting spatial data loading code
#'
#' An add-in that inserts the code to build and/or load a spatial object in R.
#'
#' @name insertMakeShpObjectAddin
#' @import shiny miniUI
#' @export

insertMakeShpObjectAddin <- function() {
        library(miniUI)
        library(shiny)
        ui <- miniPage(
                # includeHighlightJs(),
                gadgetTitleBar("Insert MakeShp() Code"),
                miniContentPanel(
                        stableColumnLayout(
                                textInput("name","Shape Name",value = "name")
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

                        name <- input$name
                        name_allcaps <- substitute(name) %>% toupper()
                        name_sp <- paste0(input$name,'_sp')
                        name_df <- paste0(input$name,'_df')

                        pasteTemplate <- function(){
                                paste0(
                                        name_sp," <- {
                                        if(!exists('",name_sp,"')){
                                        if (!file.exists('./2_inputs/2_intermediate/",name_sp,".gpkg')) {
                                        make_",name_sp," <- function() {

                                        # JOIN DATAFRAME TO SP OBJECT -------------------------------------------------------

                                        ",name_sp," <- this_geography_sp

                                        ",name_sp,"@data %<>% left_join(",name_df,", by = 'GEOID6')

                                        # SAVING ----------------------------------------------------------

                                        writeOGR(obj = ",name_sp,",
                                        dsn = './2_inputs/2_intermediate/",name_sp,".gpkg',
                                        layer = '",name_sp,"',
                                        driver = 'GPKG',
                                        verbose = FALSE,
                                        overwrite_layer = TRUE)


                                        # PLOTTING ----------------------------------------------------------

                                        view_",name_sp," <<- function() {
                                        abbr <- ",name_sp,"

                                        myYlOrRd <- RColorBrewer::brewer.pal(9, 'YlOrRd')[2:7]
                                        max <- max(abbr@data$COL_NAME) %>% round_any(.1,
                                        ceiling)
                                        pal <- colorNumeric(palette = myYlOrRd, domain = c(0,max))
                                        # pal <- colorFactor(palette = 'Set2', domain =
                                        # as.factor(abbr@data$COL_NAME))

                                        myLflt() %>% addPolygons(data = abbr, smoothFactor = 0,
                                        color = col2hex('white'), weight = 1.5, opacity = 0.5,
                                        fillColor = ~pal(COL_NAME), fillOpacity = 0.75,
                                        popup = ~GEOID6) %>%
                                        addLegend(position = 'topright', title = '",name_allcaps,"',
                                        pal = pal, values = c(0,max), opacity = 0.75,
                                        labFormat = labelFormat(
                                        suffix = '%', between = ', ',
                                        transform = function(x) 100 * x
                                        ))

                                        # myLflt() %>% addPolygons(data = abbr, smoothFactor = 0,
                                        # color = col2hex('white'),weight = 1.5,opacity = .5,
                                        # fillColor = pal(abbr@data$COL_NAME),fillOpacity = .75) %>%
                                        # addLegend(position = 'topright', title = '",name_allcaps,"', pal
                                        # = pal, values = as.factor(abbr@data$COL_NAME), opacity = .75,
                                        # labFormat = labelFormat())

                                        }

                                        #LAST LINE ----------------------------------------------------------------

                                        ",name_sp,"

                                        }

                                        ",name_sp," <- make_",name_sp,"()
                                        rm(make_",name_sp,")
                                        ",name_sp,"
                                        } else {
                                        make_",name_sp," <- function() {

                                        ",name_sp," <- readOGR(dsn = './2_inputs/2_intermediate/",name_sp,".gpkg',
                                        layer = '",name_sp,"',verbose = FALSE,p4s = crs_proj@projargs)

                                        # PLOTTING ----------------------------------------------------------

                                        view_",name_sp," <<- function() {
                                        abbr <- ",name_sp,"

                                        myYlOrRd <- RColorBrewer::brewer.pal(9, 'YlOrRd')[2:7]
                                        max <- max(abbr@data$COL_NAME) %>% round_any(.1,
                                        ceiling)
                                        pal <- colorNumeric(palette = myYlOrRd, domain = c(0,max))
                                        # pal <- colorFactor(palette = 'Set2', domain =
                                        # as.factor(abbr@data$COL_NAME))

                                        myLflt() %>% addPolygons(data = abbr, smoothFactor = 0,
                                        color = col2hex('white'), weight = 1.5, opacity = 0.5,
                                        fillColor = ~pal(COL_NAME), fillOpacity = 0.75,
                                        popup = ~GEOID6) %>%
                                        addLegend(position = 'topright', title = '",name_allcaps,"',
                                        pal = pal, values = c(0,max), opacity = 0.75,
                                        labFormat = labelFormat(
                                        suffix = '%', between = ', ',
                                        transform = function(x) 100 * x
                                        ))

                                        # myLflt() %>% addPolygons(data = abbr, smoothFactor = 0,
                                        # color = col2hex('white'),weight = 1.5,opacity = .5,
                                        # fillColor = pal(abbr@data$COL_NAME),fillOpacity = .75) %>%
                                        # addLegend(position = 'topright', title = '",name_allcaps,"', pal
                                        # = pal, values = as.factor(abbr@data$COL_NAME), opacity = .75,
                                        # labFormat = labelFormat())

                                        }

                                        #LAST LINE ----------------------------------------------------------------

                                        ",name_sp,"


                                        }
                                        ",name_sp," <- make_",name_sp,"()
                                        rm(make_",name_sp,")
                                        ",name_sp,"
                                        }
                                        }else ",name_sp,"

                        }"
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
