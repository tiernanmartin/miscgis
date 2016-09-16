insertMakeACSObjectAddin <- function() {
        library(miniUI)
        library(shiny)
        ui <- miniPage(
                # includeHighlightJs(),
                gadgetTitleBar("Insert MakeShp() Code"),
                miniContentPanel(
                        stableColumnLayout(
                                textInput("name","Shape Name",value = "name"),
                                textInput("tbl_acs","Table Code",value = "tbl_acs"),
                                textInput("geoset","Geo Set Name",value = "geoset")
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


                        name_acs <- paste0(input$name,'_acs')
                        tbl_acs <- input$tbl_acs
                        geoset <- input$geoset

                        pasteTemplate <- function(){
                                paste0(
                                        name_acs," <- {
                                        if(!exists('",name_acs,"')){
                                        if (!file.exists('./2_inputs/2_intermediate/",name_acs,".rds')) {
                                        make_",name_acs," <- function() {

                                        # DOWNLOAD ACS DATA -------------------------------------------------------

                                        tbl <- '",tbl_acs,"' # census table code

                                        acs.fetch(
                                        endyear = 2014,
                                        geography = ",geoset,",
                                        table.number = tbl
                                        )

                                        # PROCESSING -------------------------------------------------------

                                        # find the column codes
                                        # acs.fetch(endyear = 2014, geography = ",geoset,", table.number = tbl,col.names = 'pretty') %>%
                                        #         acs.colnames() %>%
                                        #         cbind(acs.colnames(col1)) %>% View


                                        ",name_acs,"<- REPLACE_THIS

                                        # SAVING ----------------------------------------------------------

                                        saveRDS(",name_acs,",file = './2_inputs/2_intermediate/",name_acs,".rds') #save an R data file

                                        # PLOTTING ----------------------------------------------------------

                                        # plot(",name_acs,")

                                        # LAST LINE ----------------------------------------------------------

                                        ",name_acs,"

                                        }
                                        ",name_acs," <- make_",name_acs,"()
                                        rm(make_",name_acs,")
                                        ",name_acs,"
                                        } else ",name_acs," <- read_rds(path = './2_inputs/2_intermediate/",name_acs,".rds')

                                        } else ",name_acs,"

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
