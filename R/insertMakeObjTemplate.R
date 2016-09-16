insertMakeObjTemplate <- function() {
        library(miniUI)
        library(shiny)
        ui <- miniPage(
                # includeHighlightJs(),
                gadgetTitleBar("Insert MakeObj() Code"),
                miniContentPanel(
                        stableColumnLayout(
                                textInput("obj_name","Object Name",value = "obj_name")
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


                        obj_name <- input$obj_name

                        pasteTemplate <- function(){
                                paste0(
                                        obj_name," <- {
                                        if(!exists('",obj_name,"')){
                                        if(!file.exists('./2_inputs/",obj_name,".rds')){
                                        make_",obj_name," <- function(){
                                        ...
                                        ",obj_name," <- CHANGE_THIS
                                        # save the object
                                        ",obj_name," %>% saveRDS(file = './2_inputs/",obj_name,".rds')

                                        ",obj_name,"

                                        }
                                        ",
                                        obj_name," <- make_",obj_name,"()
                                        rm(make_",obj_name,")
                                        ",
                                        obj_name,"
                                        }
                                        else{
                                        make_",obj_name," <- function(){
                                        ",
                                        obj_name," <- readRDS('./2_inputs/",obj_name,".rds')

                                        }

                                        ",obj_name," <- make_",obj_name,"()
                                        rm(make_",obj_name,")
                                        ",
                                        obj_name,"
                                        }
                                        }else ",obj_name,"
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
