insertYCCindTemplate <- function() {
        library(miniUI)
        library(shiny)
        ui <- miniPage(
                # includeHighlightJs(),
                gadgetTitleBar("Insert MakeShp() Code"),
                miniContentPanel(
                        stableColumnLayout(
                                textInput("indName","Indicator Name (camel case)",value = "indName"),
                                textInput("abbr","Abbr. version of the object name",value = "abbr"),
                                textInput("ACStableNum","ACS Table Number",value = "ACStableNum")
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


                        indName <- input$indName
                        abbr <- input$abbr
                        ACStableNum <- input$ACStableNum

                        pasteTemplate <- function(){
                                        paste0(
                                                indName," <- {
                                                if (!file.exists('./2_inputs/",indName,".csv')) {
                                                make_",indName," <- function() {

                                                # DOWNLOAD ACS DATA -------------------------------------------------------

                                                ",abbr,"1 <- acs.fetch(endyear = 2014,geography = ACS_yccGeos,table.number = '",ACStableNum,"')

                                                # colNameGuide <- acs.fetch(endyear = 2014,
                                                #                           geography = geo.make(us = T),
                                                #                           table.number = '",ACStableNum,"',
                                                #                           col.names = 'pretty')

                                                # colNameGuide %>%
                                                #         .@estimate %>%
                                                #         as.data.frame() %>%
                                                #         colnames() %>%
                                                #         rbind(colnames(as.data.frame(",abbr,"1@estimate))) %>%
                                                #         as.data.frame()

                                                # SEATTLE: COUNTY SUBDIVISION AND PLACE -----------------------------------

                                                ",abbr,"_sea <-
                                                ",abbr,"1[1:2] %>%
                                                .@estimate %>%
                                                as.data.frame() %>%
                                                mutate_each(funs(as.numeric),everything()) %>%
                                                ... %>%
                                                mutate(GEO = c('Seattle CCD','Seattle')) %>%
                                                select(GEO,everything())

                                                # SEATTLE: TRACTS ---------------------------------------------------------

                                                sea_tr_list <-
                                                read_csv('./2_inputs/DEC_10_SF1_H1_with_ann.csv',skip = 1) %>%
                                                as.data.frame() %>%
                                                mutate(TRACTCE = substr(Id,start = 20,stop = 25)) %>%
                                                select(TRACTCE) %>%
                                                unlist(use.names = FALSE)

                                                ",abbr,"_sea_tr1 <-
                                                ",abbr,"1[",abbr,"1@geography$tract %in% sea_tr_list]

                                                ",abbr,"_sea_tr2 <-
                                                cbind(",abbr,"_sea_tr1@estimate,",abbr,"_sea_tr1@geography$tract) %>%
                                                as.data.frame() %>%
                                                rename(GEO = V22) %>%
                                                mutate_each(funs(as.numeric),-contains('GEO')) %>%
                                                ...%>%
                                                select(GEO,everything())

                                                # YCC: URBAN VILLAGES (AGGREGATED TRACTS) ---------------------------------

                                                # Subset the Seattle data to include only YCC tracts
                                                ycc1 <-
                                                ",abbr,"_sea_tr2 %>%
                                                filter(GEO %in% tract_ycc_arb@data$TRACTCE)

                                                # Join the UV names
                                                UVs <- tract_ycc_arb@data %>% select(GEO = TRACTCE,UV)

                                                ycc2 <- ycc1 %>%
                                                left_join(UVs)


                                                # Group by UV (and calculate the percentages, if applicable)
                                                uv1 <- ycc2 %>%
                                                group_by(UV) %>%
                                                ... %>%
                                                select(GEO = UV,everything())

                                                # EXPORT THE DATAFRAME
                                                ",indName,"  <-
                                                bind_rows(",abbr,"_sea,",abbr,"_sea_tr2,uv1)
                                                ",indName," %>%
                                                write_csv(path = './2_inputs/",indName,".csv')

                                                ",indName,"

                                                }

                                                ",indName," <- make_",indName,"()
                                                rm(make_",indName,")
                                                ",indName,"
                                                } else {
                                                make_",indName," <- function() {

                                                ",indName," <- read_csv('./2_inputs/",indName,".csv')
                                                }
                                                ",indName," <- make_",indName,"()
                                                rm(make_",indName,")
                                                ",indName,"
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
