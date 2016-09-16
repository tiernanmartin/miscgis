insertMakeDfObjectAddin <- function() {
        library(miniUI)
        library(shiny)
        ui <- miniPage(
                # includeHighlightJs(),
                gadgetTitleBar("Insert MakeShp() Code"),
                miniContentPanel(
                        stableColumnLayout(
                                textInput("name","Shape Name",value = "name"),
                                textInput("tbl_acs","Table Code",value = "tbl_acs")
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
                        name_df <- paste0(input$name,'_df')
                        name_acs <- paste0(input$name,'_acs')
                        tbl_acs <- input$tbl_acs

                        pasteTemplate <- function(){
                                paste0(
                                        name_df," <- {
                                        if(!exists('",name_df,"')){
                                        if (!file.exists('./2_inputs/",name_df,".csv')) {
                                        make_",name_df," <- function() {

                                        # LOAD THE ACS FILE -------------------------------------------------------

                                        if(!exists('",name_acs,"')) ",name_acs," <- read_rds('./2_inputs/2_intermediate/",name_acs,".rds')


                                        # CREATE A DATA FRAME

                                        df1 <-
                                        data.frame(
                                        geography(",name_acs,")['tract'],
                                        estimate(",name_acs,"),
                                        1.645*standard.error(",name_acs,")) %>%
                                        `colnames<-`(.,c('GEOID6','COL_PCT_EST','COL_PCT_MOE')) %>%
                                        mutate(UPPER = COL_PCT_EST + COL_PCT_MOE,
                                        LOWER = COL_PCT_EST - COL_PCT_MOE,
                                        UPPER = if_else(UPPER>1,1,UPPER),
                                        LOWER = if_else(LOWER<0,0,LOWER))



                                        # SAVING ----------------------------------------------------------

                                        write_rds(df1,'./2_inputs/2_intermediate/",name_df,".rds')
                                        write_csv(df1,'./2_inputs/2_intermediate/",name_df,".csv')

                                        # PLOTTING --------------------------------------------------------

                                        ",name_df," <- read_rds('./2_inputs/2_intermediate/",name_df,".rds')

                                        view_",name,"_dotplot <<- function(){
                                        gg <- ggplot(",name_df,", aes(x=reorder(GEOID6, COL_PCT_EST), y=COL_PCT_EST))
                                        gg <- gg + geom_linerange(aes(ymin = LOWER, ymax = UPPER), size = 1.5, alpha = .25)
                                        gg <- gg + geom_point_interactive(aes(tooltip = GEOID6),size = 3,shape = 21, color = 'white', fill = 'grey30')
                                        gg <- gg + scale_x_discrete(expand = c(0.01,0))
                                        gg <- gg + scale_y_continuous(labels = scales::percent)
                                        gg <- gg + coord_flip()
                                        gg <- gg + theme_minimal()
                                        gg <- gg + theme(panel.grid.major.y = element_blank(),
                                        panel.grid.minor.y = element_blank(),
                                        panel.grid.major.x = element_line(linetype = 3, color = 'grey30'),
                                        panel.grid.minor.x = element_blank(),
                                        axis.text.y = element_blank())
                                        gg <- gg + labs(x=NULL, y=NULL,
                                        title='",name_allcaps,"',
                                        subtitle='Description',
                                        caption='Source: ACS Table ",tbl_acs,", Five-Year Estimates (2010-2014)')
                                        gg
                                        }

                                        # view_",name,"_dotplot()

                                        view_",name,"_dotplot_int <<- function(){
                                        ggiraph(code = {print(view_",name,"_dotplot())}, width = .66,
                                        tooltip_extra_css = 'padding:2px;background:rgba(70,70,70,0.1);color:black;border-radius:2px 2px 2px 2px;',
                                        hover_css = 'fill:#1279BF;stroke:#1279BF;cursor:pointer;')
                                        }

                                        # view_",name,"_dotplot_int()


                                        # LAST LINE ----------------------------------------------------------

                                        ",name_df,"

                                        }

                                        ",name_df," <- make_",name_df,"()
                                        rm(make_",name_df,")
                                        ",name_df,"
                                        } else {
                                        make_",name_df," <- function() {

                                        # LOAD THE RDS FILE -------------------------------------------------------
                                        ",name_df," <- read_rds('./2_inputs/2_intermediate/",name_df,".rds')

                                        view_",name,"_dotplot <<- function(){
                                        gg <- ggplot(",name_df,", aes(x=reorder(GEOID6, COL_PCT_EST), y=COL_PCT_EST))
                                        gg <- gg + geom_linerange(aes(ymin = LOWER, ymax = UPPER), size = 1.5, alpha = .25)
                                        gg <- gg + geom_point_interactive(aes(tooltip = GEOID6),size = 3,shape = 21, color = 'white', fill = 'grey30')
                                        gg <- gg + scale_x_discrete(expand = c(0.01,0))
                                        gg <- gg + scale_y_continuous(labels = scales::percent)
                                        gg <- gg + coord_flip()
                                        gg <- gg + theme_minimal()
                                        gg <- gg + theme(panel.grid.major.y = element_blank(),
                                        panel.grid.minor.y = element_blank(),
                                        panel.grid.major.x = element_line(linetype = 3, color = 'grey30'),
                                        panel.grid.minor.x = element_blank(),
                                        axis.text.y = element_blank())
                                        gg <- gg + labs(x=NULL, y=NULL,
                                        title='",name_allcaps,"',
                                        subtitle='Description',
                                        caption='Source: ACS Table ",tbl_acs,", Five-Year Estimates (2010-2014)')
                                        gg
                                        }

                                        # view_",name,"_dotplot()

                                        view_",name,"_dotplot_int <<- function(){
                                        ggiraph(code = {print(view_",name,"_dotplot())}, width = .66,
                                        tooltip_extra_css = 'padding:2px;background:rgba(70,70,70,0.1);color:black;border-radius:2px 2px 2px 2px;',
                                        hover_css = 'fill:#1279BF;stroke:#1279BF;cursor:pointer;')
                                        }

                                        # view_",name,"_dotplot_int()

                                        # LAST LINE ----------------------------------------------------------

                                        ",name_df,"


                                        }
                                        ",name_df," <- make_",name_df,"()
                                        rm(make_",name_df,")
                                        ",name_df,"
                                        }
                                        }else ",name_df,"

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
