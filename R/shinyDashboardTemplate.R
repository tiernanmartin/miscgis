# SETUP -------------------------------------------------------------------------------------------

library(shiny)
library(shinydashboard)

# UI ---------
# +--- Header ---------
header <- dashboardHeader()
# +--- Sidebar ---------
sidebar <- dashboardSidebar()
# +--- Body ---------
body <- dashboardBody()
# +--- Create UI ---------
ui <- dashboardPage(header, sidebar, body)

# SERVER ---------
server <- function(input, output) {

}

# RUN ---------
shinyApp(ui, server)
