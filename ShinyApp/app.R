library(shiny)

APP_NAME <- "Entrainment Dashboard"
APP_VERSION <- "1.1.0"

source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shinyApp(ui = ui, server = server)
