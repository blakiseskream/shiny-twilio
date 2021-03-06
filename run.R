#run.R

library(shiny)

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = paste0(getwd(), "/shiny-twilio"),
  host = '0.0.0.0',
  port = as.numeric(port)
)