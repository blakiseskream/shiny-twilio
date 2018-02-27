#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(shiny)
library(twilio)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Twilio"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          textInput(inputId     = "sid",             label = "Add Twilio SID")
        , textInput(inputId     = "token",           label = "Add Twilio token")
        , numericInput(inputId  = "from_number",     label = "Twilio number from", value = 5555555555)
        , numericInput(inputId  = "to_number",       label = "Twilio number to", value = 5555555555)
        , textAreaInput(inputId = "message",         label = "Message")
        , actionButton(inputId  = "send_message",    label = "Send!")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         dataTableOutput("messages")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  messages <- eventReactive(input$send_message,{
      req(input$sid)
      req(input$token)
      req(input$from_number)
      req(input$to_number)
      req(input$message)
      
      
      Sys.setenv(TWILIO_SID = input$sid)
      Sys.setenv(TWILIO_TOKEN = input$token)
      print(paste0("Message about to send ", input$message))
      tw_send_message(to = as.character(input$to_number), from = as.character(input$from_number), body = input$message)
  
      return(tw_get_messages_list())
  })
  
  output$messages <- renderDataTable(messages())
}

# Run the application 
shinyApp(ui = ui, server = server)

