#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Capstone Project - Words Prediction!"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        h3("Brief Redme"),
        tags$div(class="header", checked=NA,
                 tags$p("Input text in below text field and click \"Predict\" button or press \"Enter\", the mostlikely next word 
                        will be displayed at right panel. For example, input \"happy\", \"birthday\" will be
                        predicted. Input \"let us\", \"know\" will be predicted.")
        ),
        h3("Input Text (English)"),
       textInput("inputWords",
                 "Please input:"),
       submitButton("Predict")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       textOutput("value")
    )
  )
))
