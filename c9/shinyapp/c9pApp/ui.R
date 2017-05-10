#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Air Quality Data Plots"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        h3("Brief Redme"),
        tags$div(class="header", checked=NA,
                 tags$p("Ozone plots against Solar.R, Wind and Temp in each tab.
                        Plot data is based on chosen month. You can also show or
                        hide a linear model line to each plot."),
                 tags$a(href="https://github.com/fengliplatform/course9/tree/master/shinyapp/c9pApp", "Source Code on Github.")
        ),

        h3("Choose Month"),
        checkboxInput("checkMay", "May", value = TRUE),
        checkboxInput("checkJune", "June", value = TRUE),
        checkboxInput("checkJuly", "July", value = TRUE),
        checkboxInput("checkAugust", "August", value = TRUE),
        checkboxInput("checkSeptember", "September", value = TRUE),

        h3("Linear Regression Model"),
        checkboxInput("checkModel", "Show/Hide Model", value = TRUE)
    ),

    # Show a plot of the generated distribution
    #mainPanel(
    #   plotOutput("distPlot")
    #)
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("By Solar.R", br(), plotlyOutput("solarPlot")),
                    tabPanel("By Wind", br(), plotlyOutput("windPlot")),
                    tabPanel("By Temp", br(), plotlyOutput("tempPlot"))
        )
    )
  )

))
