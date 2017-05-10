#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)

data("airquality")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    solar.lm <- reactive({
        df <- generate.df()
        s.lm <- lm(Ozone~Solar.R, data=df)
        s.lm.pred <- predict(s.lm, newdata=df)
        df1 <- data.frame(x=df$Solar.R, y=s.lm.pred)
    })
    wind.lm <- reactive({
        df <- generate.df()
        w.lm <- lm(Ozone~Wind, data=df)
        w.lm.pred <- predict(w.lm, newdata=df)
        df1 <- data.frame(x=df$Wind, y=w.lm.pred)
    })
    temp.lm <- reactive({
        df <- generate.df()
        t.lm <- lm(Ozone~Temp, data=df)
        t.lm.pred <- predict(t.lm, newdata=df)
        df1 <- data.frame(x=df$Temp, y=t.lm.pred)

    })
    generate.df <- reactive({
        checked.month <- numeric
        if( input$checkMay) {
            checked.month <- c(checked.month, 5)
        }
        if( input$checkJune) {
            checked.month <- c(checked.month, 6)
        }
        if( input$checkJuly) {
            checked.month <- c(checked.month, 7)
        }
        if( input$checkAugust) {
            checked.month <- c(checked.month, 8)
        }
        if( input$checkSeptember) {
            checked.month <- c(checked.month, 9)
        }

        df <- airquality[airquality$Month %in% checked.month,]
    })


    output$solarPlot <- renderPlotly({
        df <- generate.df()
        df2 <- solar.lm()

        p <- plot_ly(df, x=~Solar.R, y=~Ozone,
                type = "scatter", mode = "markers",
                marker = list(size = 10, opacity = 0.5),
                showlegend = F)
        if (input$checkModel) {
            add_trace(p, x = df2$x, y = df2$y, mode = "lines")
        } else {
            p
        }

    })
    output$windPlot <- renderPlotly({
        df <- generate.df()
        df2 <- wind.lm()

        p <- plot_ly(df, x=~Wind, y=~Ozone,
                     type = "scatter", mode = "markers",
                     marker = list(size = 10, opacity = 0.5),
                     showlegend = F)
        if (input$checkModel) {
            add_trace(p, x = df2$x, y = df2$y, mode = "lines")
        } else {
            p
        }

    })
    output$tempPlot <- renderPlotly({
        df <- generate.df()
        df2 <- temp.lm()

        p <- plot_ly(df, x=~Temp, y=~Ozone,
                     type = "scatter", mode = "markers",
                     marker = list(size = 10, opacity = 0.5),
                     showlegend = F)
        if (input$checkModel) {
            add_trace(p, x = df2$x, y = df2$y, mode = "lines")
        } else {
            p
        }

    })

})
