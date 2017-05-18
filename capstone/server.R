#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(stringi)

trigram.df <- readRDS(file="./data/trigram.RDS")
bigram.df <- readRDS(file="./data/bigram.RDS")
suppressMessages(attach(trigram.df))
suppressMessages(attach(bigram.df))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    wordPredict <- reactive({
        text2 <- input$inputWords
        
        # input text cleaning
        cleanText <- tolower(text2)
        cleanText <- removePunctuation(cleanText)
        cleanText <- removeNumbers(cleanText)
        cleanText <- str_replace_all(cleanText, "[^[:alnum:]]", " ")
        cleanText <- stripWhitespace(cleanText)
        
        if (!is.null(cleanText)) {
            text.length <- stri_count(cleanText,regex="\\S+")
            
            if (text.length > 2) {
                #text <- "NA"
                text.new <- unlist(strsplit(cleanText, split=" "))
                text.length2 <- length(text.new)
                text.last.two <- paste(text.new[text.length2-1], text.new[text.length2], sep=" ")
                text.last <- text.new[text.length2]
                
                result <- subset(trigram.df, X==text.last.two, select="Y")
                result2 <- as.character(result[1,])
                if(is.na(result2)) {
                    result <- subset(bigram.df, X==text.last, select="Y")
                    result2 <- as.character(result[1,])
                } else {
                    result2
                }
            } else if (text.length == 2) {
                result <- subset(trigram.df, X==cleanText, select="Y")
                result2 <- as.character(result[1,])
            } else if (text.length == 1) {
                result <- subset(bigram.df, X==cleanText, select="Y")
                result2 <- as.character(result[1,])
            } else {
                text <- "NA"
            }
        } else {
            text <- "NA"
        }
    })
    
  output$value <- renderText({ 
      text <- wordPredict()
  })
  
})
