#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(qdap)
library(dplyr)

# Define UI for application that draws a histogram
shinyUI <- (fluidPage(
    titlePanel("Word Prediction App"),
    sidebarLayout(
        sidebarPanel(
            tags$head(
                tags$style(HTML("
                                    #actionbutton1, #actionbutton2, #actionbutton3{
                                    color: #FFFFFF;
                                    font-weight: bold;
                                    background-color: #f15a42
                                    }
                                   ")) 
            ),
            tags$div(
                tags$link(rel = "stylesheet", type = "text/css", href = "idx.css"),
                h4("Enter Text"),
                tags$textarea(id = 'text', placeholder = 'Type here...', rows = 3, class='form-control',"")),
            h5("Select Next Word..."),
            htmlOutput("nextword",inline = T),
            htmlOutput("secondword",inline = T),
            htmlOutput("thirdword",inline = T),
            br(" "),
            h5(tableOutput('topwords')),
            h5(textOutput('time'))),
        
        mainPanel(
            tabsetPanel(
                id = "tabPanel",
                tabPanel("Word Cloud",
                    tags$div(
                        h2("Wordcloud", align = "Center",
                           style = "color: #ef3c23; font-weight: bold;")),
                    tags$div(
                        plotOutput('wordcloud')
                    )
                ),
                tabPanel("Guide",
                    tags$div(
                        h2("Predicting the next word"),
                        tags$p("This application predicts the next word based on the last word provided. The algorithm used in this application follows the same pattern from three sample compilations of English texts drawn from Blogs, News, and Twitter.  The data file can be downloaded here: https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"),
                        tags$hr(),
                        tags$div(
                        h4("How do I use the app?"),
                        tags$p("1. Enter a word (or a group of words) in the text box located at the upperleft of the screen OR select from the suggested words below the text box."),
                        tags$p("2. Wait for the list and wordcloud to generate results. The list displays the top 6 predicted words while the wordcloud displays the top 30. The bigger the text, the more likely it will preceed the last word entered in the text box. You may see different results with different word combinations!"),
                        tags$p("Note: The time elapsed to predict the next word is located at the bottom of the sidebar."),
                        tags$hr())
                )
            )),
            hr(),
            tags$span(style="color:black", 
                      tags$footer("Word Prediction App 2019", 
                                  align = "Center"))
        )
    )
))

# Define server logic required to draw a histogram

ngram_models <- readRDS("ngram_models.rds")
source("predict.R")

shinyServer <- function(input, output, session) {
    
    observe({
        
        predict_list <- reactive({
            inputText <- input$text
            predict_list <- predictText(inputText,ngram_models)
        })
        
        nextword <- reactive({
            inputText <- input$text
            predict_list <- predictText(inputText,ngram_models)
            nextword <- predict_list[[1]][1]
        })
        
        secondword <- reactive({
            inputText <- input$text
            predict_list <- predictText(inputText,ngram_models)
            secondword <- predict_list[[1]][2]
        })
        
        thirdword <- reactive({
            inputText <- input$text
            predict_list <- predictText(inputText,ngram_models)
            thirdword <- predict_list[[1]][3]
        })
        
        time <- reactive({
            inputText <- input$text
            predict_list <- predictText(inputText,ngram_models)
            time <- paste("Run Time: ", sprintf("%.2f", predict_list[[3]][1]), " S")
        })
        
        topwords <- reactive({
            inputText <- input$text
            predict_list <- predictText(inputText,ngram_models)
            lng <- length(predict_list[[1]])
            topwords <- data_frame("Top Words Predicted" = predict_list[[1]])
            if (lng > 6){
                lng = 6
            }
            topwords <- topwords[1:lng,]
        })
        
        output$nextword <- renderText(nextword()) 
        output$time <- renderText(time())
        output$topwords <- renderTable(topwords())
        
        output$nextword <- renderUI({
            actionButton("actionbutton1", label = nextword(), width = '32%')
        })
        output$secondword <- renderUI({
            actionButton("actionbutton2", label = secondword(), width = '32%')
        })
        output$thirdword <- renderUI({
            actionButton("actionbutton3", label = thirdword(), width = '32%')
        })
        
        observeEvent(input$actionbutton1, {
            if(input$actionbutton1 == 1){
                newtext <- paste(input$text, nextword())
                updateTextInput(session, "text", value=newtext)
            }
        })
        
        observeEvent(input$actionbutton2, {
            newtext <- paste(input$text, secondword())
            updateTextInput(session, "text", value=newtext)
        })
        
        observeEvent(input$actionbutton3, {
            newtext <- paste(input$text, thirdword())
            updateTextInput(session, "text", value=newtext)
        })
        
        wordcloud_rep <- repeatable(wordcloud)
        colbrew <- brewer.pal(8, "Set2")
        
        output$wordcloud <- renderPlot(
            wordcloud_rep(
                predict_list()[[1]],
                predict_list()[[2]],
                colors=colbrew[c(1:11)],
                max.words=30,
                scale=c(5,0.75),
                min.freq = 0
            )
        )
    })
}

# Run the application 
shinyApp(ui = shinyUI, server = shinyServer )
