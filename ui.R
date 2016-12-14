#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that predicts the next word.

fluidPage(

   #titlePanel("Predict the Next Word"),
   fluidRow(column(5, offset = 1,
            
                   h1('Predict the Next Word'),
                   
                   tags$br()
         
                   )),
   fluidRow(column(4, offset = 1,
                h2('Enter Text'),
                tags$br(),
                tags$br(),
                h4('Enter the desired text in the box below and look for the 
                reponse on the right.'),
                
                tags$br(),
                textInput(inputId="text", label = "Enter text here:"),
                tags$br()
            ),
            column(4, offset = 1,
                h2('Prediction:'),
                tags$br(),
                h3('First Choice'),
                h3(verbatimTextOutput('text1')),
                h3('Second Choice:'),
                h3(verbatimTextOutput('text2')),
                h3('Third Choice:'),
                verbatimTextOutput('text3')
            )
    ),
                   
        fluidRow(column(4, offset = 1,
   
            h2('Capstone Project'),
            h3('Coursera John Hopkins Data Science Program'),
            tags$br(),
            h4('Prepared by Marcel Merchat October 8, 2016')

        ),
        column(5, offset = 1,
               dataTableOutput('results_table')
        )
        )
  )


