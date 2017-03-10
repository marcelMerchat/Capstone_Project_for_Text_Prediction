# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that predicts the next word.

fluidPage(theme = "bootstrap.css",

   #titlePanel("Predict the Next Word"),
   fluidRow(column(5, offset = 1,
                h1('Predict the Next Word'),
                tags$br(),
                h3('Enter Text'),
                h4('Enter the desired text in the box below and look for the 
                reponse on the right.'),
                
                tags$br(),
                #textInput(inputId="text", label = "Enter text here:"),
                textAreaInput(inputId="text", label = "Enter text here:", value = "", width = NULL, height = '100px',
                              cols = NULL, rows = NULL, placeholder = NULL, resize = NULL),
                tags$br(),
                
                tags$br(),
                h3('Coursera John Hopkins Data Science Program: Capstone Project'),
                h3('Marcel Merchat; December 31, 2016'),
                tags$br(),
                tags$br(),
                
                h3('Statistical Likelihood Analysis'),
                tags$div(HTML(" ")),
                p('$$\\begin{align}
                        N_1 &: \\text{The event that $N_1$ is the next word.}\\\\
                        N_2 &: \\text{The event that $N_1$ is not correct.}\\\\
                      \\psi &: \\text{The event that $\\psi$ is the entered text.}\\
                 \\end{align}$$'),
               
                tags$div(HTML(" ")),
                p('$$ \\text{Consider the conditional probability of the event
                            that $N_1$ is the desired next word if the preceeding phrase is $\\psi$.}\\\\ 
                      \\text{This conditional probability is expressed as
                        $P(N_1|\\psi)$. Bayes Theorem gives an equation governing
                        this problem.}\\
                  $$'),
                
                withMathJax(sprintf("$$P(N_1|\\psi) = \\frac{P(\\psi|N_1)  \\cdot  P(N_1)}{P(\\psi)}$$")),
            
                p('and the probability of the complement event.'),
            
                withMathJax(sprintf("$$P(N_2|\\psi) = \\frac{P(\\psi|N_2)  \\cdot  P(N_2)}{P(\\psi)}$$")),
                
                p('Dividing these equations we arrive at the likelihood ratio
                 on the right side of the following equation.'),
                
                withMathJax(sprintf("$$\\frac{P(N_1 |\\psi)}{P(N_2|\\psi)} 
                            = \\frac{P(\\psi | N_1) }{P(\\psi | N_2)} \\cdot \\frac {P(N_1)}{P(N_2)}$$")),
                
                tags$div(HTML(" ")),
                p('$$\\begin{align}
                  \\text{The basic probabilities for $P(N_1)$ and $P(N_2)$ are not in general equal.}\\
                  \\end{align}$$'),
                
                tags$br(),
                h4('(Modified from the original.)')
            ),
            column(4, offset = 1,
                h1('Prediction:'),
                tags$br(),
                h4('First Choice'),
                h4(verbatimTextOutput('text1')),
                h4('Second Choice:'),
                h4(verbatimTextOutput('text2')),
                h4('Third Choice:'),
                verbatimTextOutput('text3'),
                
               # tags$textarea(id = "myText", rows = 22, cols = 60, "")
                
                tags$br(),
                tags$br(),
                
                h1('Statistical Data'),
                tags$br(),
                dataTableOutput('results_table')
            )
    
    )            
     
  )


