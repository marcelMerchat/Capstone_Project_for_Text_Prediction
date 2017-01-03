#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output){
      
      gram1 <- read.csv("dictionary.csv",stringsAsFactors = FALSE)
      gram2 <- read.csv("gram2.csv",     stringsAsFactors = FALSE)
      gram3 <- read.csv("gram3.csv",     stringsAsFactors = FALSE)
      
      gram1 <<- gram1
      
      punct <- grepl("[[:punct:]]",gram1[,1])
      words_with_punctution <- gram1[punct==TRUE,1]
      mispelled_punctution <- gsub("[[:punct:]]+", "", words_with_punctution)
      # 
      # #########################################################################
      # 
      top400 <- as.character(head(gram1,400)[,1])
      short400 <- top400[nchar(top400)<5]

    x = "the"
    y = "a"
    z = "to"
    Words <- c("The","To","A")
    Frequency <- c(1,2,3)
    
    r <- data.frame(Words, Frequency)
 
        x <- reactive({ 
            get_choices(input$text)
              bestchoice1
        })
        
        y <- reactive({ 
            get_choices(input$text)
            bestchoice2
        })
        
        z <- reactive({ 
            get_choices(input$text)
            bestchoice3
        })
        
        r <- reactive({ 
              get_choices(input$text)
              choice_table
           })
        
        output$results_table  <- DT::renderDataTable({DT::datatable(r(), options = list(orderClasses = TRUE))})
        output$text1 <- renderText({x()})
        output$text2 <- renderText({y()})
        output$text3 <- renderText({z()})
})

