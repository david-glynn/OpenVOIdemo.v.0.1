#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  output$nameOf_t0 <- renderText({input$nameOf_t0})
  output$nameOf_t1 <- renderText({input$nameOf_t1})
  
  # conditional - is this a problem?
  output$nameOf_t2 <- renderText({input$nameOf_t2})
  output$nameOf_t3 <- renderText({input$nameOf_t3})
  
  # conditional - is this a problem?
  output$nameOfOutcome <- renderText({input$nameOfOutcome})
  
  
  VOIResults <- reactiveValues()
  
  observeEvent(input$runRCT, {
    if(input$typeOfOutcome == "Benefit"){
      resultsHolder <- reactive({
        list(analysis = "RCTBenefit",
             ICER = mean(rnorm(10000, input$P_t0, 0.2)))
        })
      VOIResults$analysis <-  resultsHolder()$analysis
      VOIResults$ICER <-  resultsHolder()$ICER
      
    }else{
      resultsHolder <- reactive({
        list(analysis = "RCTnotBenefit",
             ICER = mean(rnorm(10000, input$P_t0, 0.2)))
      })
      VOIResults$analysis <-  resultsHolder()$analysis
      VOIResults$ICER <-  resultsHolder()$ICER
      
    }
    })
  
  observeEvent(input$runFeas, {VOIResults <- list ( analysis = "Feas", ICER = 20)})
  
  
  output$analysis <- renderText({VOIResults$analysis})
  output$ICER <- renderText({VOIResults$ICER})
  
  
  
})
