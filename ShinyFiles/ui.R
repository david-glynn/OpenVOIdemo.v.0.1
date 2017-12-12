#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Demo"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30),
       selectInput(inputId = "typeOfOutcome", label = "Type of outcome", 
                   choices = c("Benefit", "Harm", "Net health effect (QALYs)")), # benefit , harm, net health effect
       
       # how many treatments are being investigated
       numericInput("numberOfTreatments", "How many treatments are being investigated?",
                    value = 2, min = 2, max = 4),
       
       # probabiilty of event with baseline treatment, right name??
       numericInput("P_t0", "Baseline probability of outcome event",
                    value = NA, min = 0, max = 1, step = 0.05),
       
       # name of first two treatments 
       textInput("nameOf_t0", "Name of baseline treatment (optional)", 
                 value = "current treatment"),
       textInput("nameOf_t1", "Name of baseline treatment (optional)", 
                 value = "new treatment 1"),
       
       # distribution of new treatments (will be either normal or half normal)
       # note it is possible to name the variables for use in the app vs what the user sees
       # could possibly get this label to update conditional on nameOf_t1
       selectInput("dist_t1", label = "Distribution of new treatment 1", 
                   choices = c("Normal", "Half Normal")),
       
       # maybe think about initial value - set it to blank to force the user to change it
       numericInput("incidence", "Incidence per annum",
                    value = 8800, min = 0, max = NA),
       
       # allow for different currencies??
       numericInput("costResearchFunder", "Cost of research to funder",
                    value = 2854000, min = 0, max = NA),
       
       numericInput("durationOfResearch", "Expected duration of research (years)",
                    value = 5, min = 0, max = NA),
       
       numericInput("timeInformation", "Time over which evidence would be valuable (years)",
                    value = 15, min = 0, max = NA),
       
       # NOTE!! this is in percent***need to divide by 100
       numericInput("discountRate", "Discount rate (%)",
                    value = 3.5, min = 0, max = 100),
       
       # need to check what is a sensible maximum! what will crash R!
       # suggested 50K
       numericInput("MCsims", "Number of simulations",
                    value = 50000, min = 0, max = 10000000),
       
       numericInput("MCD_t1", "Minimum clinical difference (MCD)",
                    value = 0, min = NA, max = NA, step = 0.05,
                    width = '50%'),
       actionButton("runRCT", label = "Run calculation for RCT"),
       actionButton("runFeas", label = "Run calculation for feasibility trial"),
       actionButton("runRec", label = "Run calculation for reconsideration of evidence")
       
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("analysis"),
      textOutput("ICER"),
      plotOutput("distPlot"),
       textOutput("nameOf_t0"),
       textOutput("nameOf_t1"),
       textOutput("nameOf_t2"), # conditional - is this a problem?
       textOutput("nameOf_t3"), # conditional - is this a problem?
       # name of outcome
       textOutput("nameOfOutcome") # conditional - is this a problem?
       
       
       
       
       
       
    )
  )
))
