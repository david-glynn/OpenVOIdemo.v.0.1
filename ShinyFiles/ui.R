library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Demo"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "typeOfOutcome", label = "Type of outcome", 
                  choices = c("Benefit" = "benefit", 
                              "Harm" = "harm", 
                              "Net health effect (QALYs)" = "netHealth"),
                  selected = "Net health effect (QALYs)"), # benefit , harm, net health effect
      
      selectInput(inputId = "typeOfResearch", label = "Type of research", 
                  choices = c("RCT" = "RCT", 
                              "Feasibility study" = "feasibility", 
                              "Reconsideration of evidence" = "reconsider"),
                  selected = "RCT"),
      
      numericInput("numberOfTreatments", "How many treatments are being investigated?",
                   value = 2, min = 2, max = 4),
      
      numericInput("P_t1", "Probability of outcome with treatment 1",
                   value = 0.5, min = 0, max = 1, step = 0.05),
      
      textInput("nameOf_t1", "Name of treatment 1 (optional)", 
                value = "late PTP"),
      
      textInput("nameOf_t2", "Name of treatment 2 (optional)", 
                value = "early PTP"),
      
      selectInput("dist_t2", label = "Distribution of treatment 2", 
                  choices = c("Normal" = "norm", 
                              "Half Normal" = "halfNorm"),
                  selected = "Normal"),
      
      numericInput("incidence", "Incidence per annum",
                   value = 8800, min = 0, max = NA, step = 20),
      
      numericInput("timeInformation", "Time over which evidence would be valuable (years)",
                   value = 15, min = 0, max = NA, step = 0.1),
      
      numericInput("discountRate", "Discount rate (%)",
                   value = 3.5, min = 0, max = 100, step = 0.1),
      
      numericInput("MCD_t2", "MCD for treatment 2",
                   value = 0, min = NA, max = NA, step = 0.05,
                   width = '50%'),
      
      actionButton("runRCT", label = "Run calculation for RCT"),
      actionButton("runFeas", label = "Run calculation for feasibility trial"),
      actionButton("runRec", label = "Run calculation for reconsideration of evidence"),
      
      textInput("nameOfOutcome", "Name of outcome"),
      
      textInput("nameOf_t3", "Name of treatment 3 (optional)", 
                value = "treatment 3"),
      
      textInput("nameOf_t4", "Name of treatment 4 (optional)", 
                value = "treatment 4"),
      
      selectInput("dist_t3", label = "Distribution of treatment 3", 
                  choices = c("Normal" = "norm", 
                              "Half Normal" = "halfNorm"),
                  selected = "Normal"),
      
      selectInput("dist_t4", label = "Distribution of treatment 4", 
                  choices = c("Normal" = "norm", 
                              "Half Normal" = "halfNorm"),
                  selected = "Normal"),
      
      numericInput("mu_t2", "Mean log odds ratio for treatment 2",
                   value = 0, min = NA, max = NA, step = 0.05,
                   width = '50%'),
      
      numericInput("variance_t2", "Variance of log odds ratio for treatment 2",
                   value = 0.25, min = NA, max = NA, step = 0.05,
                   width = '50%'),
      
      numericInput("mu_t3", "Mean log odds ratio for treatment 3",
                   value = NA, min = NA, max = NA, step = 0.05,
                   width = '50%'),
      
      numericInput("variance_t3", "Variance of log odds ratio for treatment 3",
                   value = NA, min = NA, max = NA, step = 0.05,
                   width = '50%'),
      
      numericInput("mu_t4", "Mean log odds ratio for treatment 4",
                   value = NA, min = NA, max = NA, step = 0.05,
                   width = '50%'),
      
      numericInput("variance_t4", "Variance of log odds ratio for treatment 3",
                   value = NA, min = NA, max = NA, step = 0.05,
                   width = '50%'),
      
      selectInput("direction_t2", label = "Direction of distribution for treatment 2", 
                  choices = c("Always positive" = "alwaysPositive", 
                              "Always negative" = "alwaysNegative"),
                  selected = "alwaysPositive"),
      
      selectInput("direction_t3", label = "Direction of distribution for treatment 3", 
                  choices = c("Always positive" = "alwaysPositive", 
                              "Always negative" = "alwaysNegative"),
                  selected = "alwaysPositive"),
      
      selectInput("direction_t4", label = "Direction of distribution for treatment 3", 
                  choices = c("Always positive" = "alwaysPositive", 
                              "Always negative" = "alwaysNegative"),
                  selected = "alwaysPositive"),
      
      numericInput("utilisation_t1", "Utilisation of treatment 1 (%)",
                   value = 100, min = 0, max = 100, step = 0.1),
      
      numericInput("utilisation_t2", "Utilisation of treatment 2 (%)",
                   value = 0, min = 0, max = 100, step = 0.1),
      
      numericInput("utilisation_t3", "Utilisation of treatment 3 (%)",
                   value = 0, min = 0, max = 100, step = 0.1),
      
      numericInput("utilisation_t4", "Utilisation of treatment 4 (%)",
                   value = 0, min = 0, max = 100, step = 0.1),
      
      numericInput("MCD_t3", "MCD for treatment 3",
                   value = 0, min = NA, max = NA, step = 0.05,
                   width = '50%'),
      
      numericInput("MCD_t4", "MCD for treatment 4",
                   value = 0, min = NA, max = NA, step = 0.05,
                   width = '50%'),
      
      numericInput("costResearchFunder", "Cost of research to funder",
                   value = 2854000, min = 0, max = NA, step = 100),
      
      numericInput("durationOfResearch", "Expected duration of research (years)",
                   value = 5, min = 0, max = NA, step = 0.1),
      
      numericInput("MCsims", "Number of simulations",
                   value = 50000, min = 0, max = 10000000, step = 500),
      
      numericInput("costHealthSystem", "Costs of research imposed on health system",
                   value = NA, min = 0, max = NA, step = 100)
      
      
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      textOutput("nameOf_t1"),
      textOutput("nameOf_t2"), 
      textOutput("nameOf_t3"), # conditional - is this a problem?
      textOutput("nameOf_t4"),# conditional - is this a problem?
      # name of outcome
      textOutput("nameOfOutcome"), # conditional - is this a problem?
      
      plotOutput("histVOIYear"),
      
      textOutput("optimalTreatment" ) ,
      textOutput("probTreatment1isMax" ) ,
      textOutput("probTreatment2isMax" ) ,
      textOutput("probTreatment3isMax" ) ,
      textOutput("probTreatment4isMax" ) ,
      textOutput("popDuringResearch" ) ,
      textOutput("popAfterResearch" ) ,
      textOutput("PopTotal" ) ,
      textOutput("valueOfResearchPerYear" ),
      textOutput("valueOfImplementationPerYear" ) ,
      textOutput("Cell_A" ) ,
      textOutput("Cell_C" ) ,
      textOutput("Cell_D" ) ,
      textOutput("maxvalueOfImplementation" ) ,
      textOutput("maxvalueOfResearch" ) ,
      textOutput("healthOpportunityCostsOfResearch" ) ,
      textOutput("valueOfResearchWithCurrentImplementation" ) ,
      textOutput("valueOfResearchWithPerfectImplementation" ) ,
      textOutput("ICER_ResearchWithCurrentImplementation" ) ,
      textOutput("ICER_ResearchWithPerfectImplementation" ) ,
      textOutput("valuePer15KResearchSpend")
      
      
    )
  )
))