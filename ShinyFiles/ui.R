
library(shiny)

shinyUI(fluidPage(
  titlePanel("Open VOI - Demo"),
  
  tabsetPanel(
    tabPanel("About", 
             br(),
             h3("What this app does"),
             p("This is an R Shiny App which allows easy and quick value of information calculations to
               estimate the value of research proposals. The approach translates the uncertainty in the primary outcome into health consequences. If a decision is uncertain there is a chance that the optimal treatment will not be chosen. The value of research is the value of avoiding sub optimal decisions."), 
             p("Full details of the approach used and applied examples using these methods are forthcoming. In the meantime see https://www.york.ac.uk/che/research/teehta/research-prioritisation/ for further details.")
             
             ),
    tabPanel("Inputs", tabsetPanel(
      tabPanel("General Inputs", 
               
               ### High level inputs
               
               selectInput(inputId = "typeOfEndpoint", label = "Type of primary endpoint", 
                           choices = c("Binary" = "binary", 
                                       "Continuous" = "continuous", 
                                       "Survival" = "survival",
                                       "Treatment success" = "successFail"),
                           selected = "Binary"), 
               
               selectInput(inputId = "typeOfOutcome", label = "Type of outcome", 
                           choices = c("Benefit" = "benefit", 
                                       "Harm" = "harm", 
                                       "Net health effect (QALYs)" = "netHealth"),
                           selected = "Net health effect (QALYs)"), # benefit , harm, net health effect
               
               # automatically display QALYs if typeOfOutcome == netHealth
               textInput("nameOfOutcome", "Name of outcome"), 
               
               selectInput(inputId = "typeOfResearch", label = "Type of research", 
                           choices = c("RCT" = "RCT", 
                                       "Feasibility study" = "feasibility", 
                                       "Reconsideration of evidence" = "reconsider"),
                           selected = "RCT"),
               
               numericInput("numberOfTreatments", "How many treatments are being investigated?",
                            value = 2, min = 2, max = 4),
               
               textInput("nameOf_t1", "Name of treatment 1 (optional)", 
                         value = "late PTP"),
               
               textInput("nameOf_t2", "Name of treatment 2 (optional)", 
                         value = "early PTP"),
               
               # display if: numberOfTreatments >= 3
               textInput("nameOf_t3", "Name of treatment 3 (optional)", 
                         value = "treatment 3"),
               
               # display if: numberOfTreatments >= 4
               textInput("nameOf_t4", "Name of treatment 4 (optional)", 
                         value = "treatment 4"),
               
               
               
               ### High level research inputs
               
               numericInput("costResearchFunder", "Cost of research to funder",
                            value = 2854000, min = 0, max = NA, step = 100),
               
               numericInput("incidence", "Incidence per annum",
                            value = 8800, min = 0, max = NA, step = 20),
               
               numericInput("timeInformation", "Time over which evidence would be valuable (years)",
                            value = 15, min = 0, max = NA, step = 0.1),
               
               numericInput("discountRate", "Discount rate (%)",
                            value = 3.5, min = 0, max = 100, step = 0.1),
               
               
               ### High level treatment inputs 
               
               numericInput("utilisation_t1", "Utilisation of treatment 1 (%)",
                            value = 100, min = 0, max = 100, step = 0.1),
               
               numericInput("utilisation_t2", "Utilisation of treatment 2 (%)",
                            value = 0, min = 0, max = 100, step = 0.1),
               
               # display if: numberOfTreatments >= 3
               numericInput("utilisation_t3", "Utilisation of treatment 3 (%)",
                            value = 0, min = 0, max = 100, step = 0.1),
               
               # display if: numberOfTreatments >= 4
               numericInput("utilisation_t4", "Utilisation of treatment 4 (%)",
                            value = 0, min = 0, max = 100, step = 0.1),
               
               
               ### Low level treatment inputs (in the order t2, t3, t4)
               # if typeOfEndpoint == survival rename inputs as log hazard ratios , not log odds ratios
               
               ## treatment 2
               
               # display if: typeOfEndpoint != successFail
               selectInput("dist_t2", label = "Distribution of treatment 2", 
                           choices = c("Normal" = "norm", 
                                       "Half Normal" = "halfNorm"),
                           selected = "Normal"),
               
               # display if: dist_t2 == "norm" & typeOfEndpoint != successFail
               numericInput("mu_t2", "Mean log odds ratio for treatment 2",
                            value = 0, min = NA, max = NA, step = 0.05,
                            width = '50%'),
               
               # display if: dist_t2 == "norm" & typeOfEndpoint != successFail
               numericInput("variance_t2", "Variance of log odds ratio for treatment 2",
                            value = 0.25, min = NA, max = NA, step = 0.05,
                            width = '50%'),
               
               # display if: dist_t2 == "halfNorm" & typeOfEndpoint != successFail
               selectInput("direction_t2", label = "Direction of distribution for treatment 2", 
                           choices = c("Always positive" = "alwaysPositive", 
                                       "Always negative" = "alwaysNegative"),
                           selected = "alwaysPositive"),
               
               # Display if: typeOfOutcome == "netHealth"
               numericInput("cost_t2", "Additional costs of treatment 2",
                            value = 100, min = NA, max = NA, step = 10,
                            width = '50%'),
               
               # Display if: typeOfOutcome == "netHealth"
               numericInput("k", "Opportunity cost of health expenditure (£)",
                            value = 13000, min = NA, max = NA, step = 500,
                            width = '50%'),
               
               numericInput("MCD_t2", "MCD for treatment 2",
                            value = 0, min = NA, max = NA, step = 0.05,
                            width = '50%'),
               
               
               ## treatment 3
               
               # display if: numberOfTreatments >= 3 & typeOfEndpoint != successFail 
               selectInput("dist_t3", label = "Distribution of treatment 3", 
                           choices = c("Normal" = "norm", 
                                       "Half Normal" = "halfNorm"),
                           selected = "Normal"),
               
               # display if: numberOfTreatments >= 3 & dist_t2 == "norm" & typeOfEndpoint != successFail
               numericInput("mu_t3", "Mean log odds ratio for treatment 3",
                            value = 0, min = NA, max = NA, step = 0.05,
                            width = '50%'),
               
               # display if: numberOfTreatments >= 3 & dist_t2 == "norm" & typeOfEndpoint != successFail
               numericInput("variance_t3", "Variance of log odds ratio for treatment 3",
                            value = 0.25, min = NA, max = NA, step = 0.05,
                            width = '50%'),
               
               # display if: numberOfTreatments >= 3 & dist_t2 == "halfNorm" & typeOfEndpoint != successFail
               selectInput("direction_t3", label = "Direction of distribution for treatment 3", 
                           choices = c("Always positive" = "alwaysPositive", 
                                       "Always negative" = "alwaysNegative"),
                           selected = "alwaysPositive"),
               
               # Display if: numberOfTreatments >= 3 & typeOfOutcome == "netHealth"
               numericInput("cost_t3", "Additional costs of treatment 3",
                            value = NA, min = NA, max = NA, step = 10,
                            width = '50%'),
               
               # Display if: numberOfTreatments >= 3
               numericInput("MCD_t3", "MCD for treatment 3",
                            value = 0, min = NA, max = NA, step = 0.05,
                            width = '50%'),
               
               
               ## treatment 4
               
               # display if: numberOfTreatments >= 4 & typeOfEndpoint != successFail 
               selectInput("dist_t4", label = "Distribution of treatment 4", 
                           choices = c("Normal" = "norm", 
                                       "Half Normal" = "halfNorm"),
                           selected = "Normal"),
               
               # display if: numberOfTreatments >= 4 & dist_t2 == "norm" & typeOfEndpoint != successFail
               numericInput("mu_t4", "Mean log odds ratio for treatment 4",
                            value = 0, min = NA, max = NA, step = 0.05,
                            width = '50%'),
               
               # display if: numberOfTreatments >= 4 & dist_t2 == "norm" & typeOfEndpoint != successFail
               numericInput("variance_t4", "Variance of log odds ratio for treatment 4",
                            value = 0.25, min = NA, max = NA, step = 0.05,
                            width = '50%'),
               
               # display if: numberOfTreatments >= 4 & dist_t2 == "halfNorm" & typeOfEndpoint != successFail
               selectInput("direction_t4", label = "Direction of distribution for treatment 4", 
                           choices = c("Always positive" = "alwaysPositive", 
                                       "Always negative" = "alwaysNegative"),
                           selected = "alwaysPositive"),
               
               # Display if: numberOfTreatments >= 4 & typeOfOutcome == "netHealth"
               numericInput("cost_t4", "Additional costs of treatment 4",
                            value = NA, min = NA, max = NA, step = 10,
                            width = '50%'),
               
               # Display if: numberOfTreatments >= 4
               numericInput("MCD_t4", "MCD for treatment 4",
                            value = 0, min = NA, max = NA, step = 0.05,
                            width = '50%'),
               
               
               
               ### Binary endpoint inputs
               
               # display if: typeOfEndpoint == binary
               numericInput("P_t1", "Probability of outcome with treatment 1",
                            value = 0.5, min = 0, max = 1, step = 0.05),
               
               # display if: typeOfEndpoint == binary & typeOfOutcome == "netHealth"
               numericInput("INBBinaryEvent", "Net health effect of binary event occuring (in QALYs)",
                            value = 2, min = NA, max = NA, step = 0.05),
               
               
               
               ### Continuous endpoint inputs
               
               # display if: typeOfEndpoint == continuous & typeOfOutcome == "netHealth"
               numericInput("INBContinEvent", 
                            "Net health effect of unit increase in continuous outcome (in QALYs)",
                            value = NA, min = NA, max = NA, step = 0.05),
               
               
               ### Survival endpoint inputs
               
               # display if: typeOfEndpoint == survival
               # see notes in notebook
               numericInput("LogHazard_t1", "Log hazard of event with treatment 1",
                            value = NA, min = 0, max = 1, step = 0.05),
               
               # use relative effect inputs above - interpret as describing distribiont of log hazard ratios
               
               # display if: typeOfEndpoint == survival & typeOfOutcome == "netHealth"
               numericInput("INBSurvivalEndpoint", "Net health effect of survival endpoint (in QALYs)",
                            value = NA, min = NA, max = NA, step = 0.05),
               
               
               ### success or failure endpoint inputs
               
               # display if: typeOfEndpoint == successFail
               numericInput("PSuccess_t1", "Probability of treatment 1 working",
                            value = NA, min = 0, max = 1, step = 0.05),
               
               # display if: typeOfEndpoint == successFail
               numericInput("PSuccess_t2", "Probability of treatment 2 working",
                            value = NA, min = 0, max = 1, step = 0.05),
               
               # dispaly if typeOfEndpoint == successFail & 3 / 4 treatments
               # PSuccess_t3, PSuccess_t4
               
               # display if: typeOfEndpoint == successFail & typeOfOutcome == "netHealth"
               numericInput("INBTreatmentSuccess", "Net health effect of treatment success (in QALYs)",
                            value = NA, min = NA, max = NA, step = 0.05)
               
               
      ),
      tabPanel("Standard RCT", 
               actionButton("runRCT", label = "Run calculation for RCT"),
               numericInput("durationOfResearch", "Expected duration of research (years)",
                            value = 5, min = 0, max = NA, step = 0.1),
               
               numericInput("MCsims", "Number of simulations",
                            value = 50000, min = 0, max = 10000000, step = 500),
               
               numericInput("costHealthSystem", "Costs of research imposed on health system",
                            value = 1000000, min = 0, max = NA, step = 100)
               
      ),
      tabPanel("Feasibility", 
               actionButton("runFeas", label = "Run calculation for feasibility trial")
               
      ),
      tabPanel("Reconsider Evidence", 
               actionButton("runRec", label = "Run calculation for reconsideration of evidence")
               
      ))),
    tabPanel("Results", 
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
             
    ),
    tabPanel("Report", "<code for report ui>"),
    tabPanel("About", "<code for about ui>")
  )
  
))
