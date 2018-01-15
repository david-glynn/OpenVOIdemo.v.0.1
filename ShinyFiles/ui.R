
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
               
               ### High level inputs which control what is dipalyed to user
               
               selectInput(inputId = "typeOfEndpoint", label = "Type of primary endpoint", 
                           choices = c("Binary" = "binary", 
                                       "Continuous" = "continuous", 
                                       "Survival" = "survival"),
                           selected = "Binary"), 
               
               selectInput(inputId = "typeOfOutcome", label = "Type of outcome", 
                           choices = c("Benefit" = "benefit", 
                                       "Harm" = "harm", 
                                       "Net health effect (QALYs)" = "netHealth"),
                           selected = "Net health effect (QALYs)"), # benefit , harm, net health effect
               
               # display if: typeOfEndpoint == binary & typeOfOutcome == "netHealth"
               radioButtons(inputId = "tCostsDependOnEvent", label = "Do the treatment costs depend on the primary outcome?", 
                            choices = c("Yes" = "Yes", 
                                        "No" = "No"),
                            selected = "No"), 
               
               numericInput("numberOfTreatments", "How many treatments are being investigated?",
                            value = 2, min = 2, max = 4),
               
               selectInput(inputId = "typeOfResearch", label = "Type of research", 
                           choices = c("RCT" = "RCT", 
                                       "Feasibility study" = "feasibility", 
                                       "Reconsideration of evidence" = "reconsider"),
                           selected = "RCT"),
               
               ### High level inputs
               
               # display if: typeOfOutcome != netHealth
               # automatically display QALYs if typeOfOutcome == netHealth
               textInput("nameOfOutcome", "Name of outcome (e.g. heart attacks", 
                         value = "functional recovery"), 
               
               textInput("currencySymbol", "Currency used in analysis", 
                         value = "£"),
              
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
               
               ## treatment 1
               
               # Display if: typeOfOutcome == "netHealth" & tCostsDependOnEvent == FALSE
               numericInput("cost_t1", "Lifetime treatment costs associated with treatment 1",
                            value = 100, min = NA, max = NA, step = 10,
                            width = '50%'),
               
               
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
               
               # Display if: typeOfOutcome == "netHealth" & tCostsDependOnEvent == FALSE
               numericInput("cost_t2", "Lifetime treatment costs associated with treatment 2",
                            value = 100, min = NA, max = NA, step = 10,
                            width = '50%'),
               
               # Display if: typeOfOutcome == "netHealth"
               numericInput("k", "Opportunity cost of health expenditure (£)",
                            value = 15000, min = NA, max = NA, step = 500,
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
               
               # Display if: numberOfTreatments >= 3 & typeOfOutcome == "netHealth" & tCostsDependOnEvent == FALSE
               numericInput("cost_t3", "Lifetime treatment costs associated with treatment 3",
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
               
               # display if: numberOfTreatments >= 4 & dist_t2 == "norm" 
               # change text to mean of log hazard ratio if typeOfEndpoint == survival
               numericInput("mu_t4", "Mean log odds ratio for treatment 4",
                            value = 0, min = NA, max = NA, step = 0.05,
                            width = '50%'),
               
               # display if: numberOfTreatments >= 4 & dist_t2 == "norm" 
               # change text to variance of log hazard ratio if typeOfEndpoint == survival
               numericInput("variance_t4", "Variance of log odds ratio for treatment 4",
                            value = 0.25, min = NA, max = NA, step = 0.05,
                            width = '50%'),
               
               # display if: numberOfTreatments >= 4 & dist_t2 == "halfNorm" 
               selectInput("direction_t4", label = "Direction of distribution for treatment 4", 
                           choices = c("Always positive" = "alwaysPositive", 
                                       "Always negative" = "alwaysNegative"),
                           selected = "alwaysPositive"),
               
               # Display if: numberOfTreatments >= 4 & typeOfOutcome == "netHealth" & tCostsDependOnEvent == FALSE
               numericInput("cost_t4", "Lifetime treatment costs associated with treatment 4",
                            value = NA, min = NA, max = NA, step = 10,
                            width = '50%'),
               
               # Display if: numberOfTreatments >= 4
               numericInput("MCD_t4", "MCD for treatment 4",
                            value = 0, min = NA, max = NA, step = 0.05,
                            width = '50%'),
               
               
               
               ### Extra Binary endpoint inputs
               
               # display if: typeOfEndpoint == binary
               numericInput("P_t1", "Probability of outcome with treatment 1 (Baseline risk)",
                            value = 0.5, min = 0, max = 1, step = 0.05),
               
               # display if: typeOfEndpoint == binary & typeOfOutcome == "netHealth"
               numericInput("INBBinaryEvent", "Net health effect of binary event occuring (in QALYs)",
                            value = 2, min = NA, max = NA, step = 0.05),
               
               
               ### Extra Continuous endpoint inputs
               
               # display if: typeOfEndpoint == continuous & typeOfOutcome == "netHealth"
               numericInput("INBContinEvent", 
                            "Net health effect of unit increase in continuous outcome (in QALYs)",
                            value = 0.05, min = NA, max = NA, step = 0.05),
               
               
               ### Extra Survival endpoint inputs
               
               # use relative effect inputs above - interpret as describing distribiont of log hazard ratios
               
               selectInput(inputId = "survivalDist", label = "Type of survival distribution", 
                           choices = c("Exponential" = "exponential", 
                                       "Weibull" = "weibull"),
                           selected = "exponential"), 
               
               # display if: typeOfEndpoint == survival
               numericInput("scaleParameter_t1", "Scale parameter for treatment 1 (natural scale)",
                            value = 5, min = 0, max = NA, step = 1),
               
               # display if: typeOfEndpoint == survival & survivalDist == "weibull"
               numericInput("shapeParameter_t1", "Shape parameter for treatment 1 (natural scale)",
                            value = 1.1, min = 0, max = NA, step = 0.1),
             
               # BUG!! If this has NA value then the app crashes
               # display if: typeOfEndpoint == survival & typeOfOutcome == "netHealth"
               numericInput("INBSurvivalEndpoint", "Net health effect of survival endpoint (in QALYs)",
                            value = 0.5, min = NA, max = NA, step = 0.05)
               
               
               ### success or failure endpoint inputs
               
               # display if: typeOfEndpoint == successFail
               #numericInput("PSuccess_t1", "Probability of treatment 1 working",
              #              value = NA, min = 0, max = 1, step = 0.05),
               
               # display if: typeOfEndpoint == successFail
               #numericInput("PSuccess_t2", "Probability of treatment 2 working",
                #            value = NA, min = 0, max = 1, step = 0.05),
               
               # dispaly if typeOfEndpoint == successFail & 3 / 4 treatments
               # PSuccess_t3, PSuccess_t4
               
               # display if: typeOfEndpoint == successFail & typeOfOutcome == "netHealth"
               #numericInput("INBTreatmentSuccess", "Net health effect of treatment success (in QALYs)",
              #              value = NA, min = NA, max = NA, step = 0.05)
               
               
      ),
      tabPanel("Standard RCT", 
               actionButton("runRCT", label = "Run calculation for RCT"),
               
               numericInput("MCsims", "Number of simulations",
                            value = 50000, min = 0, max = 10000000, step = 500),
               
               numericInput("durationOfResearch", "Expected duration of research (years)",
                            value = 5, min = 0, max = NA, step = 0.1),
               
               numericInput("costResearchFunder", "Cost of research to funder",
                            value = 2854000, min = 0, max = NA, step = 100),
               
               # display if: typeOfOutcome == "netHealth"
               numericInput("costHealthSystem", "Costs of research imposed on health system",
                            value = 1000000, min = 0, max = NA, step = 100)
               
      ),
      tabPanel("Feasibility Trial", 
               actionButton("runFeas", label = "Run calculation for feasibility trial"),
               
               numericInput("MCsims", "Number of simulations",
                            value = 50000, min = 0, max = 10000000, step = 500),
               
               numericInput("durationOfResearchFeas", "Expected duration of feasibility research (years)",
                            value = 2, min = 0, max = NA, step = 0.1),
               
               numericInput("durationOfResearchDefinitive", "Expected duration of follow-up research (years)",
                            value = 5, min = 0, max = NA, step = 0.1),
               
               numericInput("ProbabilityOfDefinitiveResearch", "Liklihood of feasibility research leading to follow-up study",
                            value = 0.5, min = 0, max = 1, step = 0.05),
               
               numericInput("costResearchFunderFeas", "Costs of feasibility research to funder",
                            value = 1000000, min = 0, max = NA, step = 100),
               
               numericInput("costResearchFunderDefinitive", "Costs of follow-up research to funder",
                            value = 1000000, min = 0, max = NA, step = 100),
               
               # display if: typeOfOutcome == "netHealth"
               numericInput("costHealthSystemFeas", "Costs of feasibility research imposed on health system",
                            value = 1000000, min = 0, max = NA, step = 100),
               
               # display if: typeOfOutcome == "netHealth"
               numericInput("costHealthSystemDefinitive", "Costs of follow-up research imposed on health system",
                            value = 1000000, min = 0, max = NA, step = 100)
               
               
               
      ),
      tabPanel("Reconsider Evidence", 
               actionButton("runRec", label = "Run calculation for reconsideration of evidence")
               
      ))),
    tabPanel("Results", 
             # notes:
             # display text conditional on which type of analysis has been carried out
             # Note: useing conditionalPanel the conditions are written in JavaScript and '' must be used!!
             
          
             br(),
             # heading 0
             h4("Headline results and overview"),
             br(),
             
             # heading 1
             h4("Type of analysis"),
             # if it is an RCT: 
             conditionalPanel(condition = "input.typeOfResearch != 'feasibility'",
                              p("This proposal is for a randomised controlled trial (RCT).
                                In this type of study, individuals are randomised to different treatments and the outcomes are compared accross the groups.")),
             # if it is a feasibility study:
             conditionalPanel(condition = "input.typeOfResearch == 'feasibility'",
                              p("This proposal is for a feasibility study. 
                                There are challenges and uncertainties associated with running a full trial. 
                                Due to these uncertainties it is unclear whether a definitive trial is possible.
                                Research only impacts health in so far as it changes clinical practice. 
                                This feasibility trial is unlikely to generate enough evidence to justifying changing practice on its own. 
                                Therefore the impact of this feasibility trial on population health is through the potential future definitive trial . 
                                If the definitive trial is not possible the cost of funding it will not result in health benefit. 
                                Since the value of the feasibility trial depends on the definitive trial, information on the future definitive trial is required to value feasibility trial.")),
             br(),
             
             # heading 2
             h4("Value of implementing current evidence findings"),
             # table showing expected outcomes with each treatment
             tableOutput("tableEventsPerYear"),
             # text for general discussion about current information (common accross all models and endpoints?)
             textOutput("resultsCurrenInformation"),
             br(),
             
             
             # heading 3
             h4("Value of the proposed study"),
             # if feasibility study: (require this extra bit)
             conditionalPanel(condition = "input.typeOfResearch == 'feasibility'",
                              p("Understanding the value of a feasibility trial requires two steps.
                                First the value of the definitive trial must be estimated. 
                                Second, this value must be adjusted for the fact that the definitive trial may not take place."),
                              h4("Value of potential future trial")),
             tableOutput("tableProbabilityMax"),
             # text for discussion about value of research (common accross all models and endpoints?)
             textOutput("resultsValueOfResearch"),
             br(),
             
             
             
             
             
             # Display the paragraphs of text
             #conditionalPanel(condition = "input.typeOfResearch == 'RCT'",
             #                  p("RCT")),
             #conditionalPanel(condition = "input.typeOfResearch == 'feasibility'",
             #                  p("feasibility analsis")),
          
             #textOutput("T_headlineResults"),
             
             # inputs 
             textOutput("nameOf_t1"),
             textOutput("nameOf_t2"), 
             textOutput("nameOf_t3"), # conditional - is this a problem?
             textOutput("nameOf_t4"),# conditional - is this a problem?
             textOutput("nameOfOutcome"), # conditional - is this a problem?
             
             # outputs
             textOutput("optimalTreatment" ) ,
             textOutput("expectedOutcomesPerYearoptimalTreatment"),
             textOutput("implementationValueExists"),        # new output
             textOutput("uncertaintyInCurrentEvidenceExists"),
             #textOutput("probTreatment1isMax" ) ,
             #textOutput("probTreatment2isMax" ) ,
             #textOutput("probTreatment3isMax" ) ,
             #textOutput("probTreatment4isMax" ) ,
             textOutput("popDuringResearch" ) ,
             textOutput("popAfterResearch" ) ,
             textOutput("popTotal" ) ,
             plotOutput("histVOIYear"),
             textOutput("valueOfResearchPerYear" ),
             textOutput("valueOfImplementationPerYear" ) ,
             #textOutput("Cell_A" ) ,
             #textOutput("Cell_C" ) ,
             #textOutput("Cell_D" ) ,
             textOutput("maxvalueOfImplementation" ) ,
             textOutput("maxvalueOfResearch" ) ,
             textOutput("healthOpportunityCostsOfResearch" ) ,
             textOutput("valueOfResearchWithCurrentImplementation" ) ,
             textOutput("valueOfResearchWithPerfectImplementation" ) ,
             textOutput("ICER_ResearchWithCurrentImplementation" ) ,
             textOutput("ICER_ResearchWithPerfectImplementation" ) ,
             textOutput("valuePer15KResearchSpend"),
             textOutput("absoluteExpectedHealthOutcomesFromResearchProject")
             
    ),
    tabPanel("Report", "<code for report ui>"),
    tabPanel("About", "<code for about ui>")
  )
  
))
