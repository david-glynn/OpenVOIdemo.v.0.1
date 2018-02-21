############################
# demo!
#########################

library(shiny)

shinyUI(fluidPage(
  titlePanel("OVID - Open Value of Information Device"),
  
  tabsetPanel(
    
    ##################
    # HOW TO ESTIMATE tab
    ##################
    tabPanel("Welcome"
            
             ), # end Welcome tabPanel
    
    
    ##################
    # HOW TO ESTIMATE tab
    ##################
    tabPanel("How to estimate research value" 
             
             ), # end how to estimate tabPanel
    
    ##################
    # HOW TO use this app tab
    ##################
    tabPanel("Inputs and how to use this app"
             
            ), # end how to use this app tabPanel
    
    ##################
    # INPUTs page tab
    ##################
    tabPanel("Inputs", 
             
             br(),
             fluidPage(
               
               # top fluid row (non treatment inputs)
               fluidRow(
                 column(3, 
                        ##########
                        # Primary outcome measure inputs
                        ##########
                        wellPanel(
                          h4("Primary outcome measure"),
                          p("This information determines the inputs required for the analysis and so this section",
                            strong("should be completed first.")),
                          
                          selectInput(inputId = "typeOfEndpoint", label = "Type of primary endpoint", 
                                      choices = c("Binary" = "binary", 
                                                  "Continuous" = "continuous", 
                                                  "Survival" = "survival"),
                                      selected = "Binary"), 
                          
                          # new
                          selectInput(inputId = "outcomeExpression", label = "Express results in natural outcomes or QALYs?", 
                                      choices = c("Natural outcomes" = "natural", 
                                                  "QALYs" = "QALYs"),
                                      selected = "QALYs"), # benefit , harm, net health effect
                          
                          
                          selectInput(inputId = "typeOfOutcome", label = "Type of outcome", 
                                      choices = c("Benefit" = "benefit", 
                                                  "Harm" = "harm", 
                                                  "Net health effect (QALYs)" = "netHealth"),
                                      selected = "Net health effect (QALYs)"), # benefit , harm, net health effect
                          
                          
                          numericInput("numberOfTreatments", "How many treatments are being investigated?",
                                       value = 2, min = 2, max = 4),
                          
                          selectInput(inputId = "typeOfResearch", label = "Type of research", 
                                      choices = c("RCT" = "RCT", 
                                                  "Feasibility study" = "feasibility"),
                                      selected = "RCT"),
                          
                          conditionalPanel(condition = "input.typeOfResearch == 'RCT'",
                                           radioButtons(inputId = "reconsider", label = "Calculate the value of reconsidering the evidence?", 
                                                        choices = c("Yes" = "Yes", 
                                                                    "No" = "No"),
                                                        selected = "No"),
                                           p("Note: the calculation to reconsider the evidence can take between ## and ## minutes to report.")),
                          
                          conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.typeOfOutcome == 'netHealth'",
                                           radioButtons(inputId = "tCostsDependOnEvent", label = "Do the treatment costs depend on the primary outcome?", 
                                                        choices = c("Yes" = "Yes", 
                                                                    "No" = "No"),
                                                        selected = "No")),
                          
                          conditionalPanel(condition = "input.typeOfEndpoint == 'survival'",
                                           selectInput(inputId = "survivalDist", label = "Type of survival distribution", 
                                                       choices = c("Exponential" = "exponential", 
                                                                   "Weibull" = "weibull"),
                                                       selected = "exponential"))
                          
                          
                        ) # end wellPanel
                        
                 ), # end decision problem inut column
                 
                 column(3, 
                        ##########
                        # General inputs (1st panel): health system
                        ##########
                        
                        wellPanel(
                          
                          h4("Health system"),
                          
                          numericInput("incidence", "Incidence per annum",
                                       value = 8800, min = 0, max = NA, step = 20),
                          
                          numericInput("timeInformation", "Time over which evidence would be valuable (years)",
                                       value = 15, min = 0, max = NA, step = 0.1),
                          
                          numericInput("discountRate", "Discount rate (%)",
                                       value = 3.5, min = 0, max = 100, step = 0.1),
                          
                          conditionalPanel(condition = "input.typeOfOutcome == 'netHealth'",
                                           numericInput("k", "Opportunity cost of health system expenditure (£)",
                                                        value = 15000, min = 0, max = NA, step = 500)),
                          
                          conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.typeOfOutcome == 'netHealth'",
                                           numericInput("INBBinaryEvent", "Net health effect of binary event occuring (in QALYs)",
                                                        value = 2, min = NA, max = NA, step = 0.05)),
                          
                          conditionalPanel(condition = "input.typeOfEndpoint == 'continuous' && input.typeOfOutcome == 'netHealth'",
                                           numericInput("INBContinEvent", 
                                                        "Net health effect of unit increase in continuous outcome (in QALYs)",
                                                        value = 0.05, min = NA, max = NA, step = 0.05)),
                          
                          # BUG!! If this has NA value then the app crashes
                          conditionalPanel(condition = "input.typeOfEndpoint == 'survival' && input.typeOfOutcome == 'netHealth'",
                                           numericInput("INBSurvivalEndpoint", "Net health effect of survival endpoint (in QALYs)",
                                                        value = 0.5, min = NA, max = NA, step = 0.05)),
                          
                          # make conditional? display if: typeOfOutcome != netHealth. automatically display QALYs if typeOfOutcome == netHealth
                          textInput("nameOfOutcome", "Name of outcome (e.g. heart attacks, QALY)", 
                                    value = "functional recovery"), 
                          
                          textInput("currencySymbol", "Currency used in analysis", 
                                    value = "£")
                          
                        ) # end of wellPanel 
                 ), # end of second column
                 
                 column(3,
                        ##########
                        # General inputs (2st panel): trial design
                        ##########
                        
                        wellPanel(
                          
                          h4("Trial"),
                          
                          # RCT trial design inputs
                          conditionalPanel(condition = "input.typeOfResearch == 'RCT'",
                                           
                                           numericInput("durationOfResearch", "Expected duration of research (years)",
                                                        value = 5, min = 0, max = NA, step = 0.1),
                                           
                                           numericInput("costResearchFunder", "Cost of research to funder",
                                                        value = 2854000, min = 0, max = NA, step = 100),
                                           
                                           conditionalPanel(condition = "input.typeOfOutcome == 'netHealth'",
                                                            numericInput("costHealthSystem", "Costs of research imposed on health system",
                                                                         value = 1000000, min = 0, max = NA, step = 100) )
                          ), # end RCT trial design conditional panel
                          
                          
                          # Feasibility trial design inputs
                          conditionalPanel(condition = "input.typeOfResearch == 'feasibility'",
                                           
                                           numericInput("probabilityOfDefinitiveResearch", "Liklihood of feasibility research leading to follow-up study",
                                                        value = 0.5, min = 0, max = 1, step = 0.05),
                                           
                                           numericInput("durationOfResearchFeas", "Expected duration of feasibility research (years)",
                                                        value = 2, min = 0, max = NA, step = 0.1),
                                           
                                           numericInput("durationOfResearchDefinitive", "Expected duration of follow-up research (years)",
                                                        value = 5, min = 0, max = NA, step = 0.1),
                                           
                                           numericInput("costResearchFunderFeas", "Costs of feasibility research to funder",
                                                        value = 1000000, min = 0, max = NA, step = 100),
                                           
                                           numericInput("costResearchFunderDefinitive", "Costs of follow-up research to funder",
                                                        value = 1000000, min = 0, max = NA, step = 100),
                                           
                                           conditionalPanel(condition = "input.typeOfOutcome == 'netHealth'",
                                                            numericInput("costHealthSystemFeas", "Costs of feasibility research imposed on health system",
                                                                         value = 1000000, min = 0, max = NA, step = 100),
                                                            
                                                            # display if: typeOfOutcome == "netHealth"
                                                            numericInput("costHealthSystemDefinitive", "Costs of follow-up research imposed on health system",
                                                                         value = 1000000, min = 0, max = NA, step = 100) )
                          ) # end Feasibility trial design conditional panel
                          
                          
                          
                        ) # end wellPanel 
                 ), # end 3rd column inputs
                 
                 column(3, 
                        ##########
                        # Run buttons 
                        ##########
                        
                        wellPanel(
                          
                          actionButton("run", label = "Run analysis"),
                          br(),
                          
                          # **check maximum number feasible
                          conditionalPanel(condition = "input.reconsider != 'Yes'",
                                           numericInput("MCsims", "Number of simulations",
                                                        value = 50000, min = 0, max = 10000000, step = 500)),
                          
                          # **check maximum number feasible
                          conditionalPanel(condition = "input.reconsider == 'Yes'",
                                           
                                           p("Note that this analysis will take between ## and ## minutes to report. This is due to the large number of simulations required"),
                                           numericInput("MCsimsInner", "Number of simulations for inner loop",
                                                        value = 50000, min = 0, max = 10000000, step = 500),
                                           
                                           numericInput("MCsimsOuter", "Number of simulations for outer loop",
                                                        value = 50000, min = 0, max = 10000000, step = 500))
                          
                          
                        ) # end well panel run button
                 ) # end column 4: run button
                 
               ), # end top fluid row
               
               # LOWER ROW
               fluidRow(
                 column(3, 
                        ##########
                        # treatment 1 
                        ##########
                        
                        wellPanel(
                          h4("Treatment 1"),
                          p("This is considered the baseline treatment. If a no treatment or standard practice option is considered then it should be entered here"),
                          
                          textInput("nameOf_t1", "Name of treatment 1 (optional)", 
                                    value = "late PTP"),
                          
                          numericInput("utilisation_t1", "Current utilisation of treatment 1 (%)",
                                       value = 100, min = 0, max = 100, step = 0.1),
                          
                          conditionalPanel(condition = "input.typeOfEndpoint == 'binary'",
                                           numericInput("P_t1", "Probability of outcome with treatment 1 (Baseline risk)",
                                                        value = 0.5, min = 0, max = 1, step = 0.05)),
                          
                          # survival inputs for t1
                          conditionalPanel(condition = "input.typeOfEndpoint == 'survival'",
                                           numericInput("scaleParameter_t1", "Scale parameter for treatment 1 (natural scale)",
                                                        value = 5, min = 0, max = NA, step = 1),
                                           
                                           conditionalPanel(condition = "input.survivalDist == 'weibull'",
                                                            numericInput("shapeParameter_t1", "Shape parameter for treatment 1 (natural scale)",
                                                                         value = 1.1, min = 0, max = NA, step = 0.1))
                          ), # end survival inputs for t1
                          
                          conditionalPanel(condition = "input.typeOfEndpoint == 'continuous'",
                                           p("Note that if the primary endpoint is continuous the expected outcome on the continuous scale with the baseline treatment is not required. For further details see ####INSERT REFERENCE")),
                          
                          # Cost inputs for t1
                          conditionalPanel(condition = "input.typeOfOutcome == 'netHealth'",
                                           
                                           conditionalPanel(condition = "input.tCostsDependOnEvent == 'No'",
                                                            numericInput("cost_t1", "Lifetime treatment costs associated with treatment 1",
                                                                         value = 100, min = NA, max = NA, step = 10) ),
                                           
                                           conditionalPanel(condition = "input.tCostsDependOnEvent == 'Yes'",
                                                            numericInput("costEvent_t1", "Lifetime treatment costs associated with treatment 1 if the primary outcome occurs",
                                                                         value = 100, min = NA, max = NA, step = 10),
                                                            
                                                            numericInput("costNotEvent_t1", "Lifetime treatment costs associated with treatment 1 if the primary outcome does not occur",
                                                                         value = 100, min = NA, max = NA, step = 10) )
                          ) # end Cost inputs for t1
                          
                          
                          
                        ) # end wellPanel t1
                 ), # end column t1
                 
                 column(3, 
                        ##########
                        # treatment 2 
                        ##########
                        
                        wellPanel(
                          h4("Treatment 2"),
                          textInput("nameOf_t2", "Name of treatment 2 (optional)", 
                                    value = "early PTP"),
                          
                          numericInput("utilisation_t2", "Current utilisation of treatment 2 (%)",
                                       value = 0, min = 0, max = 100, step = 0.1),
                          
                          selectInput("dist_t2", label = "Distribution of treatment 2 relative effects", 
                                      choices = c("Normal" = "norm", 
                                                  "Half Normal" = "halfNorm"),
                                      selected = "Normal"),
                          
                          # normal dist inputs for t2
                          conditionalPanel(condition = "input.dist_t2 == 'norm'",
                                           numericInput("mu_t2", "Mean log odds / log hazard ratio for treatment 2",
                                                        value = 0, min = NA, max = NA, step = 0.05),
                                           
                                           numericInput("variance_t2", "Variance of log odds / log hazard ratio for treatment 2",
                                                        value = 0.25, min = NA, max = NA, step = 0.05)
                          ), # end normal dist inputs for t2
                          
                          conditionalPanel(condition = "input.dist_t2 == 'halfNorm'",
                                           selectInput("direction_t2", label = "Direction of distribution for treatment 2", 
                                                       choices = c("Always positive" = "alwaysPositive", 
                                                                   "Always negative" = "alwaysNegative"),
                                                       selected = "alwaysPositive")),
                          
                          numericInput("MCD_t2", "MCD for treatment 2",
                                       value = 0, min = NA, max = NA, step = 0.05),
                          
                          # Cost inputs for t2
                          conditionalPanel(condition = "input.typeOfOutcome == 'netHealth'",
                                           
                                           conditionalPanel(condition = "input.tCostsDependOnEvent == 'No'",
                                                            numericInput("cost_t2", "Lifetime treatment costs associated with treatment 2",
                                                                         value = 100, min = NA, max = NA, step = 10) ),
                                           
                                           conditionalPanel(condition = "input.tCostsDependOnEvent == 'Yes'",
                                                            numericInput("costEvent_t2", "Lifetime treatment costs associated with treatment 2 if the primary outcome occurs",
                                                                         value = 100, min = NA, max = NA, step = 10),
                                                            
                                                            numericInput("costNotEvent_t2", "Lifetime treatment costs associated with treatment 2 if the primary outcome does not occur",
                                                                         value = 100, min = NA, max = NA, step = 10) )
                          ) # end Cost inputs for t2
                        ) # end wellPanel t2
                 ), # end column t2
                 
                 column(3, 
                        ##########
                        # treatment 3 
                        ##########
                        
                        conditionalPanel(condition = "input.numberOfTreatments >= 3",
                                         
                                         wellPanel(
                                           h4("Treatment 3"),
                                           # display if: numberOfTreatments >= 3
                                           textInput("nameOf_t3", "Name of treatment 3 (optional)", 
                                                     value = "treatment 3"),
                                           
                                           # display if: numberOfTreatments >= 3
                                           numericInput("utilisation_t3", "Current utilisation of treatment 3 (%)",
                                                        value = 0, min = 0, max = 100, step = 0.1),
                                           
                                           # display if: numberOfTreatments >= 3 & typeOfEndpoint != successFail 
                                           selectInput("dist_t3", label = "Distribution of treatment 3 relative effects", 
                                                       choices = c("Normal" = "norm", 
                                                                   "Half Normal" = "halfNorm"),
                                                       selected = "Normal"),
                                           
                                           # normal dist inputs for t3
                                           conditionalPanel(condition = "input.dist_t3 == 'norm'",
                                                            numericInput("mu_t3", "Mean log odds / log hazard ratio for treatment 3",
                                                                         value = 0, min = NA, max = NA, step = 0.05),
                                                            
                                                            numericInput("variance_t3", "Variance of log odds / log hazard ratio for treatment 3",
                                                                         value = 0.25, min = NA, max = NA, step = 0.05)
                                           ), # end normal dist inputs for t3
                                           
                                           conditionalPanel(condition = "input.dist_t3 == 'halfNorm'",
                                                            selectInput("direction_t3", label = "Direction of distribution for treatment 3", 
                                                                        choices = c("Always positive" = "alwaysPositive", 
                                                                                    "Always negative" = "alwaysNegative"),
                                                                        selected = "alwaysPositive")),
                                           
                                           numericInput("MCD_t3", "MCD for treatment 3",
                                                        value = 0, min = NA, max = NA, step = 0.05),
                                           
                                           # Cost inputs for t3
                                           conditionalPanel(condition = "input.typeOfOutcome == 'netHealth'",
                                                            
                                                            conditionalPanel(condition = "input.tCostsDependOnEvent == 'No'",
                                                                             numericInput("cost_t3", "Lifetime treatment costs associated with treatment 3",
                                                                                          value = 100, min = NA, max = NA, step = 10) ),
                                                            
                                                            conditionalPanel(condition = "input.tCostsDependOnEvent == 'Yes'",
                                                                             numericInput("costEvent_t3", "Lifetime treatment costs associated with treatment 3 if the primary outcome occurs",
                                                                                          value = 100, min = NA, max = NA, step = 10),
                                                                             
                                                                             numericInput("costNotEvent_t3", "Lifetime treatment costs associated with treatment 3 if the primary outcome does not occur",
                                                                                          value = 100, min = NA, max = NA, step = 10) )
                                           ) # end cost inputs t3
                                           
                                           
                                         ) # end wellPanel t3
                        ) # end conditional panel t3
                 ), # end column t3
                 
                 column(3,
                        ##########
                        # treatment 4
                        ##########
                        
                        conditionalPanel(condition = "input.numberOfTreatments >= 4",
                                         
                                         wellPanel(
                                           h4("Treatment 4"),
                                           # display if: numberOfTreatments >= 4
                                           textInput("nameOf_t4", "Name of treatment 4 (optional)", 
                                                     value = "treatment 4"),
                                           # display if: numberOfTreatments >= 4
                                           numericInput("utilisation_t4", "Current utilisation of treatment 4 (%)",
                                                        value = 0, min = 0, max = 100, step = 0.1),
                                           
                                           # display if: numberOfTreatments >= 4 & typeOfEndpoint != successFail 
                                           selectInput("dist_t4", label = "Distribution of treatment 4 relative effects", 
                                                       choices = c("Normal" = "norm", 
                                                                   "Half Normal" = "halfNorm"),
                                                       selected = "Normal"),
                                           
                                           
                                           # normal dist inputs for t4
                                           conditionalPanel(condition = "input.dist_t4 == 'norm'",
                                                            numericInput("mu_t4", "Mean log odds / log hazard ratio for treatment 4",
                                                                         value = 0, min = NA, max = NA, step = 0.05),
                                                            
                                                            numericInput("variance_t4", "Variance of log odds / log hazard ratio for treatment 4",
                                                                         value = 0.25, min = NA, max = NA, step = 0.05)
                                           ), # end normal dist inputs for t4
                                           
                                           conditionalPanel(condition = "input.dist_t4 == 'halfNorm'",
                                                            selectInput("direction_t4", label = "Direction of distribution for treatment 4", 
                                                                        choices = c("Always positive" = "alwaysPositive", 
                                                                                    "Always negative" = "alwaysNegative"),
                                                                        selected = "alwaysPositive")),
                                           
                                           numericInput("MCD_t4", "MCD for treatment 4",
                                                        value = 0, min = NA, max = NA, step = 0.05),
                                           
                                           # Cost inputs for t4
                                           conditionalPanel(condition = "input.typeOfOutcome == 'netHealth'",
                                                            
                                                            conditionalPanel(condition = "input.tCostsDependOnEvent == 'No'",
                                                                             numericInput("cost_t4", "Lifetime treatment costs associated with treatment 4",
                                                                                          value = 100, min = NA, max = NA, step = 10) ),
                                                            
                                                            conditionalPanel(condition = "input.tCostsDependOnEvent == 'Yes'",
                                                                             numericInput("costEvent_t4", "Lifetime treatment costs associated with treatment 4 if the primary outcome occurs",
                                                                                          value = 100, min = NA, max = NA, step = 10),
                                                                             
                                                                             numericInput("costNotEvent_t4", "Lifetime treatment costs associated with treatment 4 if the primary outcome does not occur",
                                                                                          value = 100, min = NA, max = NA, step = 10) )
                                           ) # end cost inputs t4
                                           
                                         ) # end wellPanel t4
                        ) # end of conditionalPanel t4
                 ) # end treatment 4 column
               ) # end lower fluidRow
             ) # end of inputs fluidPage
    ), # end of inputs tab panel
    
    ##################
    # RESULTS page
    ##################
    tabPanel("Results" 
             
             ), # end results tabPanel
    
    ##################
    # Write report page
    ##################
    # use textAreaInput to provide boxes so that analysts can justify their variable choices
    # 
    
    tabPanel("Write and Download Report",
             fluidPage(
               fluidRow(
                 column(4, "col 1"),
                 column(4, "col 2")
               ) # end 1st Write Report fluidRow 
             ) # end Write Report fluidPage 
    ), # end write report tabPanel
    
    ##################
    # ABOUT page
    ##################
    tabPanel("About", 
             fluidPage(
               fluidRow(
                 column(4, "<code for about ui>"),
                 column(4, "next row")
               ) # end 1st About fluidRow  
               
             ) # end About fluidPage 
    ) # end About tabPanel
    
                              ) # end App tabSetPanel
  
             ) # end App fluid page
                              ) # end App shinyUi function
