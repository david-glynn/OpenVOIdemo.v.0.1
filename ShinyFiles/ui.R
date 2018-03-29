############################
# potential bugs: 
# how many MCsims should be the max? some funny behavour at large numbers.
# if "reconsider == Yes" (when there is no model that runs under this condition) the model evaluates the previous model again.
# 
# possible improvements:
# 
# tests:
# need to see how it works with NAs as defaults for everything!
# odds range set both range to max - 
#Warning in min(x) : no non-missing arguments to min; returning Inf
#Warning in max(x) : no non-missing arguments to max; returning -Inf

library(shiny)

shinyUI(fluidPage(
  titlePanel("Rapid value of information (VOI) decision tool"),
  
  tabsetPanel(
    
    ##################
    # welcome tabs
    ##################
    
    tabPanel("How to use this app",
             
             br(),
             h4("A video on using this app"),
             br(),
             p("<INSERT SHORT YOUTUBE VIDEO ON HOW TO USE THIS APP>"),
             br(),
             h4("How to use this app"),
             p("This is an R Shiny App which facilitates calculations of the value of research proposals in a timely manner. 
               The inputs required in the app represent the minimum needed to understand the consequences of uncertainty and the need for further research.
               Full details of the approach used and applied examples using these methods are forthcoming. In the meantime click",a("here", href = "https://www.york.ac.uk/che/research/teehta/research-prioritisation/") ,"for further details.
               "),
             p(strong("Users unfamiliar with value of informaiton methods"), "are encouraged to read the information in the 'How to estimate research value' tab. This section describes the value of information approach and how it applies to research funding in a resource constrainted health care system.
               "),
             p(strong("Those who have not used this app before"), "click the'Inputs and how to use this app' tab. This section describes the types of analysis which are possible with this app and the inputs that are required.
               "),
             p(strong("To carry out an analysis"), "click the 'Inputs' tab
               "),
             br(),
             tags$em("This code has been produced under a GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007")
             
             ),  # close How to use this app tab
    
    
    ##################
    # How to estimate research value tab
    ##################
    
    tabPanel("How to estimate research value",
             
             br(),
             h4("A video introducing value of information"),
             HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/tbv9E9D2BRQ" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>'),
             br(),
             h4("How can health oucomes be improved?"),
             p("Additional evidence is valuable because it can improve patient outcomes by resolving existing uncertainty about the effectiveness of the interventions available. This helps inform treatment decisions for subsequent patients. 
               A judgement about the level of uncertainty in the existing evidence base can come from a number of sources including a systematic literature review and meta-analysis, expert elicitation, extrapolation, meta-epidemiological study, or a combination of these different sources. 
               The expected health benefit provided by reducing uncertainty is called the information value for a research project."
             ),
             p("In addition to funding research, it is also possible to improve health outcomes by ensuring that the treatment option that is expected to be best based on the findings of existing evidence is implemented into clinical practice.  In fact, the improvements in health outcomes from implementing the findings of the current evidence base (implementation value) may be greater than the potential improvements in health outcomes through conducting further research."
             ),
             p("Drawing a distinction between the information value and the implementation value is important because conducting further evaluative research is not the only way to change clinical practice. The results of a new research study may influence clinical practice and may contribute to the implementation of research findings but this is not the only, or necessarily the most effective, way to do so. There are other mechanisms (e.g., more effective dissemination of existing evidence) and policies which are within the remit of other bodies (e.g., incentives and sanctions) to affect implementation. Therefore, conducting research to influence implementation rather than because there is real value in acquiring additional evidence itself would seem inappropriate. This is because limited research capacity could have been used elsewhere to conduct research in areas where it would have offered greater potential health benefits
               "),
             h4("What change in the primary endpoint is required?"),
             p("Uncertainty in a decision about alternative treatment options arises from the range of plausible values that the important endpoints can take.  When the range of plausible values for a particular parameter such as the relative treatment effect can support more than one intervention (e.g., the confidence interval for the estimate of relative effect crosses the line of no difference) this uncertainty has consequences for health outcomes.  This is because for any treatment choice there is a chance that an alternative intervention could have improved health outcomes to a greater extent.  The importance of this uncertainty is indicated by the scale of the health consequences of the uncertainty.   The chance that an intervention is not the most effective, how much less effective it is likely to be, and the size of the patient population facing the uncertain treatment choice all contribute to the health consequences of uncertainty.
               "),
             p("The primary endpoint, which usually captures the most important aspects of health outcome, can be used as a starting point to understand the consequences of current uncertainty. However, in situations where there are a number of other important considerations that are not captured in the primary outcome, we can specify a minimum clinical difference (MCD) in the primary outcome that would need to be detected in future research.  This represents the change in the primary endpoint that would need to be detected for the results of any new research study to be considered clinically significant and have an impact on clinical practice. 
               This MCD concept will be explained further in the next section: 'How to use this app'.")
             
             ), # end How to estimate research value tab
    
    
    ##################
    # Inputs tabs
    ##################
    
    tabPanel("Inputs",
             
             tabsetPanel(selected = "Step 1: Primary outcome",
                         
                         #################
                         # input information subtab
                         
                         tabPanel("Input information",
                                  
                                  br(),
                                  h4("Inputs required to estimate research value"),
                                  br(),
                                  
                                  # step 1 input information
                                  wellPanel(
                                    h4("Step 1 inputs"),
                                    strong("Type of primary endpoint"),
                                    p("The primary outcome measure or endpoint captures the most important aspects of health outcome in the research."), 
                                    
                                    strong("Express results in natural outcomes (e.g. heart attacks avoided) or in QALYs?"),
                                    p("The benefits of research can be expressed in either natural outcomes or in Quality Adjusted Life Years (QALYs).
                                      Using QALYs requires more inputs but enables a comparion of the health benefits of further research and implementation efforts across diverse clinical areas."),
                                    
                                    strong("Is the outcome a benefit (e.g. cures) or a harm (e.g. heart attack)?"),
                                    p("For natural outcomes, the value of additional research is expressed in terms of ‘benefits gained’ or ‘harms avoided’ depending on whether this outcome is a benefit or harm."),
                                    
                                    strong("Name of outcome e.g. heart attack"),
                                    p("This will be used in reporting results."),
                                    
                                    strong("Type of research"),
                                    p("The value of research can be calculated for either randomised controlled trials (RCTs) or feasibility studies.
                                      The inputs required for the analysis will depend on the type of study chosen."),
                                    tags$ul(
                                      tags$li("RCT: In this type of study, individuals are randomised to different treatments and the outcomes are compared accross the groups."), 
                                      tags$li("Feasibility study: If there are uncertainties about whether a large trial is possible, a short feasibility study can be carried out to assess the possibility of future research.
                                              If the feasibility study is successful, researchers have the option to carry out the follow up trial.")
                                      ),
                                    
                                    strong("How many treatment options are under consideration? (Maximum of 4)"),
                                    p("There may be a number of relevant treatment options for a given indication. 
                                      This app currently allows for up to 4 options to be considered."),
                                    
                                    strong("Do the treatment costs depend on the primary outcome?"),
                                    p("In some cases treatment costs will depend importantly on whether the primary outcome occurs or not.
                                      For example, if a treatment is used to prevent disease progression then it will cease to be used (and its cost will no longer be incurred) if the individual progresses.")
                                    
                                    ), # end step 1 input information 
                                  br(),
                                  
                                  
                                  # step 2 input information
                                  wellPanel(
                                    h4("Step 2 inputs"),
                                    strong("Incidence per annum"),
                                    p("An estimate of the number of individuals facing the uncertain choice between alternative interventions is required in order to establish the size of the benefits to the target population. 
                                      "),
                                    strong("Length of time for which the new evidence would be valuable"),
                                    p("The information generated by new research will not be valuable indefinitely because other changes occur over time. For example, over time new and more effective interventions become available which will eventually make those currently available obsolete. This means that new information about effectiveness is only relevant for a specific amount of time. A judgement about the length of time that the evidence from the proposed RCT might be valuable is required to estimate the expected benefits over an appropriate time horizon.  
                                      "),
                                    strong("Discount rate"),
                                    p("When a time horizon greater than one year is considered in the analysis, discounting should be used to reflect the fact that resources committed today could be invested at a real rate of return to free up more resources in the future. 
                                      "),
                                    strong("Opportunity cost of health expenditure"),
                                    p("
                                      "),
                                    strong("Net health effect of survival endpoint (in QALYs)"),
                                    p("
                                      "),
                                    strong("Name of outcome"),
                                    p("
                                      "),
                                    strong("Currency used"),
                                    p("
                                      ")
                                    ), # end step 2 input information 
                                  br(),
                                  
                                  
                                  
                                  
                                  
                                  strong("The value of reconsidering the evidence"),
                                  p("
                                    "),
                                  br(),
                                  
                                  
                                  
                                  # step 3 input information
                                  wellPanel(
                                    h4("Step 3 inputs"),
                                    strong("Duration of the research"),
                                    p("**discuss feasibility durations required here too** Some assessment of the duration of time for the proposed research to be conducted and for the  results of the research to report is required since the value of research declines the longer it takes to report. This might be informed by an assessment of sample size, recruitment rates, or historical experience from conducting similar types of studies. 
                                      "),
                                    strong("Costs of the research"),
                                    p("**discuss all costs here** Some assessment of the likely costs of the proposed RCT is required to establish whether the expected benefits from the study are sufficient to justify the expected costs. 
                                      "),
                                    strong("Liklihood of feasibility research leading to follow-up study"),
                                    p("
                                      ")
                                    ),
                                  br(),
                                  
                                  wellPanel(
                                    h4("Run analysis"),
                                    strong("Run analysis button"),
                                    p(""),
                                    strong("Number of simulations"),
                                    p("**talk about reconsider evidence here")
                                  ),
                                  
                                  br(),
                                  
                                  wellPanel(
                                    h4("Treatment inputs"),
                                    strong("Baseline event rate"),
                                    p(""),
                                    strong("Current level of utilisation of the interventions"),
                                    p("Health systems can improve health outcomes in two distinct ways: (i) generating additional evidence to reduce uncertainty about which treatment improves health (information value); and (ii) changing clinical practice such that the optimal treatment based on current evidence is implemented (implementation value).
                                      Some estimate of the current level of utilisation of the interventions in clinical practice is required to establish the value of changing practice if the results of new research suggest a change. It can also be used to establish whether there is greater value from encouraging the implementation of what existing evidence suggests is the most effective intervention rather than conducting new research. 
                                      "),
                                    strong("Relative treatment effects"),
                                    p("An estimate of the relative effectiveness of the intervention is required for the primary outcome, along with an estimate of its uncertainty. 
                                      This information is required in the form of a mean and variance of the log odds ratio (for binary and continuous outcomes) or a mean and variance of the log hazard ratio (for survival outcomes). 
                                      This estimate is usually obtained from a standard meta-analysis. However, if the estimate is unavailable or considered inadequate, alternative values can be used to represent different judgements about the uncertain relative treatment effect. 
                                      "),
                                    strong("Distribution of relative effects"),
                                    p("")
                                    
                                    )
                                  
                                  
                                  
                                  
                                    ), # end how to use this app tabPanel
                         
                         
                         #################
                         # Part 1 inputs subtab
                         
                         tabPanel("Step 1: Primary outcome",
                                  br(),
                                  h4("Select appropriate values and then proceed to the 'Step 2' tab"),
                                  strong("See the 'Input information' tab for detail on how to interpret the inputs"),
                                  br(),
                                  br(),
                                  
                                  ## top fluid row for high level app inputs
                                  fluidRow(
                                    column(6,
                                           
                                           #
                                           # Primary outcome measure inputs
                                           #
                                           wellPanel(
                                             h4("Primary outcome measure"),
                                             #p("This information determines the inputs required for the analysis and so this section",
                                             #  strong("should be completed first.")),
                                             
                                             selectInput(inputId = "typeOfEndpoint",  HTML('<span title= "This captures the most important aspects of health outcome in the research" >
                                                                                           Type of primary endpoint </span>'), 
                                                         choices = c("Binary e.g. a heart attack occurs or it does not" = "binary", 
                                                                     "Continuous e.g. a measure on the blood pressure scale" = "continuous", 
                                                                     "Survival e.g. months of progression free survival" = "survival"),
                                                         selected = "binary"), 
                                             
                                             
                                             # new
                                             selectInput(inputId = "outcomeExpression", label = "Express results in natural outcomes or in Quality Adjusted Life Years (QALYs)?", 
                                                         choices = c("Natural outcomes e.g. heart attacks avoided" = "natural",  
                                                                     "QALYs" = "netHealth"),
                                                         selected = "QALYs"), 
                                             
                                             # new
                                             conditionalPanel(condition = "input.outcomeExpression == 'natural'",
                                                              selectInput(inputId = "benefitOrHarm", label = "Is the outcome a benefit or a harm?", 
                                                                          choices = c("Benefit e.g. cure" = "benefit", 
                                                                                      "Harm e.g. heart attack" = "harm"),
                                                                          selected = "Benefit")),
                                             
                                             
                                             conditionalPanel(condition = "input.outcomeExpression == 'natural'",
                                                              textInput("nameOfOutcome", "Name of outcome", 
                                                                        value = "Functional recovery"))
                                             
                                             
                                             
                                             ) # end primary outcome measure wellPanel
                                           
                                           
                                  ), # end leftmost column step 1, top level
                                  column(6,
                                         conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth'",
                                         wellPanel(
                                           
                                           radioButtons(inputId = "tCostsDependOnEvent", label = "Do the treatment costs depend on the primary outcome?", 
                                                                       choices = c("Yes" = "Yes", 
                                                                                   "No" = "No"),
                                                                       selected = "No"))
                                         )
                                         ) # end rightmost column step 1, top level
                                  
                                  ), # end step 1 top level fluidRow
                                  
                                  # binary conditional headings
                                  conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth'",
                                                   fluidRow(
                                                     column(6,
                                                     wellPanel(
                                                       numericInput("numberS1States", "Number of possible states if the primary outcome occurs (4 maximum)",
                                                                    value = 4, min = 1, max = 4),
                                                       numericInput("numberS0States", "Number of possible states if the primary outcome does not occur (4 maximum)",
                                                                    value = 4, min = 1, max = 4)
                                                     )
                                                     )
                                                   )),
                                  
                                  # binary conditional headings
                                  conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth'",
                                                   fluidRow(
                                                     h4("Outcomes if primary endpoint occurs")
                                                   )),
                                  
                                  # setp 1 mid level fluid row
                                  # binary states for when event does occur
                                  # continuous and survial inputs
                                  fluidRow(
                                    # binary: s11
                                    # continous: utility increase/decrease + how much
                                    # survival: utility pre transition
                                    column(3,
                                           
                                           # binary s11
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth'",
                                                            wellPanel(
                                                              
                                                              strong("State 1.1"),
                                                              conditionalPanel(condition = "input.numberS1States >= 2 ",
                                                                  numericInput("probability_s11", "Conditional on the primary outcome occuring, what is the probability of being in this state?",
                                                                               value = 0.42, min = 0, max = 1, step = 0.05)),
                                                              numericInput("lifeDuration_s11", "What is the life expectancy of an individual in this state (years)?",
                                                                           value = 16.73, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s11", "What is the health utility associated with this state?",
                                                                           value = 0.7, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s11", "What are the lifetime disease related costs associated with this state?",
                                                                           value = 27047, min = 0, step = 100)
                                                              
                                                              
                                                            )),
                                           
                                           # continous: utility increase decrease + how much
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'continuous' && input.outcomeExpression == 'netHealth'",
                                                            wellPanel(
                                                              
                                                              selectInput(inputId = "deltaUnitUtilityDirection",  "Is an increase in the primary outcome expected to be associated with an increase or decrease in health state utility?",
                                                                          choices = c("Increase" = "increase",
                                                                                      "Decrease" = "decrease"),
                                                                          selected = "increase"),
                                                              numericInput("deltaUnitUtilitySize", "By how much is a one unit increase in the primary outcome expected to increase/decrease the health state utility?",
                                                                           value = 0.1, min = 1, max = 1)
                                                              
                                                            )),
                                           
                                           # survival: utility pre transition
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'survival' && input.outcomeExpression == 'netHealth'",
                                                            wellPanel(
                                                              
                                                              numericInput("utilityPreTransition", "What is the health utility associated with the pre-transition health state?",
                                                                           value = 0.7, min = -2, max = 1, step = 0.05)
                                                            
                                                            ))
                                           
                                           ), # column 1 mid level
                                    
                                    # binary: s12
                                    # continous: costs increase/decrease + how much
                                    # survival: monthly costs pre transition
                                    column(3,
                                           
                                           # binary: s12
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth' && input.numberS1States >= 2",
                                                            wellPanel(
                                                              
                                                              strong("State 1.2"),
                                                              numericInput("probability_s12", "Conditional on the primary outcome occuring, what is the probability of being in this state?",
                                                                           value = 0.24, min = 0, max = 1, step = 0.05),
                                                              numericInput("lifeDuration_s12", "What is the life expectancy of an individual in this state (years)?",
                                                                           value = 16.73, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s12", "What is the health utility associated with this state?",
                                                                           value = 0.81, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s12", "What are the lifetime disease related costs associated with this state?",
                                                                           value = 27047, min = 0, step = 100)
                                                              
                                                              
                                                            )),
                                           
                                           # continous: costs increase/decrease + how much
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'continuous' && input.outcomeExpression == 'netHealth'",
                                                            wellPanel(
                                                              
                                                              selectInput(inputId = "deltaUnitCostsDirection",  "Is an increase in the primary outcome expected to be associated with disease related cost increases or decreases?",
                                                                          choices = c("Increase" = "increase",
                                                                                      "Decrease" = "decrease"),
                                                                          selected = "increase"),
                                                              numericInput("deltaUnitCostsSize", "By how much is a one unit increase in the primary outcome expected to increase/decrease monthly disease related costs?",
                                                                           value = 200, min = 0)
                                                              
                                                            )),
                                           
                                           # survival: monthly costs pre transition
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'survival' && input.outcomeExpression == 'netHealth'",
                                                            wellPanel(
                                                              numericInput("monthlyCostPreTransition", "What are the expected monthly disease related costs associated with the pre-transition health state?",
                                                                           value = 2000, min = 0, step = 100)
                                                             
                                                            ))
                                           
                                           ),
                                    
                                           
                                    
                                    
                                    # binary: s13
                                    # continuous: treat effect duration
                                    column(3,
                                           
                                           # binary s13
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth' && input.numberS1States >= 3",
                                                            wellPanel(
                                                              
                                                              strong("State 1.3"),
                                                              numericInput("probability_s13", "Conditional on the primary outcome occuring, what is the probability of being in this state?",
                                                                           value = 0.2, min = 0, max = 1, step = 0.05),
                                                              numericInput("lifeDuration_s13", "What is the life expectancy of an individual in this state (years)?",
                                                                           value = 19.23, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s13", "What is the health utility associated with this state?",
                                                                           value = 0.96, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s13", "What are the lifetime disease related costs associated with this state?",
                                                                           value = 19575, min = 0, step = 100)
                                                            )),
                                           
                                           # continous: teat effect duration
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'continuous' && input.outcomeExpression == 'netHealth'",
                                                            wellPanel(
                                                              
                                                              sliderInput("treatmentDurationMonths", "How long is the treatment effect expected to last? (months)",
                                                                          value = 36, min = 0, 120)
                                                              
                                                              
                                                            ))
                                           
                                           ),
                                    
                                    # binary: s14
                                    column(3,
                                           
                                           # binary s14
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth' && input.numberS1States >= 4",
                                                            wellPanel(
                                                              
                                                              strong("State 1.4"),
                                                              numericInput("probability_s14", "Conditional on the primary outcome occuring, what is the probability of being in this state?",
                                                                           value = 0.14, min = 0, max = 1, step = 0.05),
                                                              numericInput("lifeDuration_s14", "What is the life expectancy of an individual in this state (years)?",
                                                                           value = 19.23, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s14", "What is the health utility associated with this state?",
                                                                           value = 1, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s14", "What are the lifetime disease related costs associated with this state?",
                                                                           value = 19575, min = 0, step = 100)
                                                            ))
                                           
                                           )
                                    
                                  ), # end step 1 mid level fluid row
                                  
                                  # binary conditional headings
                                  conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth'",
                                                   fluidRow(
                                                     h4("Outcomes if primary endpoint does not occur")
                                                   )),
                                  
                                  # step 1 bottom level fluid row
                                  # binary states for when event does not occur
                                  fluidRow(
                                    
                                    #binary: s01
                                    column(3,
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth' && input.numberS0States >= 1",
                                                            wellPanel(
                                                              
                                                              strong("State 2.1"),
                                                              conditionalPanel(condition = "input.numberS0States >= 2 ",
                                                                    numericInput("probability_s01", "Conditional on the primary outcome occuring, what is the probability of being in this state?",
                                                                                 value = 0.29, min = 0, max = 1, step = 0.05)),
                                                              numericInput("lifeDuration_s01", "What is the life expectancy of an individual in this state (years)?",
                                                                           value = 0, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s01", "What is the health utility associated with this state?",
                                                                           value = 0, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s01", "What are the lifetime disease related costs associated with this state?",
                                                                           value = 0, min = 0, step = 100)
                                                            ))
                                           
                                           
                                    ),
                                    
                                    #binary: s02
                                    column(3,
                                           
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth' && input.numberS0States >= 2",
                                                            wellPanel(
                                                              
                                                              strong("State 2.2"),
                                                              numericInput("probability_s02", "Conditional on the primary outcome occuring, what is the probability of being in this state?",
                                                                           value = 0.07, min = 0, max = 1, step = 0.05),
                                                              numericInput("lifeDuration_s02", "What is the life expectancy of an individual in this state (years)?",
                                                                           value = 7.11, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s02", "What is the health utility associated with this state?",
                                                                           value = 0.11, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s02", "What are the lifetime disease related costs associated with this state?",
                                                                           value = 45450, min = 0, step = 100)
                                                            ))
                                           
                                    ),
                                    # binary: s03
                                    column(3,
                                           
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth' && input.numberS0States >= 3",
                                                            wellPanel(
                                                              
                                                              strong("State 2.3"),
                                                              numericInput("probability_s03", "Conditional on the primary outcome occuring, what is the probability of being in this state?",
                                                                           value = 0.41, min = 0, max = 1, step = 0.05),
                                                              numericInput("lifeDuration_s03", "What is the life expectancy of an individual in this state (years)?",
                                                                           value = 12.52, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s03", "What is the health utility associated with this state?",
                                                                           value = 0.41, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s03", "What are the lifetime disease related costs associated with this state?",
                                                                           value = 154324, min = 0, step = 100)
                                                            ))
                                           
                                    ),
                                    
                                    # binary: s04
                                    column(3,
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth' && input.numberS0States >= 4",
                                                            wellPanel(
                                                              
                                                              strong("State 2.4"),
                                                              numericInput("probability_s04", "Conditional on the primary outcome occuring, what is the probability of being in this state?",
                                                                           value = 0.23, min = 0, max = 1, step = 0.05),
                                                              numericInput("lifeDuration_s04", "What is the life expectancy of an individual in this state (years)?",
                                                                           value = 12.52, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s04", "What is the health utility associated with this state?",
                                                                           value = 0.58, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s04", "What are the lifetime disease related costs associated with this state?",
                                                                           value = 154324, min = 0, step = 100)
                                                            ))     
                                    )
                                    
                                    
                                  ) # end step 1 bottom level fluid row
                                  
                                  
                                  ), # end part 1 inputs tabPanel 
                         
                         ##################
                         # Part 2 treatment inputs subtab
                         
                         
                         tabPanel("Step 2: Interventions",
                                  br(),
                                  h4("Select appropriate values and then proceed to the 'Step 3' tab"),
                                  strong("See the 'Input information' tab for detail on how to interpret the inputs"),
                                  
                                  fluidPage( 
                                    br(),
                                    ## top top level
                                    fluidRow(
                                      column(6,
                                             wellPanel(
                                               numericInput("numberOfTreatments", 
                                                            "How many treatment options are under consideration? (Maximum of 4)",
                                                            value = 2, min = 2, max = 4)
                                             ) # end top top well panel
                                      ) # end column
                                    ), # end top top fluid row
                                    ## top level fluid row for "non-epi treatment inputs" width of 3 column for each
                                    fluidRow(
                                      
                                      column(3,
                                             
                                             wellPanel(
                                               h4("Baseline treatment"),
                                               #p("If a no treatment or standard practice option is considered then it should be entered here"),
                                               
                                               textInput("nameOf_t1", "Name of baseline treatment", 
                                                         value = "late PTP"),
                                               
                                               numericInput("utilisation_t1", "Current level of utilisation (%)",
                                                            value = 100, min = 0, max = 100, step = 0.1),
                                               
                                               # Cost inputs for baseline (t1)
                                               conditionalPanel(condition = "input.outcomeExpression == 'netHealth'",
                                                                
                                                                #binary costs (dont depend on outcome)
                                                                conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.tCostsDependOnEvent == 'No'",
                                                                                 numericInput("cost_t1", "Treatment costs over patient time horizon",
                                                                                              value = 100, min = NA, max = NA, step = 10) ),
                                                                #binary costs (DO depend on outcome)
                                                                conditionalPanel(condition = "input.typeOfEndpoint == 'binary' &&  input.tCostsDependOnEvent == 'Yes'",
                                                                                 numericInput("costEvent_t1", "Treatment costs over patient time horizon if the primary outcome occurs",
                                                                                              value = 100, min = NA, max = NA, step = 10),
                                                                                 
                                                                                 numericInput("costNotEvent_t1", "Treatment costs over patient time horizon if the primary outcome does not occur",
                                                                                              value = 100, min = NA, max = NA, step = 10) ),
                                                                
                                                                # continuous and survival
                                                                conditionalPanel(condition = "input.typeOfEndpoint == 'continuous' || input.typeOfEndpoint == 'survival'  ",
                                                                                 numericInput("treatmentCostsMonthly_t1", "Treatment costs per month",
                                                                                              value = 100, min = NA, max = NA, step = 10) ),
                                                                # survival
                                                                conditionalPanel(condition = "input.typeOfEndpoint == 'survival'  ",
                                                                                 
                                                                                 selectInput(inputId = "treatUntilProgression_t1",  "Are individuals always treated until progression under this treatment?",
                                                                                             choices = c("Yes" = "Yes",
                                                                                                         "No" = "No"),
                                                                                             selected = "Yes"),
                                                                                 conditionalPanel(condition = "input.treatUntilProgression_t1 == 'No' ",
                                                                                 numericInput("maxDurationOfTreatmentMonths_t1", "What is the maximum number of months that this treatment will be given?",
                                                                                              value = 12, min = 0)
                                                                                 ))
                                                                
                                               ) # end Cost inputs for baseline (t1)
                                             ) # end baseline (t1) wellPanel
                                      ), # end baseline column
                                      
                                      # intervention 1 column (non-epi inputs)
                                      column(3,
                                             wellPanel(
                                               
                                               h4("Intervention 1"),
                                               textInput("nameOf_t2", "Name of intervention", 
                                                         value = "early PTP"),
                                               
                                               numericInput("utilisation_t2", "Current level of utilisation (%)",
                                                            value = 0, min = 0, max = 100, step = 0.1),
                                               
                                               
                                               # Cost inputs for t2
                                               conditionalPanel(condition = "input.outcomeExpression == 'netHealth'",
                                                                
                                                                #binary costs (dont depend on outcome)
                                                                conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.tCostsDependOnEvent == 'No'",
                                                                                 numericInput("cost_t2", "Treatment costs over patient time horizon",
                                                                                              value = 100, min = NA, max = NA, step = 10) ),
                                                                #binary costs (DO depend on outcome)
                                                                conditionalPanel(condition = "input.typeOfEndpoint == 'binary' &&  input.tCostsDependOnEvent == 'Yes'",
                                                                                 numericInput("costEvent_t2", "Treatment costs over patient time horizon if the primary outcome occurs",
                                                                                              value = 100, min = NA, max = NA, step = 10),
                                                                                 
                                                                                 numericInput("costNotEvent_t2", "Treatment costs over patient time horizon if the primary outcome does not occur",
                                                                                              value = 100, min = NA, max = NA, step = 10) ),
                                                                
                                                                # continuous and survival
                                                                conditionalPanel(condition = "input.typeOfEndpoint == 'continuous' || input.typeOfEndpoint == 'survival'  ",
                                                                                 numericInput("treatmentCostsMonthly_t2", "Treatment costs per month",
                                                                                              value = 100, min = NA, max = NA, step = 10) ),
                                                                # survival
                                                                conditionalPanel(condition = "input.typeOfEndpoint == 'survival'  ",
                                                                                 
                                                                                 selectInput(inputId = "treatUntilProgression_t2",  "Are individuals always treated until progression under this treatment?",
                                                                                             choices = c("Yes" = "Yes",
                                                                                                         "No" = "No"),
                                                                                             selected = "Yes"),
                                                                                 conditionalPanel(condition = "input.treatUntilProgression_t2 == 'No' ",
                                                                                                  numericInput("maxDurationOfTreatmentMonths_t2", "What is the maximum number of months that this treatment will be given?",
                                                                                                               value = 12, min = 0)
                                                                                 ))
                                               ) # end Cost inputs for t2
                                               
                                             ) # end intervention 1 (t2) wellPanel
                                             
                                      ), # end intervention 1 column (non-epi inputs)
                                      
                                      # conditional
                                      # intervention 2 (t3) column (non-epi inputs)
                                      column(3,
                                             conditionalPanel(condition = "input.numberOfTreatments >= 3",
                                                              wellPanel(
                                                                
                                                                h4("Intervention 2"),
                                                                # display if: numberOfTreatments >= 3
                                                                textInput("nameOf_t3", "Name of intervention", 
                                                                          value = "intervention 2"),
                                                                
                                                                # display if: numberOfTreatments >= 3
                                                                numericInput("utilisation_t3", "Current level of utilisation (%)",
                                                                             value = 0, min = 0, max = 100, step = 0.1),
                                                                
                                                                # Cost inputs for t3
                                                                conditionalPanel(condition = "input.outcomeExpression == 'netHealth'",
                                                                                 
                                                                                 #binary costs (dont depend on outcome)
                                                                                 conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.tCostsDependOnEvent == 'No'",
                                                                                                  numericInput("cost_t3", "Treatment costs over patient time horizon",
                                                                                                               value = 100, min = NA, max = NA, step = 10) ),
                                                                                 #binary costs (DO depend on outcome)
                                                                                 conditionalPanel(condition = "input.typeOfEndpoint == 'binary' &&  input.tCostsDependOnEvent == 'Yes'",
                                                                                                  numericInput("costEvent_t3", "Treatment costs over patient time horizon if the primary outcome occurs",
                                                                                                               value = 100, min = NA, max = NA, step = 10),
                                                                                                  
                                                                                                  numericInput("costNotEvent_t3", "Treatment costs over patient time horizon if the primary outcome does not occur",
                                                                                                               value = 100, min = NA, max = NA, step = 10) ),
                                                                                 
                                                                                 # continuous and survival
                                                                                 conditionalPanel(condition = "input.typeOfEndpoint == 'continuous' || input.typeOfEndpoint == 'survival'  ",
                                                                                                  numericInput("treatmentCostsMonthly_t3", "Treatment costs per month",
                                                                                                               value = 100, min = NA, max = NA, step = 10) ),
                                                                                 # survival
                                                                                 conditionalPanel(condition = "input.typeOfEndpoint == 'survival'  ",
                                                                                                  
                                                                                                  selectInput(inputId = "treatUntilProgression_t3",  "Are individuals always treated until progression under this treatment?",
                                                                                                              choices = c("Yes" = "Yes",
                                                                                                                          "No" = "No"),
                                                                                                              selected = "Yes"),
                                                                                                  conditionalPanel(condition = "input.treatUntilProgression_t3 == 'No' ",
                                                                                                                   numericInput("maxDurationOfTreatmentMonths_t3", "What is the maximum number of months that this treatment will be given?",
                                                                                                                                value = 12, min = 0)
                                                                                                  ))
                                                                ) # end cost inputs t3
                                                                
                                                              ) # end intervention 2 (t3) wellPanel
                                             ) # end intervention 2 (t3) conditionalPanel
                                      ), # end intervention 2 column (non-epi inputs)
                                      
                                      # conditional
                                      # intervention 3 (t4) column (non-epi inputs)
                                      column(3,
                                             conditionalPanel(condition = "input.numberOfTreatments >= 4",
                                                              wellPanel(
                                                                
                                                                h4("Intervention 3"),
                                                                # display if: numberOfTreatments >= 4
                                                                textInput("nameOf_t4", "Name of intervention", 
                                                                          value = "intervention 3"),
                                                                # display if: numberOfTreatments >= 4
                                                                numericInput("utilisation_t4", "Current level of utilisation (%)",
                                                                             value = 0, min = 0, max = 100, step = 0.1),
                                                                
                                                                # Cost inputs for t4
                                                                conditionalPanel(condition = "input.outcomeExpression == 'netHealth'",
                                                                                 
                                                                                 #binary costs (dont depend on outcome)
                                                                                 conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.tCostsDependOnEvent == 'No'",
                                                                                                  numericInput("cost_t4", "Treatment costs over patient time horizon",
                                                                                                               value = 100, min = NA, max = NA, step = 10) ),
                                                                                 #binary costs (DO depend on outcome)
                                                                                 conditionalPanel(condition = "input.typeOfEndpoint == 'binary' &&  input.tCostsDependOnEvent == 'Yes'",
                                                                                                  numericInput("costEvent_t4", "Treatment costs over patient time horizon if the primary outcome occurs",
                                                                                                               value = 100, min = NA, max = NA, step = 10),
                                                                                                  
                                                                                                  numericInput("costNotEvent_t4", "Treatment costs over patient time horizon if the primary outcome does not occur",
                                                                                                               value = 100, min = NA, max = NA, step = 10) ),
                                                                                 
                                                                                 # continuous and survival
                                                                                 conditionalPanel(condition = "input.typeOfEndpoint == 'continuous' || input.typeOfEndpoint == 'survival'  ",
                                                                                                  numericInput("treatmentCostsMonthly_t4", "Treatment costs per month",
                                                                                                               value = 100, min = NA, max = NA, step = 10) ),
                                                                                 # survival
                                                                                 conditionalPanel(condition = "input.typeOfEndpoint == 'survival'  ",
                                                                                                  
                                                                                                  selectInput(inputId = "treatUntilProgression_t4",  "Are individuals always treated until progression under this treatment?",
                                                                                                              choices = c("Yes" = "Yes",
                                                                                                                          "No" = "No"),
                                                                                                              selected = "Yes"),
                                                                                                  conditionalPanel(condition = "input.treatUntilProgression_t4 == 'No' ",
                                                                                                                   numericInput("maxDurationOfTreatmentMonths_t4", "What is the maximum number of months that this treatment will be given?",
                                                                                                                                value = 12, min = 0)
                                                                                                  ))
                                                                ) # end cost inputs t4
                                                                
                                                              ) # end intervention 3 (t4) wellPanel
                                             ) # end intervention 3 (t4) conditionalPanel
                                      ) # end intervention 3 column (non-epi inputs)
                                      
                                    ), # end top level fluid row (non-epi inputs)
                                    
                                    ## lowest level fluid row for "epi treatment inputs" width of 3 column for each
                                    fluidRow(
                                      
                                      # finish changing names!
                                      # need to do something about t1 = t0, t2 = t1,...
                                      # treatment 1 = baseline treatment, treatment 2 = intervention 1,...
                                      column(3, 
                                             #
                                             # Baseline treatment (formerly: treatment 1 )
                                             #
                                             
                                             
                                             wellPanel(style = "background-color:LightSkyBlue;",  # add CSS code to change colour of wellPanel
                                                       h4("Baseline treatment"),
                                                       # new baseline binary inputs 
                                                       conditionalPanel(condition = "input.typeOfEndpoint == 'binary'",
                                                                        # choose method to input 
                                                                        selectInput(inputId = "baselineInput",  "Choose method of entering baseline probability of outcome",
                                                                                    choices = c("Upper and lower 95% range" = "range",
                                                                                                "Number of events and number at risk" = "eventsNonEvents"),
                                                                                    selected = "range"), 
                                                                        
                                                                        # if eventsNonEvents
                                                                        conditionalPanel(condition = "input.baselineInput == 'eventsNonEvents'",
                                                                                         numericInput("nEvents", "Number of events", 
                                                                                                      value = 10, min = 1, max = NA),
                                                                                         numericInput("nAtRisk", "Number at risk", 
                                                                                                      value = 40, min = 1, max = NA)
                                                                        ), # end eventsNonEvents inputs
                                                                        
                                                                        # if range
                                                                        conditionalPanel(condition = "input.baselineInput == 'range'",
                                                                                         sliderInput("baselineRange", "Select a plausible 95% range for the baseline probability of outcome",
                                                                                                     step = 0.01, min = 0, max = 1, value = c(0.248, 0.469))
                                                                        ) # end range inputs
                                                       ), # end new baseline binary inputs
                                                       
                                                       
                                                       # continuous inputs for baseline (t1)
                                                       # new baseline continuous inputs - - same as old ones!
                                                       conditionalPanel(condition = "input.typeOfEndpoint == 'continuous'",
                                                                        p("No baseline input required for continuous outcomes")),
                                                       
                                                       # survival inputs for baseline (t1)
                                                       # new survival inputs for baseline (t1) - same as old ones!
                                                       conditionalPanel(condition = "input.typeOfEndpoint == 'survival'",
                                                                        
                                                                        selectInput(inputId = "survivalType", label = "Type of survival distribution", 
                                                                                    choices = c("Exponential" = "exponential", 
                                                                                                "Weibull" = "weibull"),
                                                                                    selected = "exponential"),
                                                                        
                                                                        numericInput("lambda_t1", "Lambda (scale) parameter for baseline treatment (natural scale)",
                                                                                     value = 5, min = 0, max = NA, step = 1),
                                                                        
                                                                        conditionalPanel(condition = "input.survivalType == 'weibull'",
                                                                                         numericInput("gamma_t1", "Gamma (shape) parameter for baseline treatment (natural scale)",
                                                                                                      value = 1.1, min = 0, max = NA, step = 0.1))
                                                       ) # end survival inputs for t1
                                                       
                                                     
                                             ) # end wellPanel t1
                                      ), # end column t1
                                      
                                      column(3, 
                                             # epi inputs 
                                             # Intervention 1 (formerly: treatment 2 )
                                             #
                                             
                                             wellPanel(style = "background-color:LightSkyBlue;",  # add CSS code to change colour of wellPanel
                                                       h4("Intervention 1"),
                                                       
                                                       # new binary epi inputs for intervention 1
                                                       conditionalPanel(condition = "input.typeOfEndpoint == 'binary'",
                                                                        
                                                                        #choose input scale
                                                                        selectInput(inputId = "binaryRelativeScale_t2",  "Scale for relative effect",
                                                                                    choices = c("Odds ratio" = "OR",
                                                                                                "Risk ratio" = "RR",
                                                                                                "Risk difference" = "RD"),
                                                                                    selected = "Odds ratio"),
                                                                        
                                                                        # remove halfnormal option!
                                                                        conditionalPanel(condition = "input.typeOfEndpoint != 'binary'",
                                                                        # decide halfnorm / norm distribution
                                                                        selectInput(inputId = "binaryDist_t2",  "Compared to the baseline, is it expected that this intervention is either always better, always worse, or is there uncertainty about which is better on the primary outcome?",
                                                                                    choices = c("The intervention is known to be strictly better" = "alwaysPositive",
                                                                                                "The intervention is known to be strictly worse" = "alwaysNegative",
                                                                                                "There is uncertainty about whether the intervention is better/worse" = "norm"),
                                                                                    selected = "norm"),
                                                                        # if half Normal dist
                                                                        #
                                                                        # if alwaysPositive half normal
                                                                        conditionalPanel(condition = "input.binaryDist_t2 == 'alwaysPositive'",
                                                                                         
                                                                                         # OR
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t2 == 'OR'",
                                                                                                          sliderInput("ORHalfNorm_t2", "Select a plausible 95% range for the odds ratio. The lower bound is set to 1",
                                                                                                                      step = 0.01, min = 1, max = 5, value = 1.5)),
                                                                                         # RR
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t2 == 'RR'",
                                                                                                          sliderInput("RRHalfNorm_t2", "Select a plausible 95% range for the risk ratio. The lower bound is set to 1",
                                                                                                                      step = 0.01, min = 1, max = 5, value = 1.5)),
                                                                                         # RD
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t2 == 'RD'",
                                                                                                          sliderInput("RDHalfNorm_t2", "Select a plausible 95% range for the risk difference. The lower bound is set to 0",
                                                                                                                      post = "%", step = 0.01, min = 0, max = 50, value = 10))
                                                                                         
                                                                        ), # end alwaysPositive half normal
                                                                        
                                                                        # if alwaysNegative half normal
                                                                        conditionalPanel(condition = "input.binaryDist_t2 == 'alwaysNegative'",
                                                                                         # OR
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t2 == 'OR'",
                                                                                                          sliderInput("ORHalfNorm_t2", "Select a plausible 95% range for the odds ratio. The upper bound is set to 1",
                                                                                                                      step = 0.01, min = 0, max = 1, value = 0.5)),
                                                                                         # RR
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t2 == 'RR'",
                                                                                                          sliderInput("RRHalfNorm_t2", "Select a plausible 95% range for the risk ratio. The upper bound is set to 1",
                                                                                                                      step = 0.01, min = 0, max = 1, value = 0.5)),
                                                                                         # RD
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t2 == 'RD'",
                                                                                                          sliderInput("RDHalfNorm_t2", "Select a plausible 95% range for the risk difference. The upper bound is set to 0%",
                                                                                                                      post = "%", step = 0.01, min = -50, max = 0, value = 0.5))
                                                                        ) # end alwaysNegative half normal
                                                                        ),
                                                                        
                                                                        # if normal dist
                                                                        #
                                                                        conditionalPanel(condition = "input.binaryDist_t2 == 'norm'",
                                                                                         #
                                                                                         # for OR (norm) # CRASH default inputs
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t2 == 'OR'",
                                                                                                          sliderInput("OR_t2", "Select a plausible 95% range for the odds ratio",
                                                                                                                      step = 0.01, min = 0.01, max = 5, value = c(0.71, 1.18))),
                                                                                         # for RR (norm)
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t2 == 'RR'",
                                                                                                          sliderInput("RR_t2", "Select a plausible 95% range for the risk ratio",
                                                                                                                      step = 0.01, min = 0.01, max = 5, value = c(0.9, 1.1))),
                                                                                         # for RD (norm)
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t2 == 'RD'",
                                                                                                          sliderInput("RD_t2", "Select a plausible 95% range for the risk difference",
                                                                                                                      post = "%",step = 0.01, min = -50, max = 50, value = c(-5, 5)))
                                                                        ) # end normal dist conditional panel
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                       ), # end binary epi inputs intervention 1
                                                       
                                                       # new continuous epi inputs for intervention 1
                                                       conditionalPanel(condition = "input.typeOfEndpoint == 'continuous'",
                                                                        
                                                                        # choose method to input results
                                                                        selectInput(inputId = "continuousInput_t2",  "Choose method of entering mean difference",
                                                                                    choices = c("Upper and lower 95% range" = "range",
                                                                                                "Mean and standard error" = "meanAndSE"),
                                                                                    selected = "range"), 
                                                                        
                                                                        # if normal - mean and se
                                                                        conditionalPanel(condition = "input.continuousInput_t2 == 'meanAndSE'",
                                                                                         numericInput("continMean_t2", "Expected mean difference", 
                                                                                                      value = 0, min = NA, max = NA),
                                                                                         numericInput("continSE_t2", "Standard error for mean difference", 
                                                                                                      value = 0.5, min = 0, max = NA)
                                                                        ), # end normal mean and se inputs
                                                                        
                                                                        # if range selected
                                                                        conditionalPanel(condition = "input.continuousInput_t2 == 'range'",
                                                                                         
                                                                                         # if normal range selected
                                                                                         conditionalPanel(condition = "input.continDist_t2 == 'norm'",
                                                                                                          sliderInput("MD_t2", "Select a plausible 95% range for the mean difference",
                                                                                                                      step = 0.01, min = -20, max = 20, value = c(-2, 2))
                                                                                         ), # end normal range
                                                                                         
                                                                                         # remove half normal option!
                                                                                         conditionalPanel(condition = "input.typeOfEndpoint != 'continuous'",
                                                                                         # decide halfnorm / norm distribution - only allow this if range selected - otherwise mean and se is ambiguous
                                                                                         selectInput(inputId = "continDist_t2",  "Compared to the baseline, is it expected that this intervention is either always better, always worse, or is there uncertainty about which is better on the primary outcome?",
                                                                                                     choices = c("The intervention is known to be strictly better" = "alwaysPositive",
                                                                                                                 "The intervention is known to be strictly worse" = "alwaysNegative",
                                                                                                                 "There is uncertainty about whether the intervention is better/worse" = "norm"),
                                                                                                     selected = "norm"),
                                                                                         
                                                                                         
                                                                                         
                                                                                         # if half Normal range selected
                                                                                         #
                                                                                         # if alwaysPositive half normal
                                                                                         conditionalPanel(condition = "input.continDist_t2 == 'alwaysPositive'",
                                                                                                          # only range input
                                                                                                          sliderInput("MDHalfNorm_t2", "Select a plausible 95% range for the mean difference. The lower bound is set to 0",
                                                                                                                      step = 0.01, min = 0, max = 20, value = 0.5)
                                                                                         ), # end alwaysPositive half normal
                                                                                         #
                                                                                         # if alwaysNegative half normal
                                                                                         conditionalPanel(condition = "input.continDist_t2 == 'alwaysNegative'",
                                                                                                          # only range input
                                                                                                          sliderInput("MDHalfNorm_t2", "Select a plausible 95% range for the mean difference. The upper bound is set to 0",
                                                                                                                      step = 0.01, min = -20, max = 0, value = 0.5)
                                                                                         ) # end alwaysNegative half normal
                                                                                         
                                                                                         ) # end conditional panel to hide half normal inputs
                                                                                         
                                                                        ) # end range input
                                                                        
                                                                        
                                                                        
                                                                        
                                                       ), # end continuous epi inputs intervention 1
                                                       
                                                       # new survival epi inputs for intervention 1
                                                       conditionalPanel(condition = "input.typeOfEndpoint == 'survival'",
                                                                        
                                                                        # if normal dist
                                                                        #
                                                                        conditionalPanel(condition = "input.survivalDist_t2 == 'norm'",
                                                                                         # for HR (norm) # 
                                                                                         sliderInput("HR_t2", "Select a plausible 95% range for the hazard ratio",
                                                                                                     step = 0.01, min = 0.01, max = 5, value = c(0.71, 1.18))
                                                                        ), # end normal dist conditional panel
                                                                        
                                                                        
                                                                        # hide half normal inputs
                                                                        conditionalPanel(condition = "input.typeOfEndpoint != 'survival'",
                                                                        # decide halfnorm / norm distribution
                                                                        selectInput(inputId = "survivalDist_t2",  "Compared to the baseline, is it expected that this intervention is either always better, always worse, or is there uncertainty about which is better on the primary outcome?",
                                                                                    choices = c("The intervention is known to be strictly better" = "alwaysPositive",
                                                                                                "The intervention is known to be strictly worse" = "alwaysNegative",
                                                                                                "There is uncertainty about whether the intervention is better/worse" = "norm"),
                                                                                    selected = "norm"),
                                                                        
                                                                        # if half Normal dist
                                                                        #
                                                                        # if alwaysPositive half normal
                                                                        conditionalPanel(condition = "input.survivalDist_t2 == 'alwaysPositive'",
                                                                                         # HR half normal
                                                                                         sliderInput("HRHalfNorm_t2", "Select a plausible 95% range for the hazard ratio. The lower bound is set to 1",
                                                                                                     step = 0.01, min = 1, max = 5, value = 1.5)
                                                                                         
                                                                        ), # end alwaysPositive half normal
                                                                        
                                                                        # if alwaysNegative half normal
                                                                        conditionalPanel(condition = "input.survivalDist_t2 == 'alwaysNegative'",
                                                                                         # HR half normal
                                                                                         sliderInput("HRHalfNorm_t2", "Select a plausible 95% range for the hazard ratio. The upper bound is set to 1",
                                                                                                     step = 0.01, min = 0, max = 1, value = 0.5)
                                                                        ) # end alwaysNegative half normal
                                                                        ) # end hide survival half normal inputs
                                                                        
                                                       ), # end survival epi inputs intervention 1
                                                       
                                                       conditionalPanel(condition = "input.outcomeExpression != 'netHealth'",
                                                                        numericInput("MCD_t2", "MCD",
                                                                                     value = 0, min = NA, max = NA, step = 0.05))
                                                       
                                                       
                                             ) # end epi input intervention 1 wellPanel (t2)
                                      ), # end epi input intervention 1 column (t2)
                                      
                                      column(3, 
                                             
                                             
                                             # epi inputs 
                                             # Intervention 2 (formerly: t3 )
                                             #
                                             conditionalPanel(condition = "input.numberOfTreatments >= 3",
                                             
                                             wellPanel(style = "background-color:LightSkyBlue;",  # add CSS code to change colour of wellPanel
                                                       h4("Intervention 2"),
                                                       
                                                       # new binary epi inputs for intervention 2
                                                       conditionalPanel(condition = "input.typeOfEndpoint == 'binary'",
                                                                        
                                                                        #choose input scale
                                                                        selectInput(inputId = "binaryRelativeScale_t3",  "Scale for relative effect",
                                                                                    choices = c("Odds ratio" = "OR",
                                                                                                "Risk ratio" = "RR",
                                                                                                "Risk difference" = "RD"),
                                                                                    selected = "Odds ratio"),
                                                                        
                                                                        # if normal dist
                                                                        #
                                                                        conditionalPanel(condition = "input.binaryDist_t3 == 'norm'",
                                                                                         #
                                                                                         # for OR (norm) # CRASH default inputs
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t3 == 'OR'",
                                                                                                          sliderInput("OR_t3", "Select a plausible 95% range for the odds ratio",
                                                                                                                      step = 0.01, min = 0.01, max = 5, value = c(0.71, 1.18))),
                                                                                         # for RR (norm)
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t3 == 'RR'",
                                                                                                          sliderInput("RR_t3", "Select a plausible 95% range for the risk ratio",
                                                                                                                      step = 0.01, min = 0.01, max = 5, value = c(0.9, 1.1))),
                                                                                         # for RD (norm)
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t3 == 'RD'",
                                                                                                          sliderInput("RD_t3", "Select a plausible 95% range for the risk difference",
                                                                                                                      post = "%",step = 0.01, min = -50, max = 50, value = c(-5, 5)))
                                                                        ), # end normal dist conditional panel
                                                                        
                                                                        
                                                                        # hide half normal inputs
                                                                        conditionalPanel(condition = "input.typeOfEndpoint != 'binary'",
                                                                        # decide halfnorm / norm distribution
                                                                        selectInput(inputId = "binaryDist_t3",  "Compared to the baseline, is it expected that this intervention is either always better, always worse, or is there uncertainty about which is better on the primary outcome?",
                                                                                    choices = c("The intervention is known to be strictly better" = "alwaysPositive",
                                                                                                "The intervention is known to be strictly worse" = "alwaysNegative",
                                                                                                "There is uncertainty about whether the intervention is better/worse" = "norm"),
                                                                                    selected = "norm"),
                                                                        
                                                                        
                                                                        
                                                                        # if half Normal dist
                                                                        #
                                                                        # if alwaysPositive half normal
                                                                        conditionalPanel(condition = "input.binaryDist_t3 == 'alwaysPositive'",
                                                                                         
                                                                                         # OR
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t3 == 'OR'",
                                                                                                          sliderInput("ORHalfNorm_t3", "Select a plausible 95% range for the odds ratio. The lower bound is set to 1",
                                                                                                                      step = 0.01, min = 1, max = 5, value = 1.5)),
                                                                                         # RR
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t3 == 'RR'",
                                                                                                          sliderInput("RRHalfNorm_t3", "Select a plausible 95% range for the risk ratio. The lower bound is set to 1",
                                                                                                                      step = 0.01, min = 1, max = 5, value = 1.5)),
                                                                                         # RD
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t3 == 'RD'",
                                                                                                          sliderInput("RDHalfNorm_t3", "Select a plausible 95% range for the risk difference. The lower bound is set to 0",
                                                                                                                      post = "%", step = 0.01, min = 0, max = 50, value = 10))
                                                                                         
                                                                        ), # end alwaysPositive half normal
                                                                        
                                                                        # if alwaysNegative half normal
                                                                        conditionalPanel(condition = "input.binaryDist_t3 == 'alwaysNegative'",
                                                                                         # OR
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t3 == 'OR'",
                                                                                                          sliderInput("ORHalfNorm_t3", "Select a plausible 95% range for the odds ratio. The upper bound is set to 1",
                                                                                                                      step = 0.01, min = 0, max = 1, value = 0.5)),
                                                                                         # RR
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t3 == 'RR'",
                                                                                                          sliderInput("RRHalfNorm_t3", "Select a plausible 95% range for the risk ratio. The upper bound is set to 1",
                                                                                                                      step = 0.01, min = 0, max = 1, value = 0.5)),
                                                                                         # RD
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t3 == 'RD'",
                                                                                                          sliderInput("RDHalfNorm_t3", "Select a plausible 95% range for the risk difference. The upper bound is set to 0%",
                                                                                                                      post = "%", step = 0.01, min = -50, max = 0, value = 0.5))
                                                                        ) # end alwaysNegative half normal
                                                                        ) # end hide half normal binary inputs
                                                                        
                                                                        
                                                                        
                                                                        
                                                       ), # end binary epi inputs intervention 2
                                                       
                                                       # new continuous epi inputs for intervention 2
                                                       conditionalPanel(condition = "input.typeOfEndpoint == 'continuous'",
                                                                        
                                                                        # choose method to input results
                                                                        selectInput(inputId = "continuousInput_t3",  "Choose method of entering mean difference",
                                                                                    choices = c("Upper and lower 95% range" = "range",
                                                                                                "Mean and standard error" = "meanAndSE"),
                                                                                    selected = "range"), 
                                                                        
                                                                        # if normal - mean and se
                                                                        conditionalPanel(condition = "input.continuousInput_t3 == 'meanAndSE'",
                                                                                         numericInput("continMean_t3", "Expected mean difference", 
                                                                                                      value = 0, min = NA, max = NA),
                                                                                         numericInput("continSE_t3", "Standard error for mean difference", 
                                                                                                      value = 0.5, min = 0, max = NA)
                                                                        ), # end normal mean and se inputs
                                                                        
                                                                        # if range selected
                                                                        conditionalPanel(condition = "input.continuousInput_t3 == 'range'",
                                                                                         
                                                                                         # if normal range selected
                                                                                         conditionalPanel(condition = "input.continDist_t3 == 'norm'",
                                                                                                          sliderInput("MD_t3", "Select a plausible 95% range for the mean difference",
                                                                                                                      step = 0.01, min = -20, max = 20, value = c(-2, 2))
                                                                                         ), # end normal range
                                                                                         
                                                                                         
                                                                                         conditionalPanel(condition = "input.typeOfEndpoint != 'continuous'",
                                                                                         # decide halfnorm / norm distribution - only allow this if range selected - otherwise mean and se is ambiguous
                                                                                         selectInput(inputId = "continDist_t3",  "Compared to the baseline, is it expected that this intervention is either always better, always worse, or is there uncertainty about which is better on the primary outcome?",
                                                                                                     choices = c("The intervention is known to be strictly better" = "alwaysPositive",
                                                                                                                 "The intervention is known to be strictly worse" = "alwaysNegative",
                                                                                                                 "There is uncertainty about whether the intervention is better/worse" = "norm"),
                                                                                                     selected = "norm"),
                                                                                         
                                                                                         
                                                                                         # if half Normal range selected
                                                                                         #
                                                                                         # if alwaysPositive half normal
                                                                                         conditionalPanel(condition = "input.continDist_t3 == 'alwaysPositive'",
                                                                                                          # only range input
                                                                                                          sliderInput("MDHalfNorm_t3", "Select a plausible 95% range for the mean difference. The lower bound is set to 0",
                                                                                                                      step = 0.01, min = 0, max = 20, value = 0.5)
                                                                                         ), # end alwaysPositive half normal
                                                                                         #
                                                                                         # if alwaysNegative half normal
                                                                                         conditionalPanel(condition = "input.continDist_t3 == 'alwaysNegative'",
                                                                                                          # only range input
                                                                                                          sliderInput("MDHalfNorm_t3", "Select a plausible 95% range for the mean difference. The upper bound is set to 0",
                                                                                                                      step = 0.01, min = -20, max = 0, value = 0.5)
                                                                                         ) # end alwaysNegative half normal
                                                                                         ) # end hide half normal
                                                                                         
                                                                        ) # end range inputs
                                                                        
                                                                        
                                                                        
                                                                        
                                                       ), # end continuous epi inputs intervention 2
                                                       
                                                       # new survival epi inputs for intervention 2
                                                       conditionalPanel(condition = "input.typeOfEndpoint == 'survival'",
                                                                        
                                                                        # if normal dist
                                                                        #
                                                                        conditionalPanel(condition = "input.survivalDist_t3 == 'norm'",
                                                                                         # for HR (norm) # 
                                                                                         sliderInput("HR_t3", "Select a plausible 95% range for the hazard ratio",
                                                                                                     step = 0.01, min = 0.01, max = 5, value = c(0.71, 1.18))
                                                                        ), # end normal dist conditional panel
                                                                        
                                                                        
                                                                        conditionalPanel(condition = "input.typeOfEndpoint != 'survival'",
                                                                        # decide halfnorm / norm distribution
                                                                        selectInput(inputId = "survivalDist_t3",  "Compared to the baseline, is it expected that this intervention is either always better, always worse, or is there uncertainty about which is better on the primary outcome?",
                                                                                    choices = c("The intervention is known to be strictly better" = "alwaysPositive",
                                                                                                "The intervention is known to be strictly worse" = "alwaysNegative",
                                                                                                "There is uncertainty about whether the intervention is better/worse" = "norm"),
                                                                                    selected = "norm"),
                                                                        
                                                                       
                                                                        # if half Normal dist
                                                                        #
                                                                        # if alwaysPositive half normal
                                                                        conditionalPanel(condition = "input.survivalDist_t3 == 'alwaysPositive'",
                                                                                         # HR half normal
                                                                                         sliderInput("HRHalfNorm_t3", "Select a plausible 95% range for the hazard ratio. The lower bound is set to 1",
                                                                                                     step = 0.01, min = 1, max = 5, value = 1.5)
                                                                                         
                                                                        ), # end alwaysPositive half normal
                                                                        
                                                                        # if alwaysNegative half normal
                                                                        conditionalPanel(condition = "input.survivalDist_t3 == 'alwaysNegative'",
                                                                                         # HR half normal
                                                                                         sliderInput("HRHalfNorm_t3", "Select a plausible 95% range for the hazard ratio. The upper bound is set to 1",
                                                                                                     step = 0.01, min = 0, max = 1, value = 0.5)
                                                                        ) # end alwaysNegative half normal
                                                                        ) # end hide half normal
                                                                        
                                                       ), # end survival epi inputs intervention 2
                                                       
                                                       conditionalPanel(condition = "input.outcomeExpression != 'netHealth'",
                                                       numericInput("MCD_t3", "MCD",
                                                                    value = 0, min = NA, max = NA, step = 0.05))
                                                       
                                                       
                                                       
                                             ) # end epi input intervention 2 wellPanel (t3)
                                      ) # end epi input intervention 2 conditionalPanel (t3)
               
                                      ), # end epi input intervention 2 column (t3)
                                      
                                      column(3,
                                             
                                             # epi inputs 
                                             # Intervention 3 (formerly: t4 )
                                             #
                                             conditionalPanel(condition = "input.numberOfTreatments >= 4",
                                                              
                                                              wellPanel(style = "background-color:LightSkyBlue;",  # add CSS code to change colour of wellPanel
                                                                        h4("Intervention 3"),
                                                                        
                                                                        # new binary epi inputs for intervention 3
                                                                        conditionalPanel(condition = "input.typeOfEndpoint == 'binary'",
                                                                                         
                                                                                         #choose input scale
                                                                                         selectInput(inputId = "binaryRelativeScale_t4",  "Scale for relative effect",
                                                                                                     choices = c("Odds ratio" = "OR",
                                                                                                                 "Risk ratio" = "RR",
                                                                                                                 "Risk difference" = "RD"),
                                                                                                     selected = "Odds ratio"),
                                                                                         
                                                                                         # if normal dist
                                                                                         #
                                                                                         conditionalPanel(condition = "input.binaryDist_t4 == 'norm'",
                                                                                                          #
                                                                                                          # for OR (norm) # CRASH default inputs
                                                                                                          conditionalPanel(condition = "input.binaryRelativeScale_t4 == 'OR'",
                                                                                                                           sliderInput("OR_t4", "Select a plausible 95% range for the odds ratio",
                                                                                                                                       step = 0.01, min = 0.01, max = 5, value = c(0.71, 1.18))),
                                                                                                          # for RR (norm)
                                                                                                          conditionalPanel(condition = "input.binaryRelativeScale_t4 == 'RR'",
                                                                                                                           sliderInput("RR_t4", "Select a plausible 95% range for the risk ratio",
                                                                                                                                       step = 0.01, min = 0.01, max = 5, value = c(0.9, 1.1))),
                                                                                                          # for RD (norm)
                                                                                                          conditionalPanel(condition = "input.binaryRelativeScale_t4 == 'RD'",
                                                                                                                           sliderInput("RD_t4", "Select a plausible 95% range for the risk difference",
                                                                                                                                       post = "%",step = 0.01, min = -50, max = 50, value = c(-5, 5)))
                                                                                         ), # end normal dist conditional panel
                                                                                         
                                                                                         
                                                                                         # hide half normal
                                                                                         conditionalPanel(condition = "input.typeOfEndpoint != 'binary'",
                                                                                         # decide halfnorm / norm distribution
                                                                                         selectInput(inputId = "binaryDist_t4",  "Compared to the baseline, is it expected that this intervention is either always better, always worse, or is there uncertainty about which is better on the primary outcome?",
                                                                                                     choices = c("The intervention is known to be strictly better" = "alwaysPositive",
                                                                                                                 "The intervention is known to be strictly worse" = "alwaysNegative",
                                                                                                                 "There is uncertainty about whether the intervention is better/worse" = "norm"),
                                                                                                     selected = "norm"),
                                                                                         
                                                                                         
                                                                                         
                                                                                         # if half Normal dist
                                                                                         #
                                                                                         # if alwaysPositive half normal
                                                                                         conditionalPanel(condition = "input.binaryDist_t4 == 'alwaysPositive'",
                                                                                                          
                                                                                                          # OR
                                                                                                          conditionalPanel(condition = "input.binaryRelativeScale_t4 == 'OR'",
                                                                                                                           sliderInput("ORHalfNorm_t4", "Select a plausible 95% range for the odds ratio. The lower bound is set to 1",
                                                                                                                                       step = 0.01, min = 1, max = 5, value = 1.5)),
                                                                                                          # RR
                                                                                                          conditionalPanel(condition = "input.binaryRelativeScale_t4 == 'RR'",
                                                                                                                           sliderInput("RRHalfNorm_t4", "Select a plausible 95% range for the risk ratio. The lower bound is set to 1",
                                                                                                                                       step = 0.01, min = 1, max = 5, value = 1.5)),
                                                                                                          # RD
                                                                                                          conditionalPanel(condition = "input.binaryRelativeScale_t4 == 'RD'",
                                                                                                                           sliderInput("RDHalfNorm_t4", "Select a plausible 95% range for the risk difference. The lower bound is set to 0",
                                                                                                                                       post = "%", step = 0.01, min = 0, max = 50, value = 10))
                                                                                                          
                                                                                         ), # end alwaysPositive half normal
                                                                                         
                                                                                         # if alwaysNegative half normal
                                                                                         conditionalPanel(condition = "input.binaryDist_t4 == 'alwaysNegative'",
                                                                                                          # OR
                                                                                                          conditionalPanel(condition = "input.binaryRelativeScale_t4 == 'OR'",
                                                                                                                           sliderInput("ORHalfNorm_t4", "Select a plausible 95% range for the odds ratio. The upper bound is set to 1",
                                                                                                                                       step = 0.01, min = 0, max = 1, value = 0.5)),
                                                                                                          # RR
                                                                                                          conditionalPanel(condition = "input.binaryRelativeScale_t4 == 'RR'",
                                                                                                                           sliderInput("RRHalfNorm_t4", "Select a plausible 95% range for the risk ratio. The upper bound is set to 1",
                                                                                                                                       step = 0.01, min = 0, max = 1, value = 0.5)),
                                                                                                          # RD
                                                                                                          conditionalPanel(condition = "input.binaryRelativeScale_t4 == 'RD'",
                                                                                                                           sliderInput("RDHalfNorm_t4", "Select a plausible 95% range for the risk difference. The upper bound is set to 0%",
                                                                                                                                       post = "%", step = 0.01, min = -50, max = 0, value = 0.5))
                                                                                         ) # end alwaysNegative half normal
                                                                                         ) # end conditional panel to hide half normal  
                                                                                         
                                                                                         
                                                                                         
                                                                                         
                                                                        ), # end binary epi inputs intervention 3
                                                                        
                                                                        # new continuous epi inputs for intervention 3
                                                                        conditionalPanel(condition = "input.typeOfEndpoint == 'continuous'",
                                                                                         
                                                                                         # choose method to input results
                                                                                         selectInput(inputId = "continuousInput_t4",  "Choose method of entering mean difference",
                                                                                                     choices = c("Upper and lower 95% range" = "range",
                                                                                                                 "Mean and standard error" = "meanAndSE"),
                                                                                                     selected = "range"), 
                                                                                         
                                                                                         # if normal - mean and se
                                                                                         conditionalPanel(condition = "input.continuousInput_t4 == 'meanAndSE'",
                                                                                                          numericInput("continMean_t4", "Expected mean difference", 
                                                                                                                       value = 0, min = NA, max = NA),
                                                                                                          numericInput("continSE_t4", "Standard error for mean difference", 
                                                                                                                       value = 0.5, min = 0, max = NA)
                                                                                         ), # end normal mean and se inputs
                                                                                         
                                                                                         # if range selected
                                                                                         conditionalPanel(condition = "input.continuousInput_t4 == 'range'",
                                                                                                          
                                                                                                          # if normal range selected
                                                                                                          conditionalPanel(condition = "input.continDist_t4 == 'norm'",
                                                                                                                           sliderInput("MD_t4", "Select a plausible 95% range for the mean difference",
                                                                                                                                       step = 0.01, min = -20, max = 20, value = c(-2, 2))
                                                                                                          ), # end normal range
                                                                                                          
                                                                                                          
                                                                                                          conditionalPanel(condition = "input.typeOfEndpoint != 'continuous'",
                                                                                                          # decide halfnorm / norm distribution - only allow this if range selected - otherwise mean and se is ambiguous
                                                                                                          selectInput(inputId = "continDist_t4",  "Compared to the baseline, is it expected that this intervention is either always better, always worse, or is there uncertainty about which is better on the primary outcome?",
                                                                                                                      choices = c("The intervention is known to be strictly better" = "alwaysPositive",
                                                                                                                                  "The intervention is known to be strictly worse" = "alwaysNegative",
                                                                                                                                  "There is uncertainty about whether the intervention is better/worse" = "norm"),
                                                                                                                      selected = "norm"),
                                                                                                          
                                                                                                          
                                                                                                          
                                                                                                          # if half Normal range selected
                                                                                                          #
                                                                                                          # if alwaysPositive half normal
                                                                                                          conditionalPanel(condition = "input.continDist_t4 == 'alwaysPositive'",
                                                                                                                           # only range input
                                                                                                                           sliderInput("MDHalfNorm_t4", "Select a plausible 95% range for the mean difference. The lower bound is set to 0",
                                                                                                                                       step = 0.01, min = 0, max = 20, value = 0.5)
                                                                                                          ), # end alwaysPositive half normal
                                                                                                          #
                                                                                                          # if alwaysNegative half normal
                                                                                                          conditionalPanel(condition = "input.continDist_t4 == 'alwaysNegative'",
                                                                                                                           # only range input
                                                                                                                           sliderInput("MDHalfNorm_t4", "Select a plausible 95% range for the mean difference. The upper bound is set to 0",
                                                                                                                                       step = 0.01, min = -20, max = 0, value = 0.5)
                                                                                                          ) # end alwaysNegative half normal
                                                                                                          ) # end hide half normal 
                                                                                                          
                                                                                         ) # end range inputs
                                                                                         
                                                                                         
                                                                                         
                                                                                         
                                                                        ), # end continuous epi inputs intervention 3
                                                                        
                                                                        # new survival epi inputs for intervention 3
                                                                        conditionalPanel(condition = "input.typeOfEndpoint == 'survival'",
                                                                                         
                                                                                         # if normal dist
                                                                                         #
                                                                                         conditionalPanel(condition = "input.survivalDist_t4 == 'norm'",
                                                                                                          # for HR (norm) # 
                                                                                                          sliderInput("HR_t4", "Select a plausible 95% range for the hazard ratio",
                                                                                                                      step = 0.01, min = 0.01, max = 5, value = c(0.71, 1.18))
                                                                                         ), # end normal dist conditional panel
                                                                                         
                                                                                         
                                                                                         conditionalPanel(condition = "input.typeOfEndpoint != 'survival'",
                                                                                         # decide halfnorm / norm distribution
                                                                                         selectInput(inputId = "survivalDist_t4",  "Compared to the baseline, is it expected that this intervention is either always better, always worse, or is there uncertainty about which is better on the primary outcome?",
                                                                                                     choices = c("The intervention is known to be strictly better" = "alwaysPositive",
                                                                                                                 "The intervention is known to be strictly worse" = "alwaysNegative",
                                                                                                                 "There is uncertainty about whether the intervention is better/worse" = "norm"),
                                                                                                     selected = "norm"),
                                                                                         
                                                                                         # if half Normal dist
                                                                                         #
                                                                                         # if alwaysPositive half normal
                                                                                         conditionalPanel(condition = "input.survivalDist_t4 == 'alwaysPositive'",
                                                                                                          # HR half normal
                                                                                                          sliderInput("HRHalfNorm_t4", "Select a plausible 95% range for the hazard ratio. The lower bound is set to 1",
                                                                                                                      step = 0.01, min = 1, max = 5, value = 1.5)
                                                                                                          
                                                                                         ), # end alwaysPositive half normal
                                                                                         
                                                                                         # if alwaysNegative half normal
                                                                                         conditionalPanel(condition = "input.survivalDist_t4 == 'alwaysNegative'",
                                                                                                          # HR half normal
                                                                                                          sliderInput("HRHalfNorm_t4", "Select a plausible 95% range for the hazard ratio. The upper bound is set to 1",
                                                                                                                      step = 0.01, min = 0, max = 1, value = 0.5)
                                                                                         ) # end alwaysNegative half normal
                                                                                         ) # end hide half normal
                                                                                         
                                                                        ), # end survival epi inputs intervention 3
                                                                        
                                                                        conditionalPanel(condition = "input.outcomeExpression != 'netHealth'",
                                                                        numericInput("MCD_t4", "MCD",
                                                                                     value = 0, min = NA, max = NA, step = 0.05))
                                                                        
                                                                        
                                                                        
                                                              ) # end epi input intervention 3 wellPanel (t4)
                                             ) # end epi input intervention 3 conditionalPanel (t4)
                                             
                                             
                                               
                                             
                                      ) # end epi input intervention 3 column
                                      
                                    ) # end lowest level fluid row (epi inputs)
                                    
                                  ) # end part 2 treatment inputs fluidPage
                                  
                         ), # end part 2 treatment inputs tabPanel
                         
                         ###################
                         # Part 3 other inputs
                         
                         tabPanel("Step 3: Proposed research", 
                                  
                                  br(),
                                  h4("Select appropriate values and then press 'Run analysis'"),
                                  strong("See the 'Input information' tab for detail on how to interpret the inputs"),
                                  fluidPage(
                                    br(),
                                    # top fluid row (non treatment inputs)
                                    fluidRow(
                                      
                                      
                                      column(3, 
                                             ##########
                                             # General inputs (1st panel): health system
                                             ##########
                                             
                                             wellPanel(
                                               
                                               h4("Proposed research study"),
                                               
                                               selectInput(inputId = "typeOfResearch", label = "Type of research", 
                                                           choices = c("RCT" = "RCT", 
                                                                       "Feasibility study" = "feasibility"),
                                                           selected = "RCT"),
                                               
                                               # RCT trial design inputs
                                               conditionalPanel(condition = "input.typeOfResearch == 'RCT'",
                                                                
                                                                sliderInput("durationOfResearch", "Expected duration of research (years)",
                                                                            min = 0, max = 25, step = 0.25, value = 5),
                                                                
                                                                numericInput("costResearchFunder", "Cost of research to funder",
                                                                             value = 2854000, min = 0, max = NA, step = 100),
                                                                
                                                                conditionalPanel(condition = "input.outcomeExpression == 'netHealth'",
                                                                                 numericInput("costHealthSystem", "Costs of research imposed on health system",
                                                                                              value = 1000000, min = 0, max = NA, step = 100) )
                                               ), # end RCT trial design conditional panel
                                               
                                               
                                               # Feasibility trial design inputs
                                               conditionalPanel(condition = "input.typeOfResearch == 'feasibility'",
                                                                
                                                                numericInput("probabilityOfDefinitiveResearch", "Probability of feasibility research leading to follow-up study",
                                                                             value = 0.5, min = 0, max = 1, step = 0.05),
                                                                
                                                                
                                                                sliderInput("durationOfResearchFeas", "Expected duration of feasibility research (years)",
                                                                            min = 0, max = 10, step = 0.25, value = 2),
                                                                
                                                                sliderInput("durationOfResearchDefinitive", "Expected duration of follow-up research (years)",
                                                                            min = 0, max = 25, step = 0.25, value = 5),
                                                                
                                                                numericInput("costResearchFunderFeas", "Costs of feasibility research to funder",
                                                                             value = 1000000, min = 0, max = NA, step = 100),
                                                                
                                                                numericInput("costResearchFunderDefinitive", "Costs of follow-up research to funder",
                                                                             value = 1000000, min = 0, max = NA, step = 100),
                                                                
                                                                conditionalPanel(condition = "input.outcomeExpression == 'netHealth'",
                                                                                 numericInput("costHealthSystemFeas", "Costs of feasibility research imposed on health system",
                                                                                              value = 1000000, min = 0, max = NA, step = 100),
                                                                                 
                                                                                 # display if: outcomeExpression == "netHealth"
                                                                                 numericInput("costHealthSystemDefinitive", "Costs of follow-up research imposed on health system",
                                                                                              value = 1000000, min = 0, max = NA, step = 100) )
                                               ), # end Feasibility trial design conditional panel
                                               
                                               
                                               sliderInput("timeInformation", "Time over which evidence would be valuable (years)",
                                                           min = 0, max = 30, step = 1, value = 15)
                                               
                                             ) # end of wellPanel 
                                      ), # end of proposed research study column
                                      
                                      column(3,
                                             ##########
                                             # General inputs (2st panel): Other inputs
                                             ##########
                                             
                                             wellPanel( # START "other inputs" well panel
                                               
                                               h4("Other inputs"),
                                               
                                               numericInput("discountRate", "Discount rate (%)",
                                                            value = 3.5, min = 0, max = 100, step = 0.1),
                                               
                                               numericInput("incidence", "Incidence per annum",
                                                            value = 8800, min = 0, max = NA, step = 20),
                                               
                                               textInput("currencySymbol", "Currency used in analysis", 
                                                         value = "£"),
                                               
                                               
                                               # hide this until it is fully coded! (condition for display can never be satisfied)
                                               # add warning about time taken to do this calculation?
                                               conditionalPanel(condition = "input.typeOfResearch == 'RCT111'", #"input.typeOfResearch == 'RCT'",
                                                                radioButtons(inputId = "reconsider", label = "Calculate the value of reconsidering the evidence?", 
                                                                             choices = c("Yes" = "Yes", 
                                                                                         "No" = "No"),
                                                                             selected = "No"))
                                               
                                               
                                             ) # end "other inputs" wellpanel 
                                             
                                      ), # end other inputs column
                                      
                                      
                                      column(3, 
                                             
                                             # conditional well panel
                                             conditionalPanel(condition = "input.outcomeExpression == 'netHealth'", # start conditional well panel
                                                              
                                                              wellPanel( # START wellPanel
                                                                
                                                                h4("Comprehensive meausure of health outcome"),
                                                                
                                                                numericInput("k", "Opportunity cost of health system expenditure",
                                                                             value = 15000, min = 0, max = NA, step = 500)
                                                                
                                                                # conditionalPanel(condition = "input.typeOfEndpoint == 'binary'",
                                                                #                  numericInput("INBBinaryEvent", "Net health effect of binary event occuring (in QALYs)",
                                                                #                               value = 2, min = NA, max = NA, step = 0.05)),
                                                                # 
                                                                # conditionalPanel(condition = "input.typeOfEndpoint == 'continuous'",
                                                                #                  numericInput("INBContinEvent", 
                                                                #                               "Net health effect of unit increase in continuous outcome (in QALYs)",
                                                                #                               value = 0.05, min = NA, max = NA, step = 0.05)),
                                                                # 
                                                                # # BUG!! If this has NA value then the app crashes
                                                                # conditionalPanel(condition = "input.typeOfEndpoint == 'survival'",
                                                                #                  numericInput("INBSurvivalEndpoint", "Net health effect of survival endpoint (in QALYs)",
                                                                #                               value = 0.5, min = NA, max = NA, step = 0.05))
                                                                
                                                              ) # end wellPanel
                                             ) # end conditional well panel
                                             
                                      ), # end net benefit inut column
                                      
                                      
                                      
                                      
                                      column(3, 
                                             ##########
                                             # Run buttons 
                                             ##########
                                             
                                             wellPanel(
                                               
                                               br(),
                                               actionButton("run", label = "Run analysis"),
                                               br(),
                                               br(),
                                               
                                               conditionalPanel(condition = "input.reconsider != 'Yes'",
                                                                p("Click once and go to the Results tab")),
                                               
                                               conditionalPanel(condition = "input.reconsider == 'Yes'",
                                                                p("Click once, and go to the Results tab. The calculation to reconsider the evidence can take up to 10 minutes to report."))
                                               
                                               
                                               # do not display MC simulations to user
                                               # just inputted 50000 directly into master() input
                                               # **check maximum number feasible
                                               #conditionalPanel(condition = "input.reconsider != 'Yes'",
                                               #                 numericInput("MCsims", "Number of simulations",
                                               #                              value = 50000, min = 0, max = 10000000, step = 500)),
                                               
                                               # do not display MC simulations to user
                                               # **check maximum number feasible
                                               # conditionalPanel(condition = "input.reconsider == 'Yes'",
                                               #                  
                                               #                  p("Note that this analysis will take between ## and ## minutes to report. This is due to the large number of simulations required"),
                                               #                  numericInput("MCsimsInner", "Number of simulations for inner loop",
                                               #                               value = 50000, min = 0, max = 10000000, step = 500),
                                               #                  
                                               #                  numericInput("MCsimsOuter", "Number of simulations for outer loop",
                                               #                               value = 50000, min = 0, max = 10000000, step = 500))
                                               
                                               
                                             ) # end well panel run button
                                      ) # end column 4: run button
                                      
                                    ) # end top fluid row
                                    
                                    
                                    
                                    
                                  ) # end of part 3 inputs fluidPage
                         ) # end of part 3 inputs subtab
                         
                         )  # end Inputs tabsetPanel   
                                    ), # end Inputs tabPanel 
    
    ##################
    # RESULTS page
    ##################
    tabPanel("Results", 
             
             
             # Headline
             ############
             br(),
             wellPanel(
               h4("Headline results"),
               
               tags$ul(tags$li(textOutput("headlineBestTreatment"))),

               # imp value exists
               conditionalPanel(condition = "output.implementationValueExists == 'TRUE'  ",
                                tags$ul(tags$li(textOutput("headlineImpOutcomesExist")))
               ),
               # imp value does not exist
               conditionalPanel(condition = "output.implementationValueExists == 'FALSE' ",
                                tags$ul(tags$li(textOutput("headlineImpOutcomesNotExist")))
               ),
               
               # if no value of information at all
               conditionalPanel(condition = "output.PositiveValueOfInformation == 'FALSE' ",
                                tags$ul(tags$li(textOutput("headlineNoVOI")))
               ),
               # if VOI > 0 but Bad research
               conditionalPanel(condition = "output.PositiveValueOfInformation == 'TRUE' && output.specificResearchWorthwhile == 'FALSE' ",
                                tags$ul(tags$li(textOutput("headlineSomeVOIButBadResearch")))
               ),
               # if VOI > 0 and good research (VOI must be > 0 if research is good)
               conditionalPanel(condition = "output.specificResearchWorthwhile == 'TRUE' ",
                                tags$ul(tags$li(textOutput("headlineSomeVOIAndGoodResearch")))
               ),
               
               # if its an RCT and Research is worthwhile 
               conditionalPanel(condition = "input.typeOfResearch == 'RCT' && output.specificResearchWorthwhile == 'TRUE' ",
                                tags$ul(tags$li(textOutput("headlineValueOfResearchRCT")))
               ),
               # if its a feasibility and Research is worthwhile 
               conditionalPanel(condition = "input.typeOfResearch == 'feasibility' && output.specificResearchWorthwhile == 'TRUE' ",
                                tags$ul(tags$li(textOutput("headlineValueOfResearchFeas1")),
                                        tags$li(textOutput("headlineValueOfResearchFeas2")))
               )
               
               
             ),
             
             
             # The value of changing practice
             ##########################
             br(),
             h4("What is the value of changing practice based on what we currently know about the treatments?"),
             
             # reuse headline text: the best treatment...
             tags$ul(tags$li(textOutput("changePracticeBestTreatment"))),
             
             # imp value exists
             conditionalPanel(condition = "output.implementationValueExists == 'TRUE'  ",
                              tags$ul(tags$li(textOutput("changePracticeImpOutcomesExist")))
             ),
             # imp value does not exist
             conditionalPanel(condition = "output.implementationValueExists == 'FALSE' ",
                              tags$ul(tags$li(textOutput("changePracticeImpOutcomesNotExist")))
             ),
             
             # unconditionally: show table of events per year
             tags$ul(tags$li("The table below displays the expected health benefits of each treatment:")),
             tableOutput("tableEventsPerYear"),
             
             # conditional on cost and qaly analysis show table of costs
             conditionalPanel(condition = "input.outcomeExpression != 'natural'",
                              tags$ul(tags$li("The table below displays the expected treatment costs associated with each treatment option:")),
                              tableOutput("tableTreatmentCosts")
                              ),
             
             
             # Remaining uncertainty
             ##########################
             br(),
             h4("What are the expected health consequences of the remaining uncertainty?"),
             
             # if VOI = 0 (no uncertainty)
             conditionalPanel(condition = "output.PositiveValueOfInformation == 'FALSE' ",
                              tags$ul(tags$li(textOutput("uncertaintyNone1")),
                                      tags$li(textOutput("uncertaintyNone2")))
             ),
             
             # if VOI > 0 (some uncertainty)
             conditionalPanel(condition = "output.PositiveValueOfInformation == 'TRUE' ",
                              tags$ul(tags$li(textOutput("uncertaintySome1")),
                                      tags$li(textOutput("uncertaintySome2")),
                                      tags$li(textOutput("uncertaintySome3"))),
                              # histogram
                              plotOutput("histVOIYear"),
                              # explain hist
                              tags$ul(tags$li(textOutput("discussHistVOIYear1")),
                                      tags$li(textOutput("discussHistVOIYear2")),
                                      tags$li("The average over this range of outcomes provides an estimate of the health benefits that could potentially be gained each year if the uncertainty in the decision could be resolved."))
             ),
             
             # Proposed research
             ##########################
             
             # if VOI > 0 (some uncertainty)
             conditionalPanel(condition = "output.PositiveValueOfInformation == 'TRUE' ",
                              br(),
                              h4("What is the value of the proposed research?"),
                              
                              # RCT value of proposed research
                              ###
                              conditionalPanel(condition = "input.typeOfResearch == 'RCT' ",
                                               tags$ul(tags$li(textOutput("proposedResearchMaxValueOfResearchRCT"))),
                                               tags$ul(tags$li(textOutput("fullTrialMaxValueOfResearchRCT"))),
                                               
                                               # if there is NO value in the RCT (after taking account of research time)
                                               conditionalPanel(condition = "output.PositiveValueOfResearchDesignRCT == 'FALSE'",
                                                                tags$ul(tags$li(textOutput("researchTakesTooLong")))
                                                                
                                               ),
                                               # if there is potential value in the RCT (after taking account of research time)
                                               conditionalPanel(condition = "output.PositiveValueOfResearchDesignRCT == 'TRUE'",
                                                                # if the outcome is QALY add in this
                                                                conditionalPanel(condition = "input.outcomeExpression != 'natural'",
                                                                                 tags$ul(tags$li(textOutput("expectedRCTNHSOpportunityCost")))
                                                                                 
                                                                                 ),
                                                                # final ICER calculation bit
                                                                tags$ul(tags$li(textOutput("ValueOfResearchRCT")))
                                                                
                                               )
                              ),
                              
                              # Feasibility value of proposed research
                              ###
                              conditionalPanel(condition = "input.typeOfResearch == 'feasibility' ",
                                               tags$ul(tags$li("The value of the feasibility trial depends on the health effect of the full trial and the likelihood that the full trial will be commissioned and report.")),
                                               strong("Health impact of full trial"),
                                               tags$ul(tags$li(textOutput("proposedResearchMaxValueOfResearchFeas"))),
                                               tags$ul(tags$li(textOutput("fullTrialMaxValueOfResearchFeas"))),
                                               
                                               # if there is there NO potential value in the full trial
                                               conditionalPanel(condition = "output.PositiveValueOfFullTrialFeas == 'FALSE' ",
                                                                tags$ul(tags$li("As the potential full trial is expected to have no health impact, there is no value in the proposed feasibility trial."))
                                               
                                               ),
                                               # if there is there IS potential value in the full trial
                                               conditionalPanel(condition = "output.PositiveValueOfFullTrialFeas == 'TRUE' ",
                                                                strong("Value of feasibility research"),
                                                                tags$ul(tags$li(textOutput("fullTrialNotCertainFeas"))),
                                                                tags$ul(tags$li(textOutput("expectedFullTrialFunderCost")))
                                                                
                                               ),
                                               # if natural outcome feasibitliy study
                                               conditionalPanel(condition = "input.outcomeExpression == 'natural'",
                                                                tags$ul(tags$li(textOutput("valueFeasNatural")))),
                                               
                                               # if QALY feasibiilty study
                                               conditionalPanel(condition = "input.outcomeExpression != 'natural'",
                                                                tags$ul(tags$li(textOutput("expectedFullTrialNHSCost"))),
                                                                tags$ul(tags$li(textOutput("expectedFullTrialNHSOpportunityCost"))),
                                                                # if there is still value in the research after subtracting op costs
                                                                conditionalPanel(condition = "output.PositiveValueOfFeas == 'TRUE' ",
                                                                                 tags$ul(tags$li(textOutput("ValueOfResearchFeasQALY"))),
                                                                                 tags$ul(tags$li("Whether this research represents good value to the health system depends on the value of the other potential uses of these resources."))
                                                                                 ),
                                                                # if there is not still value in the research
                                                                conditionalPanel(condition = "output.PositiveValueOfFeas == 'FALSE' ",
                                                                                 tags$ul(tags$li("Due to the opportunity costs imposed on the health system by this research, this research design is expected to have negative net health consequences."))
                                                                                 )
                                                                
                                                                )
                                               
                              ) # end feasibility value of proposed research
                              
                              
                              
                              
                              
                              
                              
                              
             ), # end of positive uncertainty conditional panel
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             
             # must render these commands for them to work as in java script contional panel
             # find somewhere out of the way to put these
             textOutput("implementationValueExists"),
             textOutput("PositiveValueOfInformation"),
             textOutput("specificResearchWorthwhile"),
             textOutput("PositiveValueOfResearchDesignRCT"),
             textOutput("PositiveValueOfFullTrialFeas"),
             textOutput("PositiveValueOfFeas")
             
             ##### old stuff
             
             #tableOutput("tableProbabilityMax"),
             
             
             
             
             
             
             
             
             # zombie code 
             #################### old results ###############################
             #
             
             
             # conditionalPanel(condition = "input.typeOfResearch != 'feasibility'",
             #                  p("This proposal is for a randomised controlled trial (RCT).
             #                    In this type of study, individuals are randomised to different treatments and the outcomes are compared accross the groups.")),
             # # if it is a feasibility study:
             # conditionalPanel(condition = "input.typeOfResearch == 'feasibility'",
             #                  p("This proposal is for a feasibility study. 
             #                    There are challenges and uncertainties associated with running a full trial. 
             #                    Due to these uncertainties it is unclear whether the larger follow-up trial is possible.
             #                    Research only impacts health in so far as it changes clinical practice. 
             #                    This feasibility trial is unlikely to generate enough evidence to justifying changing practice on its own. 
             #                    Therefore the impact of this feasibility trial on population health is through the potential future follow-up trial . 
             #                    If the follow-up trial is not possible the cost of funding it will not result in health benefit. 
             #                    Since the value of the feasibility trial depends on the follow-up trial, an evaluation of the future follow-up trial is required to value feasibility trial.")),
             # br(),
             
             
             # if cost + QALY study: (require this extra bit)
             # conditionalPanel(condition = "input.outcomeExpression == 'netHealth'",
             #                  h4("Summary of treatment costs"),
             #                  tableOutput("tableTreatmentCosts"),
             #                  textOutput("discussTableTreatmentCosts"),
             #                  br()),
             # 
             
             
             # # heading 4
             # h4("Value of implementing current evidence findings"),
             # # table showing expected outcomes with each treatment
             # #tableOutput("tableEventsPerYear"),
             # # text for general discussion about current information (common accross all models and endpoints?)
             # textOutput("resultsCurrenInformation"),
             # br(),
             # 
             # 
             # # heading 5
             # h4("Value of the proposed research"),
             # CONDITIONAL TEXT and HEADING: if feasibility study: (require this extra bit)
             # conditionalPanel(condition = "input.typeOfResearch == 'feasibility'",
             #                  p("Understanding the value of a feasibility trial requires two steps.
             #                    First the value of the follow-up trial must be estimated. 
             #                    Second, this value must be adjusted for the fact that the follow-up trial may not take place."),
             #                  strong("Value of potential follow-up trial"),
             #                  br()),
             # tableOutput("tableProbabilityMax"),
             # # text for discussion about value of research (common accross all models and endpoints?)
             # textOutput("resultsValueOfResearch"),
             # bug
             # problem in ui.R conditional planel
             # cannot make javaScript condition depend on results of VOI calcluation
             # must display this even if there is value in the research
             #plotOutput("histVOIYear"),
             # **problem**
             # the probabilies do not match between the histogram output and the analysis
             # the histogram is probably wrong and needs to be changed.
             # textOutput("discussHistVOIYear"),
             # br(),
             # textOutput("VOIresultsPart1"), # this section of the results is common to both RCT and Feas
             # br(),
             # # extra text for RCT results and interpretation
             # conditionalPanel(condition = "input.typeOfResearch == 'RCT'",
             #                  textOutput("RCTVOIresults")),
             # 
             # # extra text for Feasibility results and interpretation
             # conditionalPanel(condition = "input.typeOfResearch == 'feasibility'",
             #                  strong("Adjust value of potential follow up trial to value the feasibility study"),
             #                  textOutput("FeasVOIresults")),
             
             
             
             
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
                 column(4, 
                        downloadButton("report", label = "Download report"), # , class = "butt2"),
                        # change CSS sytle of download button!
                        # see https://gist.github.com/aagarw30/9c60b87e839db05b8dcc
                        #tags$head(tags$style(".butt2{background-color:black;} .butt2{color: white;} .butt2{font-style: italic;}")),
                        p("Save file to your computer with a .doc extension")
                 )
               ) # end 1st Write Report fluidRow 
             ) # end Write Report fluidPage 
    ) # end write report tabPanel
    
    
                              ) # end App tabSetPanel
             ) # end App fluid page
                              ) # end App shinyUi function










































#######################################################################################
# ZOMBIE CODE 
#######################################################################################




#textOutput("nameOf_t1"),

#textOutput("nameOf_t2"), 

#textOutput("nameOf_t3"), # conditional - is this a problem?

#textOutput("nameOf_t4"),# conditional - is this a problem?

#textOutput("nameOfOutcome"), # conditional - is this a problem?

# outputs

#textOutput("optimalTreatment" ) ,

#textOutput("expectedOutcomesPerYearoptimalTreatment"),

#textOutput("implementationValueExists"),        # new output

#textOutput("uncertaintyInCurrentEvidenceExists"),

#textOutput("probTreatment1isMax" ) ,

#textOutput("probTreatment2isMax" ) ,

#textOutput("probTreatment3isMax" ) ,

#textOutput("probTreatment4isMax" ) ,

#textOutput("popDuringResearch" ) ,

#textOutput("popAfterResearch" ) ,

#textOutput("popTotal" ) ,

#textOutput("popDuringFeasResearch" ) ,

#textOutput("popDuringDefinitiveResearch" ) ,

#textOutput("popAfterDefinitiveResearch" ) ,
#textOutput("valueOfResearchPerYear" ),

#textOutput("valueOfImplementationPerYear" ) ,

#textOutput("Cell_A" ) ,

#textOutput("Cell_C" ) ,

#textOutput("Cell_D" ) ,

#textOutput("maxvalueOfImplementation" ) ,

#textOutput("maxvalueOfResearch" ) ,

#textOutput("healthOpportunityCostsOfResearch" ) ,

#textOutput("expectedCostResearchFunder" ) ,                # unique Feas

#textOutput("valueOfResearchWithCurrentImplementation" ) ,

#textOutput("valueOfResearchWithPerfectImplementation" ) ,

#textOutput("valueOfCertainResearchWithPerfectImplementation" ) ,  # unique feas

#textOutput("ICER_ResearchWithCurrentImplementation" ) ,

#textOutput("ICER_ResearchWithPerfectImplementation" ) ,

#textOutput("valuePer15KResearchSpend"),

#textOutput("absoluteExpectedHealthOutcomesFromResearchProject"),

#textOutput("costResearchFunderFeas"),

#textOutput("costResearchFunderDefinitive"),

#textOutput("probabilityOfDefinitiveResearch")

#textOutput("test1"),

#textOutput("test2"),

#textOutput("test3")





