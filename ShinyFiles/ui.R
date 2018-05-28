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
    
    tabPanel("Welcome",
             
             br(),
             #h4("A video on using this app"),
             #br(),
             #p("<INSERT SHORT YOUTUBE VIDEO ON HOW TO USE THIS APP>"),
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
             br(),
             br(),
             br(),
             
             tags$em("This code has been produced under a GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007"),
             
             br(),
             br()
             
             ),  # close How to use this app tab
    
    
    ##################
    # How to estimate research value tab
    ##################
    
    tabPanel("How to estimate research value",
             
             br(),
             #h4("A video introducing value of information"),
             #HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/tbv9E9D2BRQ" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>'),
             #br(),
             
             h4("Principles of research prioritisation"),
             
             p("The following section outlines the principles of the assessments required when considering the need for additional evidence and the priority of proposed research. 
               These assessments help inform the two questions which must be answered when considering whether to prioritize and commission research:"),
             tags$ul(
               tags$li("Are the expected health benefits of additional evidence sufficient to regard the research proposal as potentially worthwhile?"), 
               tags$li("Should the proposal be prioritized over other research topics that could have been commissioned with the same resources?")
             ),
             
             br(),
             h4("Are the expected health benefits of additional evidence sufficient to regard the research proposal as potentially worthwhile?"),
             
             
             strong("How can health outcomes be improved?"),
             p("Health outcomes can be improved by conducting research or implementing the findings of existing research. In order to understand the value of additional evidence it is necessary to distinguish between these two very different ways to improve health outcomes. It is also necessary to take account of the costs associated with research projects and the fact that minimum changes in outcomes may need to be observed before clinical practice will change. "),
             
             strong("How does conducting research improve health outcomes?"),
             p("Additional evidence is valuable because it can improve patient outcomes by resolving existing uncertainty about the effectiveness of the interventions available. This helps inform treatment decisions for subsequent patients. For example, based on the balance of existing evidence a clinician might judge that a particular intervention is the most effective option, but there will be a chance that the other alternative interventions are in fact more effective. Therefore, when the existing evidence is uncertain there is a chance that one of the alternative interventions would have improved health outcomes to a greater extent. This means that there are adverse health consequences associated with uncertainty. A judgement about the level of uncertainty can come from a systematic review and meta-analysis, expert elicitation, extrapolation, meta-epidemiological study, or a combination of these sources. The level of uncertainty in the decision arises from the range of plausible values that the outcome can take.  This is represented by the confidence interval (CI) or standard error around the estimate. A wide CI implies a large amount of uncertainty.
               "),
             p("As an example, consider the evidence on the use of corticosteroids following traumatic brain injury (TBI) before the large definitive trial of CRASH. Before CRASH, a meta-analysis of 19 randomised controlled trials indicated that the effects of corticosteroids on death and disability were unclear. Taking death as an endpoint, we can start to understand the consequences of the uncertainty on mortality. The odds ratio for death was 0.93 in favour of the use of corticosteroids but with 95% CI from 0.71 to 1.18. This uncertainty means that every decision about the use of corticosteroids following TBI may not have been the most effective choice. In this case, there was a 74% chance that corticosteroids were effective and improved survival. However, there was a 26% chance that corticosteroids resulted in excess deaths per annum. This uncertainty can be translated into the consequences for patient outcomes, i.e. number of expected deaths per annum due to uncertainty, by combining the uncertain relative effect with an estimate of the baseline risk (derived from the control arms of the trials or from an alternative source) and multiplying by the incidence of TBI per year.
               "),
             p("These expected health consequences can be interpreted as an estimate of the health benefits that could be gained each year if the uncertainty surrounding treatment choice could be resolved, i.e., it provides an expected upper bound on the health benefits of further research which would confirm whether corticosteroids following TBI increase deaths or reduce them. In effect, this is the value of reducing the 95% confidence interval to a single point. These potential expected benefits increase with the size of the patient population whose treatment choice can be informed by additional evidence and the time over which evidence about the effectiveness of these interventions is expected to be useful. 
               "),
             
             strong("How does implementing the findings of existing evidence improve health outcomes?"),
             p("In addition to funding research, it is also possible to improve health outcomes by ensuring that the treatment option that is expected to be best based on the findings of existing evidence is implemented into clinical practice.  In fact, the improvements in health outcomes from implementing the findings of the current evidence base (implementation value) may be greater than the potential improvements in health outcomes through conducting further research."),
             
             strong("What change in the primary endpoint is required to change practice?"),
             p("The health benefits of conducting further research will only improve patient outcomes if the findings of the research change clinical practice. Again, it is important to recognise that there are many ways to influence implementation other than by conducting more research. However, concerns about implementation might influence research priority and the design of research.   For example, if it is very unlikely that the findings of research will affect clinical practice and other mechanisms are unlikely to be effective at changing practice, then another area of research might be prioritised even though the expected benefits are smaller. Furthermore, if the research must demonstrate highly statistically significant results to be implemented this will influence design, cost and time taken for research to report.  In some cases, larger clinical differences in effectiveness may be required before research would have an impact on practice. This will tend to reduce the potential benefits of further research as larger differences are less likely to be observed than smaller ones. The change in the primary endpoint required is called the minimum clinical difference (MCD)"),
             
             strong("Research costs imposed on the health system"),
             p("Carrying out research consumes resources in the general health system e.g. doctors, nurses, and pharmacists whose time commitments are moved away from general patient care and reallocated to research projects. The general health budget may also bear the costs of administrative staff and equipment that is needed to carry out the research. Another important type of cost that is often borne by the health budget is the acquisition costs of the health technologies under investigation (e.g. drug or device costs).  These costs must be taken into account to comprehensively understand the expected health benefits of research projects."),
             
             
             br(),
             h4("Should the research proposal be prioritized over other research topics that could have been commissioned with the same resources?"),
             
             p("Since research funding bodies have limited resources, not all research proposals can be funded and so the benefits of some research projects must be foregone in order to fund others. Quantitative estimates of the health benefits of research projects are required to compare the benefits of funded research to the foregone benefits of research not funded. These assessments can help to inform which research projects represent “best buys” for the research funder. They allow decision makers to compare value across proposals make explicit judgements about the trade-offs between different outcomes. If the potential health benefits of research are not in generic health outcomes (such as QALYs), implicit extrapolations are required to link changes in primary outcomes to compare health outcomes. "),
             
             
             br(),
             br(),
             br(),
             br(),
             br(),
             br()
             
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
                                    h4("Step 1: Primary outcome"),
                                    
                                    strong("Type of primary endpoint"),
                                    p("The primary outcome measure or endpoint captures the most important aspects of health outcome in the research."), 
                                    
                                    strong("Express results in natural outcomes (e.g. heart attacks avoided) or in QALYs?"),
                                    p("The benefits of research can be expressed in either natural outcomes or in Quality Adjusted Life Years (QALYs).
                                      Using QALYs requires more inputs but enables a comparison of the health benefits of further research and implementation efforts across diverse clinical areas."),
                                    
                                    strong("Is the outcome a benefit (e.g. cure) or a harm (e.g. heart attack)?"),
                                    p("For natural outcomes: the value of additional research is expressed in terms of ‘benefits gained’ or ‘harms avoided’ depending on whether this outcome is a benefit or harm."),
                                    
                                    strong("Name of outcome e.g. heart attack"),
                                    p("For natural outcomes: this will be used in reporting results."),
                                    
                                    strong("Do the treatment costs depend on the primary outcome?"),
                                    p("For binary outcomes: in some cases treatment costs will depend on whether the primary outcome occurs or not.
                                      For example, if a treatment is used to prevent disease progression then it will cease to be used (and its cost will no longer be incurred) if the individual progresses."),
                                    
                                    strong("Number of possible states if the primary outcome does / does not occur (4 maximum)"),
                                    p("For binary outcomes: the primary endpoint may be a scale or a composite outcome which is composed of a number of health states.
                                      If there are different levels of health and costs associated with these health states then these can be considered explicitly here.
                                      For example, if primary outcome is a composite of heart attack and stroke then there are two possible states if the primary outcome occurs and the costs and health consequences associated with these states must be explicitly considered."),
                                    
                                    strong("Conditional on the primary outcome occurring, what is the probability of being in this state?"),
                                    p("For binary outcomes: if the primary outcome is composed of distinct health states, different proportions of individuals may be expected to enter these health states.
                                      For example, if primary outcome is a composite of heart attack and stroke then conditional on the primary outcome occurring 40% and 60% of these individuals may be expected to experience heart attack and stroke respectively."),
                                    
                                    strong("Patient time horizon / time in this state (years)?"),
                                    p("For binary outcomes: if differential survival is considered then represents the expected survival time associated with each state. 
                                      Otherwise this represents the patient time horizon considered for the decision i.e. how far into the future individual patient outcomes are modelled."),
                                    
                                    strong("What is the health utility associated with this state?"),
                                    p("For binary outcomes: this is a number which represents the health related quality of life associated with a state."),
                                    
                                    strong("What are the disease related costs associated with this state?"),
                                    p("For binary outcomes: these are the costs associated with a particular disease state, they do not include the costs of the treatment under consideration."),
                                    
                                    strong("What is the health utility associated with the pre-transition health state?"),
                                    p("For survival outcomes: this is a number which represents the health related quality of life associated with the pre-transition state."),
                                    
                                    strong("What are the expected monthly disease related costs associated with the pre-transition health state?"),
                                    p("For survival outcomes: in the same manner as for health utility changes in the expected survival must be linked to changes in disease related costs.
                                      These are the costs associated with a particular disease state, they do not include the costs of the treatment under consideration."),
                                    
                                    strong("By how much is a one unit increase in the primary outcome expected to increase/decrease the health state utility?"),
                                    p("For continuous outcomes: the effect of changes from baseline on changes in health related quality of life (utility) will depend on the severity of the disease and range of the outcome measure. 
                                      “Mapping” studies which use statistical methods to estimate the effect of a unit change in a natural outcome on utility provide this link."),
                                    
                                    strong("By how much is a one unit increase in the primary outcome expected to increase/decrease monthly disease related costs?"),
                                    p("For continuous outcomes: changes in the primary outcome may also be expected to result in changes in disease related costs. 
                                      These are the costs associated with a particular disease state, they do not include the costs of the treatment under consideration."),
                                    
                                    strong("How long is the treatment effect expected to last? (months)"),
                                    p("For continuous outcomes: the scale of the health gains and disease related costs associated with changes in the primary outcome will depend on the expected treatment effect duration. 
                                      Estimates of treatment effect duration exist for few outcomes so in practice with will require expert opinion to inform this.")
                                    
                                    
                                    
                                    ), # end step 1 input information 
                                  br(),
                                  
                                  
                                  # step 2 input information
                                  wellPanel(
                                    h4("Step 2: Interventions"),
                                    
                                    strong("How many treatment options are under consideration? (Maximum of 4)"),
                                    p("There may be a number of relevant treatment options for a given indication. 
                                      This app currently allows for up to 4 options to be considered."),
                                    
                                    strong("Current level of utilisation (%)"),
                                    p("Some estimate of the current level of utilisation of the interventions in clinical practice is required to establish the value of changing practice if the results of new research suggest a change. 
                                      It can also be used to establish whether there is greater value from encouraging the implementation of what existing evidence suggests is the most effective intervention rather than conducting new research.
                                      The utilisation of all treatments must sum to 100%."),
                                    
                                    strong("Choose method of entering baseline probability of outcome"),
                                    p("An estimate of event rate with the baseline treatment is required.
                                       This is used to obtain an estimate of the absolute effect of the interventions on the primary outcome by applying the relative measure of effect to the baseline risk. 
                                       There are two options for entering this data. 1) Upper and lower 95% range: this may come from discussion with an expert and/or from a confidence interval reported in the literature.
                                       2) Number of events and number at risk: this may come from an observational study or control arm of an RCT"),
                                    
                                    strong("Scale for relative effect"),
                                    p("An estimate of the relative effectiveness of the intervention is required for the primary outcome, along with an estimate of its uncertainty. 
                                      This can be expressed with a 95% confidence interval in terms of an odds ratio (binary), relative risk (binary), risk difference (binary), hazard ratio (survival) or mean difference (continuous)."),
                                    
                                    strong("Minimum clinical difference (MCD)"),
                                    p("Specifying a MCD required to change clinical practice is one way to incorporate concerns about increased costs and/or potential adverse events.
                                      A larger clinical difference in effectiveness may need to be demonstrated before the findings of the proposed research are implemented with greater improvements in the primary outcome required to justify any additional costs.
                                      This must be specified as; percentage change in probability of outcome (binary), change in months before progression (survival) and units of change in continuous outcome (continuous)"),
                                    
                                    strong("Treatment costs over patient time horizon"),
                                    p("For binary outcomes: here Treatment costs are assumed to be the same for all individuals treated; regardless of health outcomes (see below).
                                      If treatment costs accrue over multiple years they should be discounted to present value."),
                                    
                                    strong("Treatment costs over patient time horizon if the primary outcome occurs / does not occur "),
                                    p("For binary outcomes: in some cases treatment costs will depend on the primary outcome, for example; intensive preventative treatment may be administered continuously until an event occurs (e.g. heart attack). Once the event has occurred the preventative treatment is halted and these treatment costs are no longer incurred."),
                                    
                                    # dont need to explain?
                                    strong("Treatment costs per month"),
                                    p("For continuous or survival outcomes: treatment costs incurred each month."),
                                    
                                    strong("Are individuals always treated until progression under this treatment?"),
                                    p("For survival outcomes individuals may be treated until progression or there may be a maximum duration of treatment.")
                                    
                                    
                                    ), # end step 2 input information 
                                  br(),
                               
                                  
                                  # step 3 input information
                                  wellPanel(
                                    h4("Step 3: Proposed research"),
                                    
                                    strong("Type of research"),
                                    p("The value of research can be calculated for either full research (e.g a randomised controlled trial (RCT)) or feasibility studies.
                                      The inputs required for the analysis will depend on the type of study chosen."),
                                    tags$ul(
                                      tags$li("Full research: In contrast to feasibility studies which facilitate full research this type of research aims to address clinical questions directly. For example RCTs which aim to reduce uncertainty about relative effects."), 
                                      tags$li("Feasibility study: If there are uncertainties about whether a full trial is possible, a short feasibility study can be carried out to assess the possibility of future full research.
                                              If the feasibility study is successful, researchers have the option to carry out the follow up trial.")
                                      ),
                                    
                                    strong("Probability of feasibility research leading to follow-up study"),
                                    p("For feasibility studies: the motivation is that there is uncertainty about whether a full trial is possible. If the feasibility study shows that the full trial is not possible, the research budget spent on the feasibility study will have no impact on health outcomes. For this reason, the likelihood of a feasibility study leading to the full trial is an important determinant of its value."),
                                    
                                    strong("Expected duration of research"),
                                    p("Some assessment of the duration of time for the proposed research to be conducted and for the  results of the research to report is required since the value of research declines the longer it takes to report. 
                                       This might be informed by an assessment of sample size, recruitment rates, or historical experience from conducting similar types of studies.
                                       For feasibility studies: estimates of duration for both the feasibility study and the full trial are required."),
                                    
                                    strong("Costs of the research to funder"),
                                    p("These costs are the costs of research which are directly borne by the research funder.
                                       For feasibility studies: estimates of research funder costs for both the feasibility study and the full trial are required."),
                                    
                                    strong("Costs of the research imposed on health system"),
                                    p("These costs are the costs of research which fall on the general health system.
                                       Carrying out research consumes valuable resources from the general health care budget e.g. additional treatment costs and health professionals whose time commitments are moved away from general patient care and reallocated to research projects. 
                                       For feasibility studies: estimates of research health system costs for both the feasibility study and the full trial are required.
                                      "),
                                    
                                    strong("Length of time for which the new evidence would be valuable"),
                                    p("The information generated by new research will not be valuable indefinitely because other changes occur over time. For example, over time new and more effective interventions become available which will eventually make those currently available obsolete. 
                                       This means that new information about effectiveness is only relevant for a specific amount of time. A judgement about the length of time that the evidence from the proposed research might be valuable is required to estimate the expected benefits over an appropriate time horizon.  
                                      "),
                                    
                                    strong("Discount rate"),
                                    p("Discounting should be used to reflect the fact that resources committed today could be invested at a real rate of return to free up more resources in the future. 
                                      "),
                                    
                                    strong("Incidence per annum"),
                                    p("An estimate of the number of individuals facing the uncertain choice between alternative interventions is required in order to establish the size of the benefits to the target population. 
                                      "),
                                  
                                    strong("Opportunity cost of health system expenditure"),
                                    p("Increasing treatment costs will be associated with health opportunity costs. 
                                      These are the improvement in health that would have been possible if any additional resources required had, instead, been made available for other health care activities.")
                                    
                                    
                                    ),
                                  
                                  
                                  br(),
                                  br(),
                                  br()
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
                                                                  numericInput("probability_s11", "Conditional on the primary outcome occurring, what is the probability of being in this state?",
                                                                               value = 1, min = 0, max = 1, step = 0.05)),
                                                              numericInput("lifeDuration_s11", "Patient time horizon / time in this state (years)?",
                                                                           value = 16.73, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s11", "What is the health utility associated with this state?",
                                                                           value = 0.7, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s11", "What are the disease related costs associated with this state?",
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
                                                              numericInput("probability_s12", "Conditional on the primary outcome occurring, what is the probability of being in this state?",
                                                                           value = 0.24, min = 0, max = 1, step = 0.05),
                                                              numericInput("lifeDuration_s12", "Patient time horizon / time in this state (years)?",
                                                                           value = 16.73, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s12", "What is the health utility associated with this state?",
                                                                           value = 0.81, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s12", "What are the disease related costs associated with this state?",
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
                                                              numericInput("probability_s13", "Conditional on the primary outcome occurring, what is the probability of being in this state?",
                                                                           value = 0.2, min = 0, max = 1, step = 0.05),
                                                              numericInput("lifeDuration_s13", "Patient time horizon / time in this state (years)?",
                                                                           value = 19.23, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s13", "What is the health utility associated with this state?",
                                                                           value = 0.96, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s13", "What are the disease related costs associated with this state?",
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
                                                              numericInput("probability_s14", "Conditional on the primary outcome occurring, what is the probability of being in this state?",
                                                                           value = 0.14, min = 0, max = 1, step = 0.05),
                                                              numericInput("lifeDuration_s14", "Patient time horizon / time in this state (years)?",
                                                                           value = 19.23, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s14", "What is the health utility associated with this state?",
                                                                           value = 1, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s14", "What are the disease related costs associated with this state?",
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
                                                                    numericInput("probability_s01", "Conditional on the primary outcome occurring, what is the probability of being in this state?",
                                                                                 value = 1, min = 0, max = 1, step = 0.05)),
                                                              numericInput("lifeDuration_s01", "Patient time horizon / time in this state (years)?",
                                                                           value = 0, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s01", "What is the health utility associated with this state?",
                                                                           value = 0, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s01", "What are the disease related costs associated with this state?",
                                                                           value = 0, min = 0, step = 100)
                                                            ))
                                           
                                           
                                    ),
                                    
                                    #binary: s02
                                    column(3,
                                           
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth' && input.numberS0States >= 2",
                                                            wellPanel(
                                                              
                                                              strong("State 2.2"),
                                                              numericInput("probability_s02", "Conditional on the primary outcome occurring, what is the probability of being in this state?",
                                                                           value = 0.07, min = 0, max = 1, step = 0.05),
                                                              numericInput("lifeDuration_s02", "Patient time horizon / time in this state (years)?",
                                                                           value = 7.11, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s02", "What is the health utility associated with this state?",
                                                                           value = 0.11, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s02", "What are the disease related costs associated with this state?",
                                                                           value = 45450, min = 0, step = 100)
                                                            ))
                                           
                                    ),
                                    # binary: s03
                                    column(3,
                                           
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth' && input.numberS0States >= 3",
                                                            wellPanel(
                                                              
                                                              strong("State 2.3"),
                                                              numericInput("probability_s03", "Conditional on the primary outcome occurring, what is the probability of being in this state?",
                                                                           value = 0.41, min = 0, max = 1, step = 0.05),
                                                              numericInput("lifeDuration_s03", "Patient time horizon / time in this state (years)?",
                                                                           value = 12.52, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s03", "What is the health utility associated with this state?",
                                                                           value = 0.41, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s03", "What are the disease related costs associated with this state?",
                                                                           value = 154324, min = 0, step = 100)
                                                            ))
                                           
                                    ),
                                    
                                    # binary: s04
                                    column(3,
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth' && input.numberS0States >= 4",
                                                            wellPanel(
                                                              
                                                              strong("State 2.4"),
                                                              numericInput("probability_s04", "Conditional on the primary outcome occurring, what is the probability of being in this state?",
                                                                           value = 0.23, min = 0, max = 1, step = 0.05),
                                                              numericInput("lifeDuration_s04", "Patient time horizon / time in this state (years)?",
                                                                           value = 12.52, min = 0, max = 100, step = 0.5),
                                                              numericInput("utility_s04", "What is the health utility associated with this state?",
                                                                           value = 0.58, min = -2, max = 1, step = 0.05),
                                                              numericInput("cost_s04", "What are the disease related costs associated with this state?",
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
                                                                                                                      step = 0.01, min = 0.01, max = 7, value = c(0.23, 5.24))),
                                                                                         # for RR (norm)
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t2 == 'RR'",
                                                                                                          sliderInput("RR_t2", "Select a plausible 95% range for the risk ratio",
                                                                                                                      step = 0.01, min = 0.01, max = 7, value = c(0.9, 1.1))),
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
                                                                                                     step = 0.01, min = 0.01, max = 7, value = c(1.05, 4))
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
                                                       
                                                       
                                                                        numericInput("MCD_t2", "Minimum clinical difference (MCD)",
                                                                                     value = 0, min = 0, max = NA, step = 0.05)
                                                       
                                                       
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
                                                                                                                      step = 0.01, min = 0.01, max = 7, value = c(0.19, 4.39))),
                                                                                         # for RR (norm)
                                                                                         conditionalPanel(condition = "input.binaryRelativeScale_t3 == 'RR'",
                                                                                                          sliderInput("RR_t3", "Select a plausible 95% range for the risk ratio",
                                                                                                                      step = 0.01, min = 0.01, max = 7, value = c(0.9, 1.1))),
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
                                                                                                     step = 0.01, min = 0.01, max = 7, value = c(1.1, 4.39))
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
                                                       
                                                       
                                                       numericInput("MCD_t3", "Minimum clinical difference (MCD)",
                                                                    value = 0, min = 0, max = NA, step = 0.05)
                                                       
                                                       
                                                       
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
                                                                                                                                       step = 0.01, min = 0.01, max = 7, value = c(0.71, 1.18))),
                                                                                                          # for RR (norm)
                                                                                                          conditionalPanel(condition = "input.binaryRelativeScale_t4 == 'RR'",
                                                                                                                           sliderInput("RR_t4", "Select a plausible 95% range for the risk ratio",
                                                                                                                                       step = 0.01, min = 0.01, max = 7, value = c(0.9, 1.1))),
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
                                                                                                                      step = 0.01, min = 0.01, max = 7, value = c(1.05, 5.24))
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
                                                                        
                                                                        
                                                                        numericInput("MCD_t4", "Minimum clinical difference (MCD)",
                                                                                     value = 0, min = 0, max = NA, step = 0.05)
                                                                        
                                                                        
                                                                        
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
                                               
                                               h4("Proposed research"),
                                               
                                               selectInput(inputId = "typeOfResearch", label = "Type of research", 
                                                           choices = c("Full research (e.g. RCT)" = "RCT", 
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
                                                           min = 0, max = 30, step = 0.25, value = 15)
                                               
                                             ) # end of wellPanel 
                                      ), # end of proposed research study column
                                      
                                      column(3,
                                             ##########
                                             # General inputs (2st panel): Other inputs
                                             ##########
                                             
                                             wellPanel( # START "other inputs" well panel
                                               
                                               h4("Other inputs"),
                                               
                                               # discount function is not defined for discount rate of zero
                                               numericInput("discountRate", "Discount rate (%)",
                                                            value = 3.5, min = 0.0001, max = 100, step = 0.1),
                                               
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
                                                                
                                                                h4("Opportunity cost of health system expenditure"),
                                                                
                                                                numericInput("k", " ",
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
             tags$ul(tags$li("The table below displays the expected outcomes for each treatment:")),
             tableOutput("tableEventsPerYear"),
             
             # **hide this table as it does not work! - impossible condition.
             # conditional on cost and qaly analysis show table of costs
             conditionalPanel(condition = "input.outcomeExpression != 'natural' && input.outcomeExpression == 'natural'",
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
             
             tags$style(type='text/css', '#implementationValueExists {font-size:1% }'), 
             tags$style(type='text/css', '#PositiveValueOfInformation {font-size:1% }'),
             tags$style(type='text/css', '#specificResearchWorthwhile {font-size:1% }'),
             tags$style(type='text/css', '#PositiveValueOfResearchDesignRCT {font-size:1% }'),
             tags$style(type='text/css', '#PositiveValueOfFullTrialFeas {font-size:1% }'),
             tags$style(type='text/css', '#PositiveValueOfFeas {font-size:1% }'),
             
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
             
             
             
             
             ) # end results tabPanel
    
    ##################
    # Write report page
    ##################
    # use textAreaInput to provide boxes so that analysts can justify their variable choices
    # 
    
    
    #tabPanel("Write and Download Report",
    #         fluidPage(
    #           fluidRow(
    #             column(4, "col 1"),
    #             column(4, 
    #                    downloadButton("report", label = "Download report"), # , class = "butt2"),
    #                    # change CSS sytle of download button!
    #                    # see https://gist.github.com/aagarw30/9c60b87e839db05b8dcc
    #                    #tags$head(tags$style(".butt2{background-color:black;} .butt2{color: white;} .butt2{font-style: italic;}")),
    #                    p("Save file to your computer with a .doc extension")
    #             )
    #           ) # end 1st Write Report fluidRow 
    #         ) # end Write Report fluidPage 
    #) # end write report tabPanel
    
    
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





