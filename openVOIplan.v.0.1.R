####################
# Shiny app planning
####################


# libraries required
library(fdrtool)

#####################################################################################
#####################################################################################
############### Inputs
# in the ui.R section - intput tabs

# Common (core?) - Inputs
################

######### UNCONDITIONAL INPUTS (Common)

# choices:	List of values to select from. 
# If elements of the list are named, 
# then that name rather than the value is displayed to the user
selectInput(inputId = "typeOfOutcome", label = "Type of outcome", 
            choices = c("Benefit" = "benefit", 
                        "Harm" = "harm", 
                        "Net health effect (QALYs)" = "netHealth"),
            selected = "Net health effect (QALYs)") # benefit , harm, net health effect

# do i need this?? I could just wait until they press the relevant 
# update results action button!
# ***
selectInput(inputId = "typeOfResearch", label = "Type of research", 
            choices = c("RCT" = "RCT", 
                        "Feasibility study" = "feasibility", 
                        "Reconsideration of evidence" = "reconsider"),
            selected = "RCT") 

# how many treatments are being investigated
numericInput("numberOfTreatments", "How many treatments are being investigated?",
             value = 2, min = 2, max = 4)

# probabiilty of event with baseline treatment, right name??
numericInput("P_t1", "Probability of outcome with treatment 1",
             value = 0.5, min = 0, max = 1, step = 0.05)

# name of first two treatments 
textInput("nameOf_t1", "Name of treatment 1 (optional)", 
          value = "late PTP")
textInput("nameOf_t2", "Name of treatment 2 (optional)", 
          value = "early PTP")

# distribution of new treatments (will be either normal or half normal)
# note it is possible to name the variables for use in the app vs what the user sees
# could possibly get this label to update conditional on nameOf_t1
selectInput("dist_t2", label = "Distribution of treatment 2", 
            choices = c("Normal" = "norm", 
                        "Half Normal" = "halfNorm"),
            selected = "Normal")
 
# maybe think about initial value - set it to blank to force the user to change it
numericInput("incidence", "Incidence per annum",
             value = 8800, min = 0, max = NA, step = 20)

numericInput("timeInformation", "Time over which evidence would be valuable (years)",
             value = 15, min = 0, max = NA, step = 0.1)

# NOTE!! this is in percent***need to divide by 100
numericInput("discountRate", "Discount rate (%)",
             value = 3.5, min = 0, max = 100, step = 0.1)

# width works well - note: text above box is squeezed along with the box
numericInput("MCD_t2", "MCD for treatment 2",
             value = 0, min = NA, max = NA, step = 0.05,
             width = '50%')

actionButton("runRCT", label = "Run calculation for RCT")
actionButton("runFeas", label = "Run calculation for feasibility trial")
actionButton("runRec", label = "Run calculation for reconsideration of evidence")



####### CONDITIONAL INPUTS (Common)

# just a string for writing results and report
# set equal to QALYs if typeOfOutcome == "Net health effect (QALYs)" 
# note- it is possible to assign a different variable name to "Net health effect (QALYs)"
# check that it is a string!
textInput("nameOfOutcome", "Name of outcome")

# would be nice if these boxes for treatment 2 and 3 were conditional on the number of treatments selected
# would be nice to update the names of the treatments in the labels for the distribution inputs
# name of first two treatments 
textInput("nameOf_t3", "Name of treatment 3 (optional)", 
          value = "treatment 3")

textInput("nameOf_t4", "Name of treatment 4 (optional)", 
          value = "treatment 4")


# distribution of new treatments (will be either normal or half normal)
selectInput("dist_t3", label = "Distribution of treatment 3", 
            choices = c("Normal" = "norm", 
                        "Half Normal" = "halfNorm"),
            selected = "Normal")

selectInput("dist_t4", label = "Distribution of treatment 4", 
            choices = c("Normal" = "norm", 
                        "Half Normal" = "halfNorm"),
            selected = "Normal")


# if normal - NEED conditional boxes with conditional lables
mu_t2 # mean LOR for t1
var_t2# variance (to avoid questions about sd or se)
mu_t3
var_t3
mu_t4 
var_t4 
# if half normal

direction_t2  # takes value "alwaysPositive" or "alwaysNegative" has value of NA if nothing selected
var_t2 # will be converted to theta parameter has value of NA if nothing selected
direction_t3  
var_t3
direction_t4 
var_t4 

k # conditional on type of outcome == net health effect

# conditional on type of outcome == net health effect
cost_t2
cost_t3
cost_t4

# NOTE!! this is in percent***need to divide by 100


numericInput("utilisation_t1", "Utilisation of treatment 1 (%)",
             value = 100, min = 0, max = 100, step = 0.1)

numericInput("utilisation_t2", "Utilisation of treatment 2 (%)",
             value = 0, min = 0, max = 100, step = 0.1)

numericInput("utilisation_t3", "Utilisation of treatment 3 (%)",
             value = 0, min = 0, max = 100, step = 0.1)

numericInput("utilisation_t4", "Utilisation of treatment 4 (%)",
             value = 0, min = 0, max = 100, step = 0.1)



numericInput("MCD_t3", "MCD for treatment 3",
             value = 0, min = NA, max = NA, step = 0.05,
             width = '50%')

numericInput("MCD_t4", "MCD for treatment 4",
             value = 0, min = NA, max = NA, step = 0.05,
             width = '50%')




# Randomised controlled trial - Inputs
#####################################

######### UNCONDITIONAL INPUTS (RCT)


# allow for different currencies??
numericInput("costResearchFunder", "Cost of research to funder",
             value = 2854000, min = 0, max = NA, step = 100)

numericInput("durationOfResearch", "Expected duration of research (years)",
             value = 5, min = 0, max = NA, step = 0.1)



# need to check what is a sensible maximum! what will crash R!
# suggested 50K
numericInput("MCsims", "Number of simulations",
             value = 50000, min = 0, max = 10000000, step = 500)
# need to repeat this?


####### CONDITIONAL INPUTS (RCT)

numericInput("costHealthSystem", "Costs of research imposed on health system",
             value = NA, min = 0, max = NA, step = 100)
 # conditional on type of outcome == net health effect




# Feasibility study - Inputs
#####################################

######### UNCONDITIONAL INPUTS (Feasibility) 

numericInput("costResearchFunderPilot", "Cost of feasibility study to funder",
             value = NA, min = 0, max = NA, step = 100)

costResearchFunderDefinitive

durationOfResearchPilot
durationOfResearchDefinitive

# need to repeat this?
MCsims # suggested 50K - should be at initial inputs?

######### CONDITIONAL INPUTS (Feasibility) 

costHealthSystemPilot # conditional on type of outcome == net health effect
costHealthSystemDefinitive # conditional on type of outcome == net health effect



# Reconsider evidence - Inputs
#####################################
# can you do this without NB approach?

######### UNCONDITIONAL INPUTS (Reconsider)

varExercise_t1 # variance of estimate for t1 from expert elicitation 

# decision rule for research funder to fund trial
# do i need one for each treatment??
costPerOutcomeThreshold  

costResearchFunderProposal
costResearchFunderReconsideration # default = 0

durationOfResearchProposal
durationOfReconsideration

# suggest something sensible with estimate of time for result 
# and a timer bar
MCsimsInner 
MCsimsOuter # suggest something sensible


######### CONDITIONAL INPUTS (Reconsider)

# variance of estimate for t2 from expert elicitation 
varExercise_t3  
varExercise_t4 

costHealthSystemProposal # conditional on type of outcome == net health effect
costHealthSystemReconsideration # required? # conditional on type of outcome == net health effect



#############
# Calculate outputs  (translate inputs into outputs)
#############
################################################################################
################################################################################
### put in server.R

# translate inputs for use in functions
#############################
# 
# inputs required for GenericBinaryOutcome.v1 
              # P_t_U ,Benefit,MCD_t , Incidence, 
              # Time_info ,Utilisation_t,D_rate ,Time_research ,
              # Cost_research_funder,UtilMatrix

# inputs required for GenericBinaryQALY.v1
              # P_t_U ,INB_Event,k,C_t, MCD_t , Incidence, 
              # Time_info ,Utilisation_t,D_rate ,Time_research ,
              # Cost_research_funder,Cost_research_system, 
              # UtilMatrix = NA, Time_delay = 0 # these show defaults

# create: P_t_U 
# a matrix of prob sims with a column for each treatment and lenght = MCsims
# quite complex - depends on many inputs


# create: MCD_t - not required? taken as input into each of the VOI functions
# a vector of MCDs for each treatment with zero for t0
# MCD_t <- reactive({ c(0, input$MCD_t1, input$MCD_t2, input$MCD_t3) })
# input to VOI functions using MCD_t()
# should have lenght = the number of treatments (nt)
# trim to the correct size using numberOfTreatments




###################################################################################
# rendering outputs

################ Easy reactive outputs that are not relevant to the model
# 

output$nameOf_t1 <- renderText({input$nameOf_t1})
output$nameOf_t2 <- renderText({input$nameOf_t2})

# conditional - is this a problem?
# if the relevant inputs do not exist then the "output" (in this case the 
# treatment name just does not show up - the renderText function probably keeps
# a space in the app for it though.)

output$nameOf_t3 <- renderText({input$nameOf_t3})
output$nameOf_t4 <- renderText({input$nameOf_t4})

# conditional - is this a problem?
output$nameOfOutcome <- renderText({input$nameOfOutcome})



################ more difficult reactive outputs that are very relevant to the model
#
# see openVOIdemoNotes.Rmd

# create empty managed state variable
VOIResults <- reactiveValues()

# when the appropriate event is observed, then carry out the appropriate analysis

# user pushes runRCT action button
observeEvent(input$runRCT, {
  
  # if cost and QALY analysis then run this function
  if(input$typeOfOutcome == "netHealth"){
    resultsHolder <- reactive({
      # VOI function taking user inputs and returning results
      # for RCT cost and QALY analysis
    })
    # save results of RCT cost and QALY analysis
    VOIResults$VOIYear <-  resultsHolder()$VOIYear
    VOIResults$ICERResearch <-  resultsHolder()$ICERResearch
    
  }else{
    # if natural outcome analysis then fun this function
    resultsHolder <- reactive({
      # VOI function taking user inputs and returning results
      # for RCT natural outcome analysis
    })
    # save results of natural outcome analysis
    VOIResults$VOIYear <-  resultsHolder()$VOIYear
    VOIResults$ICERResearch <-  resultsHolder()$ICERResearch
  }
})

observeEvent(input$runFeas, {
  #  similar code for: feasibility study
})
observeEvent(input$runRec, {
  #  similar code for: reconsideration of evidence
})


######
# for each type of analysis need to have a function which will take inputs
# and return a list of the required results
# then must assign the reults of the analysis to VOIResults list

############# functions for each analysis



############# assign results of each analysis (resultsHolder) to VOIResults list
# resultsHolder is reactive and so requires () when calling it
# must do this for each type of analysis
# ***need to extend/ammend this***

VOIResults$histVOIYear <- resultsHolder()$histVOIYear # plot
VOIResults$optimalTreatment <- resultsHolder()$optimalTreatment
VOIResults$probOptimalTreatment <- resultsHolder()$probOptimalTreatment
VOIResults$VOImpYear <- resultsHolder()$VOImpYear
VOIResults$VOImpTotal <- resultsHolder()$VOImpTotal
VOIResults$VOIYear <- resultsHolder()$VOIYear
VOIResults$VOITotal <- resultsHolder()$VOITotal
VOIResults$VOIResearch <- resultsHolder()$VOIResearch
VOIResults$ICERResearch <- resultsHolder()$ICERResearch
VOIResults$OutcomePerNETSCCSpend <- resultsHolder()$OutcomePerNETSCCSpend






###########
# render each type of output from VOIResults to the output list
#############
# outputs for all types of analysis
# just needs to be done once and this covers all the different types of analysis
# note: it is possible that some types of analysis will not create every type of 
# output

# renderPlot
output$histVOIYear <- renderPlot({VOIResults$histVOIYear})

# renderText
output$optimalTreatment <- renderText({VOIResults$optimalTreatment})
output$probOptimalTreatment <- renderText({VOIResults$probOptimalTreatment})
output$VOImpYear <- renderText({VOIResults$VOImpYear})
output$VOImpTotal <- renderText({VOIResults$VOImpTotal})
output$VOIYear <- renderText({VOIResults$VOIYear})
output$VOITotal <- renderText({VOIResults$VOITotal})
output$VOIResearch <- renderText({VOIResults$VOIResearch})
output$ICERResearch <- renderText({VOIResults$ICERResearch})
output$OutcomePerNETSCCSpend <- renderText({VOIResults$OutcomePerNETSCCSpend})







#####################################################################################
#####################################################################################
##################################### USER INTERFACE stuff ####################################
### PUT IN ui.R
##  displays results in a section of the user interface
# use these elements in text elements - place into sentences and paragraphs

#######
# simple non reactive objects which do not depend on updating the model

# name of treatments
textOutput("nameOf_t1")
textOutput("nameOf_t2") 
textOutput("nameOf_t3") # conditional - is this a problem?
textOutput("nameOf_t4")# conditional - is this a problem?
# name of outcome
textOutput("nameOfOutcome") # conditional - is this a problem?


#######
# COMPLEX reactive objects which DO depend on updating the model

plotOutput("histVOIYear")

textOutput("optimalTreatment")
textOutput("probOptimalTreatment")
textOutput("VOImpYear")
textOutput("VOImpTotal")
textOutput("VOIYear")
textOutput("VOITotal")
textOutput("VOIResearch")
textOutput("ICERResearch")
textOutput("OutcomePerNETSCCSpend")



