#######################
# potential server side bugs: 
# if the app is open on a number of windows on the browser (i.e. if it has not been "stopped" in a while)
# then Binary/Survival Feas NetHealth models crash. Some problem with trying to find popDurationResearch 
# 
#


# must load the required functions! SupplementaryFunctions.R

library(shiny)

############################
# load up required functions

library(scales) # required to format tables in renderTable
library(fdrtool) # required for halfnormal simulations
library(MASS) # for use in EpiInputFunctions.R to fit beta distributions to unknown probabilities

#W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles
# absolute paths for use in desktop development
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/BinaryOutcomeFunction.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/BinaryQALYFunction.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/SupplementaryFunctions.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/ContinuousOutcomeFunction.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/ContinuousQALYFunction.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/SurvivalOutcomeFunction.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/SurvivalQALYFunction.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/SupplementaryFunctionsFeas.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/master.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/ReconFunctions.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/EpiInputFunctions.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/PlottingFunction.R", local = TRUE)


# relative paths for publishing in shinyapps.io
#source("BinaryOutcomeFunction.R", local = TRUE)
#source("BinaryQALYFunction.R", local = TRUE)
#source("SupplementaryFunctions.R", local = TRUE)
#source("ContinuousOutcomeFunction.R", local = TRUE)
#source("ContinuousQALYFunction.R", local = TRUE)
#source("SurvivalOutcomeFunction.R", local = TRUE)
#source("SurvivalQALYFunction.R", local = TRUE)
#source("SupplementaryFunctionsFeas.R", local = TRUE)
#source("master.R", local = TRUE)
#source("ReconFunctions.R", local = TRUE)
#source("EpiInputFunctions.R", local = TRUE)
#source("PlottingFunction.R")



shinyServer(function(input, output) {
  
  
  
  
  
})



