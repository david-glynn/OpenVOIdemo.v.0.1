############
# master function



# test data
# typeOfEndpoint = "binary" # 'binary', "continuous''survival"
# baselineInput = "range"
# MCsims = 10
# baselineRange = c(0.3, 0.8)  
# nEvents = 10
# nAtRisk = 20
# numberOfTreatments = 3
# binaryRelativeScale_t2 = "OR"
# binaryRelativeScale_t3 = "RR"
# binaryRelativeScale_t4 = "OR"
# binaryDist_t2 = "alwaysPositive"
# binaryDist_t3 ="alwaysPositive" 
# binaryDist_t4 = "alwaysPositive"
# OR_t2= c(0.71, 1.18)
# OR_t3 = c(0.71, 1.18)
# OR_t4= c(0.71, 1.18)
# RR_t2 =c(0.9, 1.1)
# RR_t3 =c(0.9, 1.1)
# RR_t4 =c(0.9, 1.1)
# RD_t2 =c(-5, 5)
# RD_t3=c(-5, 5)
# RD_t4=c(-5, 5)
# ORHalfNorm_t2=2
# ORHalfNorm_t3=2
# ORHalfNorm_t4=2
# RRHalfNorm_t2=2
# RRHalfNorm_t3=2
# RRHalfNorm_t4=2
# RDHalfNorm_t2=2
# RDHalfNorm_t3=2
# RDHalfNorm_t4=2
# continuousInput_t2="meanAndSE" # "range", # "meanAndSE",
# continuousInput_t3="range" # "meanAndSE",
# continuousInput_t4="range" # "meanAndSE",
# continMean_t2=1
# continMean_t3=1
# continMean_t4=1
# continSE_t2=.5
# continSE_t3=.5
# continSE_t4=.5
# continDist_t2="alwaysPositive" #  "alwaysNegative"  "norm",
# continDist_t3="alwaysPositive" #  "alwaysNegative"  "norm",
# continDist_t4="alwaysPositive" #  "alwaysNegative"  "norm",
# MD_t2=c(-2, 2)
# MD_t3=c(-2, 2)
# MD_t4=c(-2, 2)
# MDHalfNorm_t2=2
# MDHalfNorm_t3=2
# MDHalfNorm_t4=2
# survivalType = "weibull" # "exponential", # "weibull",
# survivalDist_t2="alwaysPositive" #  "alwaysNegative"  "norm",,
# survivalDist_t3="alwaysPositive" #  "alwaysNegative"  "norm",,
# survivalDist_t4="alwaysPositive" #  "alwaysNegative"  "norm",,
# lambda_t1=10
# gamma_t1=1
# HR_t2=c(0.71, 1.18)
# HR_t3=c(0.71, 1.18)
# HR_t4=c(0.71, 1.18)
# HRHalfNorm_t2=1.4
# HRHalfNorm_t3=1.4
# HRHalfNorm_t4=1.4
# typeOfOutcome= "netHealth"
# tCostsDependOnEvent= "Yes"
# MCD_t2 = 0.1
# MCD_t3 = 0.2
# MCD_t4 = 0.3
# INBBinaryEvent = 2
# cost_t1 = 20
# cost_t2= 30
# cost_t3 = 40
# cost_t4 = 60
# k = 13000
# currencySymbol = "£"
# incidence = 100
# discountRate = 0.03 
# timeInformation = 15
# nameOf_t1 = "1"
# nameOf_t2 = "2"
# nameOf_t3 = "3"
# nameOf_t4 = "4"
# costEvent_t1 = 10
# costEvent_t2 = 20
# costEvent_t3 = 3
# costEvent_t4 = 40
# costNotEvent_t1 = 50
# costNotEvent_t2 = 60
# costNotEvent_t3 = 70
# costNotEvent_t4 = 80
# INBContinEvent = 0.5
# INBSurvivalEndpoint = 0.5
# typeOfResearch = "RCT"
# durationOfResearch = 1
# costResearchFunder = 70000
# utilisation_t1 = 100
# utilisation_t2 = 0
# utilisation_t3 = 0
# utilisation_t4 = 0
# costHealthSystem = 60000
# durationOfResearchDefinitive = 2
# durationOfResearchFeas = 2
# costResearchFunderFeas = 100
# costResearchFunderDefinitive =  200
# probabilityOfDefinitiveResearch = 0.9
# costHealthSystemFeas = 200000
# costHealthSystemDefinitive = 200000




# takes all inputs from ui.R (except the action button)
master <- function(
  typeOfEndpoint ,
  baselineInput ,
  MCsims ,
  baselineRange,
  nEvents ,
  nAtRisk ,
  numberOfTreatments ,
  binaryRelativeScale_t2 ,
  binaryRelativeScale_t3,
  binaryRelativeScale_t4,
  binaryDist_t2,
  binaryDist_t3,
  binaryDist_t4,
  OR_t2,
  OR_t3,
  OR_t4,
  RR_t2 ,
  RR_t3 ,
  RR_t4,
  RD_t2,
  RD_t3,
  RD_t4,
  ORHalfNorm_t2,
  ORHalfNorm_t3,
  ORHalfNorm_t4,
  RRHalfNorm_t2,
  RRHalfNorm_t3,
  RRHalfNorm_t4,
  RDHalfNorm_t2,
  RDHalfNorm_t3,
  RDHalfNorm_t4,
  continuousInput_t2,
  continuousInput_t3,
  continuousInput_t4,
  continMean_t2,
  continMean_t3,
  continMean_t4,
  continSE_t2,
  continSE_t3,
  continSE_t4,
  continDist_t2,
  continDist_t3,
  continDist_t4,
  MD_t2,
  MD_t3,
  MD_t4,
  MDHalfNorm_t2,
  MDHalfNorm_t3,
  MDHalfNorm_t4,
  survivalType ,
  survivalDist_t2,
  survivalDist_t3,
  survivalDist_t4,
  lambda_t1,
  gamma_t1,
  HR_t2,
  HR_t3,
  HR_t4,
  HRHalfNorm_t2,
  HRHalfNorm_t3,
  HRHalfNorm_t4,
  typeOfOutcome, 
  tCostsDependOnEvent,
  MCD_t2, 
  MCD_t3, 
  MCD_t4,
  INBBinaryEvent,
  cost_t1,
  cost_t2, 
  cost_t3, 
  cost_t4, 
  k,
  currencySymbol, 
  incidence, 
  discountRate, 
  timeInformation,
  nameOf_t1,
  nameOf_t2, 
  nameOf_t3, 
  nameOf_t4,
  costEvent_t1,
  costEvent_t2,
  costEvent_t3,
  costEvent_t4,
  costNotEvent_t1,
  costNotEvent_t2,
  costNotEvent_t3,
  costNotEvent_t4,
  INBContinEvent,
  INBSurvivalEndpoint,
  typeOfResearch,
  durationOfResearch,
  costResearchFunder,
  utilisation_t1, 
  utilisation_t2,
  utilisation_t3, 
  utilisation_t4,
  costHealthSystem,
  durationOfResearchDefinitive,
  durationOfResearchFeas,
  costResearchFunderFeas,
  costResearchFunderDefinitive,
  probabilityOfDefinitiveResearch,
  costHealthSystemFeas,
  costHealthSystemDefinitive

  
){
  
  ################################
  # 1) simulate outcome matrix outcome_t
  #########################
  set.seed(5)
  
  # baseline prob of outcome for binary models
  if(typeOfEndpoint == "binary"){
    
    # simulate probabilities of event with UNCERTAIN baseline treatment
    if(baselineInput == "range"){
      P_t1 <- BaselineProbCI(MCsims, baselineRange[1], baselineRange[2])$P_t1   
      
    }
    if(baselineInput == "eventsNonEvents"){
      P_t1 <- probEvents(MCsims, nEvents, nAtRisk)$P_t1
    }
    
  } else {
    # placeholder for this variable in simOutcomeMatrix
    P_t1 <- NA
  }
  
  
  # outcome matrix simulated for all models: binary, contin, survival
  outcome_t <- simOutcomeMatrix(numberOfTreatments,
                          MCsims, 
                          typeOfEndpoint,
                          ### binary
                          binaryRelativeScale_t2,binaryRelativeScale_t3,binaryRelativeScale_t4,
                          P_t1,
                          binaryDist_t2,binaryDist_t3,binaryDist_t4,
                          # norm dist
                          OR_t2,OR_t3,OR_t4,RR_t2,RR_t3,RR_t4,RD_t2,RD_t3,RD_t4,
                          # half norm dist
                          ORHalfNorm_t2,ORHalfNorm_t3,ORHalfNorm_t4,
                          RRHalfNorm_t2,RRHalfNorm_t3,RRHalfNorm_t4,
                          RDHalfNorm_t2, RDHalfNorm_t3,RDHalfNorm_t4,
                          ### continuous
                          continuousInput_t2,continuousInput_t3,continuousInput_t4,
                          # contin meanAndSE
                          continMean_t2,continMean_t3,continMean_t4,
                          continSE_t2,continSE_t3,continSE_t4,
                          continDist_t2,continDist_t3,continDist_t4,
                          # contin normal mean difference 
                          MD_t2,MD_t3,MD_t4,
                          # contin half normal mean difference
                          MDHalfNorm_t2,MDHalfNorm_t3,MDHalfNorm_t4,
                          ### survival
                          survivalType,
                          survivalDist_t2,survivalDist_t3,survivalDist_t4,
                          lambda_t1,gamma_t1,
                          # survival norm dist 
                          HR_t2,HR_t3,HR_t4,
                          HRHalfNorm_t2,HRHalfNorm_t3,HRHalfNorm_t4)

  
  
  ##############################
  # 2) convert matrix outcome_t tO NB_t - and also calculate tableTreatmentCostsDF
  #########################
  NBOutput <- outcomeToNB_t(outcome_t, MCsims,
                        typeOfEndpoint, typeOfOutcome, tCostsDependOnEvent,
                        MCD_t2, MCD_t3, MCD_t4,
                        INBBinaryEvent,
                        cost_t1,cost_t2, cost_t3, cost_t4, k,
                        currencySymbol, 
                        incidence, discountRate, timeInformation,
                        nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                        numberOfTreatments,
                        costEvent_t1,costEvent_t2,costEvent_t3,costEvent_t4,
                        costNotEvent_t1,costNotEvent_t2,costNotEvent_t3,costNotEvent_t4,
                        INBContinEvent,
                        INBSurvivalEndpoint)
  NB_t <- NBOutput$NB_t
  
  ##############################
  # 3) use NB_t to calculate EVPI results for RCT or Feasibility studies
  ########################
  
  # RCT research 
  ###############
  if(typeOfResearch == "RCT"){
    VOIoutputs <- NBtoEVPIResults(NB_t,
                                  nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                  typeOfOutcome, incidence,timeInformation,
                                  discountRate ,durationOfResearch,costResearchFunder,
                                  MCD_t2, MCD_t3, MCD_t4,
                                  utilisation_t1, utilisation_t2,
                                  utilisation_t3, utilisation_t4,
                                  costHealthSystem, k)
    VOIoutputs$tableTreatmentCostsDF <- NBOutput$tableTreatmentCostsDF # add the expected cost table to the input list
    
  }
  
  # Feasibility research
  #####################
  if(typeOfResearch == "feasibility"){
    VOIoutputs <- NBtoEVPIResultsFeas(NB_t,
                                      nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                      typeOfOutcome, incidence,timeInformation,
                                      discountRate ,durationOfResearchDefinitive,
                                      durationOfResearchFeas,costResearchFunderFeas,
                                      costResearchFunderDefinitive,
                                      MCD_t2, MCD_t3, MCD_t4,
                                      utilisation_t1, utilisation_t2,
                                      utilisation_t3, utilisation_t4,
                                      probabilityOfDefinitiveResearch,
                                      costHealthSystemFeas,costHealthSystemDefinitive, k,
                                      currencySymbol)
    VOIoutputs$tableTreatmentCostsDF <- NBOutput$tableTreatmentCostsDF # add the expected cost table to the input list
    
  }
  
  
  

  return(VOIoutputs)
} # end master function





# test function
# load required functions
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/SupplementaryFunctions.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/SupplementaryFunctionsFeas.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/ReconFunctions.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/EpiInputFunctions.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/PlottingFunctions.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/EpiCalcFunctions.R", local = TRUE)
# source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/NBCalcFunctions.R", local = TRUE)
# master <- master(
#     typeOfEndpoint = "binary", # 'binary', "continuous''survival"
#     baselineInput = "range",
#     MCsims = 100000,
#     baselineRange = c(0.475, 0.475) ,
#     nEvents = 10,
#     nAtRisk = 20,
#     numberOfTreatments = 3,
#     binaryRelativeScale_t2 = "OR",
#     binaryRelativeScale_t3 = "OR",
#     binaryRelativeScale_t4 = "OR",
#     binaryDist_t2 = "norm",
#     binaryDist_t3 ="norm" ,
#     binaryDist_t4 = "alwaysPositive",
#     OR_t2= c(0.375, 2.66),
#     OR_t3 = c(0.375, 2.66),
#     OR_t4= c(0.71, 1.18),
#     RR_t2 =c(0.9, 1.1),
#     RR_t3 =c(0.9, 1.1),
#     RR_t4 =c(0.9, 1.1),
#     RD_t2 =c(-5, 5),
#     RD_t3=c(-5, 5),
#     RD_t4=c(-5, 5),
#     ORHalfNorm_t2=2,
#     ORHalfNorm_t3=2,
#     ORHalfNorm_t4=2,
#     RRHalfNorm_t2=2,
#     RRHalfNorm_t3=2,
#     RRHalfNorm_t4=2,
#     RDHalfNorm_t2=2,
#     RDHalfNorm_t3=2,
#     RDHalfNorm_t4=2,
#     continuousInput_t2="meanAndSE", # "range", # "meanAndSE",
#     continuousInput_t3="range" ,# "meanAndSE",
#     continuousInput_t4="range", # "meanAndSE",
#     continMean_t2=1,
#     continMean_t3=1,
#     continMean_t4=1,
#     continSE_t2=.5,
#     continSE_t3=.5,
#     continSE_t4=.5,
#     continDist_t2="alwaysPositive", #  "alwaysNegative"  "norm",
#     continDist_t3="alwaysPositive", #  "alwaysNegative"  "norm",
#     continDist_t4="alwaysPositive", #  "alwaysNegative"  "norm",
#     MD_t2=c(-2, 2),
#     MD_t3=c(-2, 2),
#     MD_t4=c(-2, 2),
#     MDHalfNorm_t2=2,
#     MDHalfNorm_t3=2,
#     MDHalfNorm_t4=2,
#     survivalType = "weibull", # "exponential", # "weibull",
#     survivalDist_t2="alwaysPositive" ,#  "alwaysNegative"  "norm",,
#     survivalDist_t3="alwaysPositive", #  "alwaysNegative"  "norm",,
#     survivalDist_t4="alwaysPositive" ,#  "alwaysNegative"  "norm",,
#     lambda_t1=10,
#     gamma_t1=1,
#     HR_t2=c(0.71, 1.18),
#     HR_t3=c(0.71, 1.18),
#     HR_t4=c(0.71, 1.18),
#     HRHalfNorm_t2=1.4,
#     HRHalfNorm_t3=1.4,
#     HRHalfNorm_t4=1.4,
#     typeOfOutcome= "netHealth",
#     tCostsDependOnEvent= "No",
#     MCD_t2 = 0.0,
#     MCD_t3 = 0.0,
#     MCD_t4 = 0.3,
#     INBBinaryEvent = -0.7409,
#     cost_t1 = 687,
#     cost_t2= 1746,
#     cost_t3 = 2433,
#     cost_t4 = 60,
#     k = 13000,
#     currencySymbol = "£",
#     incidence = 1563,
#     discountRate = 0.035 ,
#     timeInformation = 15,
#     nameOf_t1 = "APs",
#     nameOf_t2 = "PI",
#     nameOf_t3 = "APs + PI",
#     nameOf_t4 = "4",
#     costEvent_t1 = 10,
#     costEvent_t2 = 20,
#     costEvent_t3 = 3,
#     costEvent_t4 = 40,
#     costNotEvent_t1 = 50,
#     costNotEvent_t2 = 60,
#     costNotEvent_t3 = 70,
#     costNotEvent_t4 = 80,
#     INBContinEvent = 0.5,
#     INBSurvivalEndpoint = 0.5,
#     typeOfResearch = "feasibility",
#     durationOfResearch = 4,
#     costResearchFunder = 2200000,
#     utilisation_t1 = 100,
#     utilisation_t2 = 0,
#     utilisation_t3 = 0,
#     utilisation_t4 = 0,
#     costHealthSystem = 60000,
#     durationOfResearchDefinitive = 5,
#     durationOfResearchFeas = 2,
#     costResearchFunderFeas = 601481,
#     costResearchFunderDefinitive =  2522710,
#     probabilityOfDefinitiveResearch = 0.5,
#     costHealthSystemFeas = 150000,
#     costHealthSystemDefinitive = 490000
# )
# master$listForhistVOIYear






































































######### ZOMBIE CODE
################################################################################################
# OLD MASTER
##############################################################################################

# wrapper function for all functions used
# takes all reacitive inputs from ui
# note: ordering of inputs into lower level models does not matter - all inputs from ui.R exist as named variables
# each function used here is defined in R scripts which are sourced in the server.R function of the shiny app

# source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/BinaryOutcomeFunction.R", local = TRUE)
# source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/BinaryQALYFunction.R", local = TRUE)
# source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/SupplementaryFunctions.R", local = TRUE)
# source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/ContinuousOutcomeFunction.R", local = TRUE)
# source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/ContinuousQALYFunction.R", local = TRUE)
# source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/SurvivalOutcomeFunction.R", local = TRUE)
# source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/SurvivalQALYFunction.R", local = TRUE)
# source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/SupplementaryFunctionsFeas.R", local = TRUE)
# # test inputs for binary outcome RCT
# numberOfTreatments =2
# MCsims = 100
# P_t1 =0.5
# mu_t2=0
# variance_t2=1
# dist_t2="norm"
# direction_t2= NA
# mu_t3=NA
# variance_t3=NA
# dist_t3=NA
# direction_t3=NA
# mu_t4=NA
# variance_t4=NA
# dist_t4=NA
# direction_t4=NA
# nameOf_t1="1"
# nameOf_t2="2"
# nameOf_t3=NA
# nameOf_t4=NA
# typeOfOutcome="benefit"
# incidence=1000
# timeInformation=15
# discountRate=3.5
# durationOfResearch= 4
# costResearchFunder=1000000
# MCD_t2=0
# MCD_t3=NA
# MCD_t4=NA
# utilisation_t1=100
# utilisation_t2=0
# utilisation_t3=NA
# utilisation_t4=NA
# currencySymbol = "£"
# # feasibilty test inputs
# numberOfTreatments =2
# MCsims = 100
# P_t1 =0.5
# mu_t2=0
# variance_t2=1
# dist_t2="norm"
# direction_t2= NA
# mu_t3=NA
# variance_t3=NA
# dist_t3=NA
# direction_t3=NA
# mu_t4=NA
# variance_t4=NA
# dist_t4=NA
# direction_t4=NA
# nameOf_t1="1"
# nameOf_t2="2"
# nameOf_t3=NA
# nameOf_t4=NA
# typeOfOutcome="benefit"
# incidence=1000
# timeInformation=15
# discountRate=3.5
# MCD_t2=0
# MCD_t3=NA
# MCD_t4=NA
# utilisation_t1=100
# utilisation_t2=0
# utilisation_t3=NA
# utilisation_t4=NA
# costResearchFunderFeas = 100000
# costResearchFunderDefinitive = 882177 #Cost_research_funder =  882177
# durationOfResearchDefinitive = 3 #durationOfResearch = 3  # Time_research = 3
# durationOfResearchFeas = 1
# probabilityOfDefinitiveResearch = 0.5
# currencySymbol = "£"
# # control type of model used
# typeOfEndpoint = "binary" 
# typeOfOutcome = "benefit" 
# typeOfResearch = "RCT"


# takes all inputs from ui.R (except the action button)
# master <- function(
#                    # type of analysis 
#                    typeOfEndpoint,
#                    typeOfOutcome,
#                    tCostsDependOnEvent,
#                    numberOfTreatments,
#                    typeOfResearch,
#                    reconsider,
#                    MCsims,
#                    # report writing inputs
#                    nameOf_t1,
#                    nameOf_t2,
#                    nameOf_t3,
#                    nameOf_t4,
#                    nameOfOutcome,
#                    currencySymbol,
#                    # basic health system info
#                    incidence,
#                    timeInformation,
#                    discountRate,
#                    utilisation_t1,
#                    utilisation_t2,
#                    utilisation_t3,
#                    utilisation_t4,
#                    MCD_t2,
#                    MCD_t3,
#                    MCD_t4,
#                    # epidemiology: binary + generic
#                    P_t1,
#                    dist_t2,
#                    mu_t2,
#                    variance_t2,
#                    direction_t2,
#                    dist_t3,
#                    mu_t3,
#                    variance_t3,
#                    direction_t3,
#                    dist_t4,
#                    mu_t4,
#                    variance_t4,
#                    direction_t4,
#                    # epidemiology: survival
#                    survivalDist,
#                    scaleParameter_t1,
#                    shapeParameter_t1,
#                    # trial info: RCT
#                    durationOfResearch,
#                    costResearchFunder,
#                    costHealthSystem,
#                    # trial info: feasibility
#                    probabilityOfDefinitiveResearch,
#                    durationOfResearchFeas,
#                    durationOfResearchDefinitive,
#                    costResearchFunderFeas,
#                    costResearchFunderDefinitive,
#                    costHealthSystemFeas,
#                    costHealthSystemDefinitive,
#                    # cost and QALY inputs
#                    k,
#                    INBBinaryEvent,
#                    INBContinEvent,
#                    INBSurvivalEndpoint,
#                    cost_t1,
#                    costEvent_t1,
#                    costNotEvent_t1,
#                    cost_t2,
#                    costEvent_t2,
#                    costNotEvent_t2,
#                    cost_t3,
#                    costEvent_t3,
#                    costNotEvent_t3,
#                    cost_t4,
#                    costEvent_t4,
#                    costNotEvent_t4
#                    
#                    ){
#   
# 
#   ########################
#   # Binary Endpoint models
#   ########################
#   
#   # RUN IF: binary natural outcome RCT
#   if(typeOfEndpoint == "binary" & typeOfOutcome != "netHealth" & typeOfResearch == "RCT" & reconsider == "No"){
#     masterOutput <- BinaryOutcomeFunction(numberOfTreatments, MCsims, P_t1,
#                          mu_t2, variance_t2, dist_t2, direction_t2,
#                          mu_t3, variance_t3, dist_t3, direction_t3,
#                          mu_t4, variance_t4, dist_t4, direction_t4,
#                          nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
#                          typeOfOutcome, incidence,timeInformation,
#                          discountRate ,durationOfResearch,costResearchFunder,
#                          MCD_t2, MCD_t3, MCD_t4,
#                          utilisation_t1, utilisation_t2,
#                          utilisation_t3, utilisation_t4,
#                          currencySymbol)
#     return(masterOutput)
#     }
#   
#   
#   # RUN IF: binary QALY RCT
#   if(typeOfEndpoint == "binary" & typeOfOutcome == "netHealth" & typeOfResearch == "RCT"& reconsider == "No"){
#     masterOutput <- BinaryQALYFunction(numberOfTreatments, MCsims, P_t1, INBBinaryEvent,
#                                          mu_t2, variance_t2, dist_t2, direction_t2,
#                                          mu_t3, variance_t3, dist_t3, direction_t3,
#                                          mu_t4, variance_t4, dist_t4, direction_t4,
#                                          nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
#                                          tCostsDependOnEvent, 
#                                          cost_t1, cost_t2, cost_t3, cost_t4,
#                                          costEvent_t1,costEvent_t2,costEvent_t3,costEvent_t4,
#                                          costNotEvent_t1,costNotEvent_t2,costNotEvent_t3,costNotEvent_t4,
#                                          typeOfOutcome, incidence,timeInformation,
#                                          discountRate ,durationOfResearch,costResearchFunder,
#                                          MCD_t2, MCD_t3, MCD_t4,
#                                          utilisation_t1, utilisation_t2,
#                                          utilisation_t3, utilisation_t4, 
#                                          costHealthSystem, k, currencySymbol)
#     return(masterOutput)
#   }
#   
#   
#   # RUN IF: binary natural outcome Feasibility
#   if(typeOfEndpoint == "binary" & typeOfOutcome != "netHealth" & typeOfResearch == "feasibility"& reconsider == "No"){
#     masterOutput <- BinaryOutcomeFunctionFeas(numberOfTreatments, MCsims, P_t1,
#                               mu_t2, variance_t2, dist_t2, direction_t2,
#                               mu_t3, variance_t3, dist_t3, direction_t3,
#                               mu_t4, variance_t4, dist_t4, direction_t4,
#                               nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
#                               typeOfOutcome, incidence,timeInformation,
#                               discountRate,
#                               MCD_t2, MCD_t3, MCD_t4,
#                               utilisation_t1, utilisation_t2,
#                               utilisation_t3, utilisation_t4,
#                               durationOfResearchDefinitive, durationOfResearchFeas,
#                               costResearchFunderFeas,costResearchFunderDefinitive,
#                               probabilityOfDefinitiveResearch, currencySymbol)
#     return(masterOutput)
#   }
#   
#   
#   # RUN IF: binary QALY Feasibility
#   if(typeOfEndpoint == "binary" & typeOfOutcome == "netHealth" & typeOfResearch == "feasibility"& reconsider == "No"){
#     masterOutput <- BinaryQALYFunctionFeas(numberOfTreatments, MCsims, P_t1, INBBinaryEvent,
#                                            mu_t2, variance_t2, dist_t2, direction_t2,
#                                            mu_t3, variance_t3, dist_t3, direction_t3,
#                                            mu_t4, variance_t4, dist_t4, direction_t4,
#                                            nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
#                                            tCostsDependOnEvent, 
#                                            cost_t1, cost_t2, cost_t3, cost_t4,
#                                            costEvent_t1,costEvent_t2,costEvent_t3,costEvent_t4,
#                                            costNotEvent_t1,costNotEvent_t2,costNotEvent_t3,costNotEvent_t4,
#                                            typeOfOutcome, incidence,timeInformation,
#                                            discountRate ,
#                                            MCD_t2, MCD_t3, MCD_t4,
#                                            utilisation_t1, utilisation_t2,
#                                            utilisation_t3, utilisation_t4, 
#                                            k, currencySymbol,
#                                            probabilityOfDefinitiveResearch,durationOfResearchDefinitive,
#                                            durationOfResearchFeas,costResearchFunderFeas,
#                                            costResearchFunderDefinitive,
#                                            costHealthSystemFeas,costHealthSystemDefinitive )
#     return(masterOutput)
#   }
#   
#   ########################
#   # Continuous Endpoint models
#   ########################
#   
#   # RUN IF: continuous natural outcome RCT
#   if(typeOfEndpoint == "continuous" & typeOfOutcome != "netHealth" & typeOfResearch == "RCT"& reconsider == "No"){
#     masterOutput <- ContinuousOutcomeFunction(numberOfTreatments, MCsims,
#                                               mu_t2, variance_t2, dist_t2, direction_t2,
#                                               mu_t3, variance_t3, dist_t3, direction_t3,
#                                               mu_t4, variance_t4, dist_t4, direction_t4,
#                                               nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
#                                               typeOfOutcome, incidence,timeInformation,
#                                               discountRate ,durationOfResearch,costResearchFunder,
#                                               MCD_t2, MCD_t3, MCD_t4,
#                                               utilisation_t1, utilisation_t2,
#                                               utilisation_t3, utilisation_t4,
#                                               currencySymbol)
#     return(masterOutput)
#   }
#   
#   # RUN IF: continuous QALY RCT
#   if(typeOfEndpoint == "continuous" & typeOfOutcome == "netHealth" & typeOfResearch == "RCT"& reconsider == "No"){
#     masterOutput <- ContinuousQALYFunction(numberOfTreatments, MCsims, INBContinEvent,
#                                            mu_t2, variance_t2, dist_t2, direction_t2,
#                                            mu_t3, variance_t3, dist_t3, direction_t3,
#                                            mu_t4, variance_t4, dist_t4, direction_t4,
#                                            nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
#                                            cost_t1, cost_t2, cost_t3, cost_t4,
#                                            typeOfOutcome, incidence,timeInformation,
#                                            discountRate ,durationOfResearch,costResearchFunder,
#                                            MCD_t2, MCD_t3, MCD_t4,
#                                            utilisation_t1, utilisation_t2,
#                                            utilisation_t3, utilisation_t4,
#                                            costHealthSystem, k, currencySymbol)
#     return(masterOutput)
#   }
#   
#   
#   # RUN IF: continuous natural outcome Feasibility
#   if(typeOfEndpoint == "continuous" & typeOfOutcome != "netHealth" & typeOfResearch == "feasibility"& reconsider == "No"){
#     masterOutput <- ContinuousOutcomeFunctionFeas(numberOfTreatments, MCsims,
#                                                   mu_t2, variance_t2, dist_t2, direction_t2,
#                                                   mu_t3, variance_t3, dist_t3, direction_t3,
#                                                   mu_t4, variance_t4, dist_t4, direction_t4,
#                                                   nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
#                                                   typeOfOutcome, incidence,timeInformation,
#                                                   discountRate ,
#                                                   MCD_t2, MCD_t3, MCD_t4,
#                                                   utilisation_t1, utilisation_t2,
#                                                   utilisation_t3, utilisation_t4,
#                                                   durationOfResearchDefinitive, durationOfResearchFeas,
#                                                   costResearchFunderFeas,costResearchFunderDefinitive,
#                                                   probabilityOfDefinitiveResearch,
#                                                   currencySymbol)
#       
#       
#     return(masterOutput)
#   }
#   
#   
#   # RUN IF: continuous QALY Feasibility
#   if(typeOfEndpoint == "continuous" & typeOfOutcome == "netHealth" & typeOfResearch == "feasibility"& reconsider == "No"){
#     masterOutput <- ContinuousQALYFunctionFeas(numberOfTreatments, MCsims, INBContinEvent,
#                                                mu_t2, variance_t2, dist_t2, direction_t2,
#                                                mu_t3, variance_t3, dist_t3, direction_t3,
#                                                mu_t4, variance_t4, dist_t4, direction_t4,
#                                                nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
#                                                cost_t1, cost_t2, cost_t3, cost_t4,
#                                                typeOfOutcome, incidence,timeInformation,
#                                                discountRate ,
#                                                MCD_t2, MCD_t3, MCD_t4,
#                                                utilisation_t1, utilisation_t2,
#                                                utilisation_t3, utilisation_t4,
#                                                costHealthSystem, k, currencySymbol,
#                                                probabilityOfDefinitiveResearch,durationOfResearchDefinitive,
#                                                durationOfResearchFeas,costResearchFunderFeas,
#                                                costResearchFunderDefinitive,
#                                                costHealthSystemFeas,costHealthSystemDefinitive)
#     return(masterOutput)
#   }
#   
#   
#   ########################
#   # Survival Endpoint models
#   ########################
#   
#   # RUN IF: survival natural outcome RCT
#   if(typeOfEndpoint == "survival" & typeOfOutcome != "netHealth" & typeOfResearch == "RCT"& reconsider == "No"){
#     masterOutput <- SurvivalOutcomeFunction(numberOfTreatments, MCsims, 
#                                             survivalDist,scaleParameter_t1,shapeParameter_t1,
#                                             mu_t2, variance_t2, dist_t2, direction_t2,
#                                             mu_t3, variance_t3, dist_t3, direction_t3,
#                                             mu_t4, variance_t4, dist_t4, direction_t4,
#                                             nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
#                                             typeOfOutcome, incidence,timeInformation,
#                                             discountRate ,durationOfResearch,costResearchFunder,
#                                             MCD_t2, MCD_t3, MCD_t4,
#                                             utilisation_t1, utilisation_t2,
#                                             utilisation_t3, utilisation_t4, currencySymbol)
#     return(masterOutput)
#   }
#   
#   # RUN IF: survival QALY RCT
#   if(typeOfEndpoint == "survival" & typeOfOutcome == "netHealth" & typeOfResearch == "RCT"& reconsider == "No"){
#     masterOutput <- SurvivalQALYFunction(numberOfTreatments, MCsims, 
#                                          survivalDist,scaleParameter_t1,shapeParameter_t1,
#                                          INBSurvivalEndpoint,
#                                          mu_t2, variance_t2, dist_t2, direction_t2,
#                                          mu_t3, variance_t3, dist_t3, direction_t3,
#                                          mu_t4, variance_t4, dist_t4, direction_t4,
#                                          nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
#                                          cost_t1, cost_t2, cost_t3, cost_t4,
#                                          typeOfOutcome, incidence,timeInformation,
#                                          discountRate ,durationOfResearch,costResearchFunder,
#                                          MCD_t2, MCD_t3, MCD_t4,
#                                          utilisation_t1, utilisation_t2,
#                                          utilisation_t3, utilisation_t4,
#                                          costHealthSystem, k, currencySymbol)
#       return(masterOutput)
#   }
#   
#   
#   # RUN IF: survival natural outcome Feasibility
#   if(typeOfEndpoint == "survival" & typeOfOutcome != "netHealth" & typeOfResearch == "feasibility"& reconsider == "No"){
#     masterOutput <- SurvivalOutcomeFunctionFeas(numberOfTreatments, MCsims, 
#                                                 survivalDist,scaleParameter_t1,shapeParameter_t1,
#                                                 mu_t2, variance_t2, dist_t2, direction_t2,
#                                                 mu_t3, variance_t3, dist_t3, direction_t3,
#                                                 mu_t4, variance_t4, dist_t4, direction_t4,
#                                                 nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
#                                                 typeOfOutcome, incidence,timeInformation,
#                                                 discountRate ,
#                                                 MCD_t2, MCD_t3, MCD_t4,
#                                                 utilisation_t1, utilisation_t2,
#                                                 utilisation_t3, utilisation_t4, 
#                                                 durationOfResearchDefinitive, durationOfResearchFeas,
#                                                 costResearchFunderFeas,costResearchFunderDefinitive,
#                                                 probabilityOfDefinitiveResearch, currencySymbol)
#       return(masterOutput)
#   }
#   
#   
#   # RUN IF: survival QALY Feasibility
#   if(typeOfEndpoint == "survival" & typeOfOutcome == "netHealth" & typeOfResearch == "feasibility"& reconsider == "No"){
#     masterOutput <- SurvivalQALYFunctionFeas(numberOfTreatments, MCsims, 
#                                              survivalDist,scaleParameter_t1,shapeParameter_t1,
#                                              INBSurvivalEndpoint,
#                                              mu_t2, variance_t2, dist_t2, direction_t2,
#                                              mu_t3, variance_t3, dist_t3, direction_t3,
#                                              mu_t4, variance_t4, dist_t4, direction_t4,
#                                              nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
#                                              cost_t1, cost_t2, cost_t3, cost_t4,
#                                              typeOfOutcome, incidence,timeInformation,
#                                              discountRate ,
#                                              MCD_t2, MCD_t3, MCD_t4,
#                                              utilisation_t1, utilisation_t2,
#                                              utilisation_t3, utilisation_t4,
#                                              k, currencySymbol,
#                                              probabilityOfDefinitiveResearch,durationOfResearchDefinitive,
#                                              durationOfResearchFeas,costResearchFunderFeas,
#                                              costResearchFunderDefinitive,
#                                              costHealthSystemFeas,costHealthSystemDefinitive)
#       return(masterOutput)
#   }
#   
#   
# } # end master function





# test master function 
# master(typeOfEndpoint = "binary",
#                    typeOfOutcome = "benefit",
#                    tCostsDependOnEvent,
#                    numberOfTreatments = 2,
#                    typeOfResearch = "RCT",
#                    nameOfOutcome,
#                    currencySymbol = "£",
#                    nameOf_t1 = "1",
#                    nameOf_t2 = "2",
#                    nameOf_t3= "3",
#                    nameOf_t4 ="4",
#                    incidence = 100,
#                    timeInformation = 15,
#                    discountRate = 3.5,
#                    utilisation_t1 = 100,
#                    utilisation_t2 = 0,
#                    utilisation_t3 = 0,
#                    utilisation_t4 = 0,
#                    cost_t1,
#                    costEvent_t1,
#                    costNotEvent_t1,
#                    dist_t2 = "norm",
#                    mu_t2 = 0,
#                    variance_t2 = 0.25,
#                    direction_t2,
#                    cost_t2,
#                    costEvent_t2,
#                    costNotEvent_t2,
#                    k,
#                    MCD_t2 = 0,
#                    dist_t3= "norm" ,
#                    mu_t3 = 0,
#                    variance_t3 = 0.2,
#                    direction_t3,
#                    cost_t3,
#                    costEvent_t3,
#                    costNotEvent_t3,
#                    MCD_t3 = 0,
#                    dist_t4= "norm",
#                    mu_t4 = 0,
#                    variance_t4 = 0.25,
#                    direction_t4,
#                    cost_t4,
#                    costEvent_t4,
#                    costNotEvent_t4,
#                    MCD_t4 = 0,
#                    P_t1 = 0.5,
#                    INBBinaryEvent,
#                    INBContinEvent,
#                    survivalDist,
#                    scaleParameter_t1,
#                    shapeParameter_t1,
#                    INBSurvivalEndpoint,
#                    #runRCT,
#                    durationOfResearch = 5,
#                    costResearchFunder = 2000000,
#                    costHealthSystem = 1000000,
#                    MCsims = 50000,
#                    durationOfResearchFeas = 2,
#                    durationOfResearchDefinitive = 5,
#                    probabilityOfDefinitiveResearch = 0.5,
#                    costResearchFunderFeas = 100000,
#                    costResearchFunderDefinitive = 100000,
#                    costHealthSystemFeas = 1000000,
#                    costHealthSystemDefinitive = 1000000)



