############
# master function
# wrapper function for all functions used
# takes all reacitive inputs from ui


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
master <- function(
                   # type of analysis 
                   typeOfEndpoint,
                   typeOfOutcome,
                   tCostsDependOnEvent,
                   numberOfTreatments,
                   typeOfResearch,
                   MCsims,
                   # report writing inputs
                   nameOf_t1,
                   nameOf_t2,
                   nameOf_t3,
                   nameOf_t4,
                   nameOfOutcome,
                   currencySymbol,
                   # basic health system info
                   incidence,
                   timeInformation,
                   discountRate,
                   utilisation_t1,
                   utilisation_t2,
                   utilisation_t3,
                   utilisation_t4,
                   MCD_t2,
                   MCD_t3,
                   MCD_t4,
                   # epidemiology: binary + generic
                   P_t1,
                   dist_t2,
                   mu_t2,
                   variance_t2,
                   direction_t2,
                   dist_t3,
                   mu_t3,
                   variance_t3,
                   direction_t3,
                   dist_t4,
                   mu_t4,
                   variance_t4,
                   direction_t4,
                   # epidemiology: survival
                   survivalDist,
                   scaleParameter_t1,
                   shapeParameter_t1,
                   # trial info: RCT
                   durationOfResearch,
                   costResearchFunder,
                   costHealthSystem,
                   # trial info: feasibility
                   probabilityOfDefinitiveResearch,
                   durationOfResearchFeas,
                   durationOfResearchDefinitive,
                   costResearchFunderFeas,
                   costResearchFunderDefinitive,
                   costHealthSystemFeas,
                   costHealthSystemDefinitive,
                   # cost and QALY inputs
                   k,
                   INBBinaryEvent,
                   INBContinEvent,
                   INBSurvivalEndpoint,
                   cost_t1,
                   costEvent_t1,
                   costNotEvent_t1,
                   cost_t2,
                   costEvent_t2,
                   costNotEvent_t2,
                   cost_t3,
                   costEvent_t3,
                   costNotEvent_t3,
                   cost_t4,
                   costEvent_t4,
                   costNotEvent_t4
                   
                   ){
  

  # RUN IF: binary natural outcome RCT
  if(typeOfEndpoint == "binary" & typeOfOutcome != "netHealth" & typeOfResearch == "RCT"){
    
    masterOutput <- BinaryOutcomeFunction(numberOfTreatments, MCsims, P_t1,
                         mu_t2, variance_t2, dist_t2, direction_t2,
                         mu_t3, variance_t3, dist_t3, direction_t3,
                         mu_t4, variance_t4, dist_t4, direction_t4,
                         nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                         typeOfOutcome, incidence,timeInformation,
                         discountRate ,durationOfResearch,costResearchFunder,
                         MCD_t2, MCD_t3, MCD_t4,
                         utilisation_t1, utilisation_t2,
                         utilisation_t3, utilisation_t4,
                         currencySymbol)
    return(masterOutput)
    }
  
  # RUN IF: binary natural outcome Feasibility
  if(typeOfEndpoint == "binary" & typeOfOutcome != "netHealth" & typeOfResearch == "feasibility"){
    masterOutput <- BinaryOutcomeFunctionFeas(numberOfTreatments, MCsims, P_t1,
                              mu_t2, variance_t2, dist_t2, direction_t2,
                              mu_t3, variance_t3, dist_t3, direction_t3,
                              mu_t4, variance_t4, dist_t4, direction_t4,
                              nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                              typeOfOutcome, incidence,timeInformation,
                              discountRate,
                              MCD_t2, MCD_t3, MCD_t4,
                              utilisation_t1, utilisation_t2,
                              utilisation_t3, utilisation_t4,
                              durationOfResearchDefinitive, durationOfResearchFeas,
                              costResearchFunderFeas,costResearchFunderDefinitive,
                              probabilityOfDefinitiveResearch, currencySymbol)
    return(masterOutput)
    }
  
  
}



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



