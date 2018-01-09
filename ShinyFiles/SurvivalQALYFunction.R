##############################
# Survial QALY function v.0.1 
###############################
# statistical theory found in Survial SupplementaryFunctions.R

########################
# supporting functions 
#########################
# functions required for this to work

# found in SupplementaryFunctions.R



# test data
# numberOfTreatments =2
# MCsims = 100
# survivalDist <- "exponential" # other option = "weibull"
# scaleParameter_t1 = 20
# shapeParameter_t1 = 1
# INBSurvivalEndpoint = 0.5
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
# costHealthSystem = 1000000
# k = 13000
# cost_t1 = 200
# cost_t2 = 2000
# cost_t3 = NA
# cost_t4 = NA

SurvivalQALYFunction.v.0.1 <- function(numberOfTreatments, MCsims, 
                                          survivalDist,scaleParameter_t1,shapeParameter_t1,
                                          INBSurvivalEndpoint,
                                          mu_t2, variance_t2, dist_t2, direction_t2,
                                          mu_t3, variance_t3, dist_t3, direction_t3,
                                          mu_t4, variance_t4, dist_t4, direction_t4,
                                          nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                          cost_t1, cost_t2, cost_t3, cost_t4,
                                          typeOfOutcome, incidence,timeInformation,
                                          discountRate ,durationOfResearch,costResearchFunder,
                                          MCD_t2, MCD_t3, MCD_t4,
                                          utilisation_t1, utilisation_t2,
                                          utilisation_t3, utilisation_t4,
                                          costHealthSystem, k){
  
  
  # no uncertainty in baseline for now
  ExpectedSurvival_t1 <- if(survivalDist == "exponential"){
    rep(scaleParameter_t1, MCsims)
  } else { # if weibull
    rep(scaleParameter_t1^(1/shapeParameter_t1)*gamma(1/shapeParameter_t1 + 1), MCsims)
  }
  
  
  # Based on baseline outcome above simulate expected survival each treatment
  #########################
  
  ExpectedSurvival_t <- simDurationMatrixSurvival(numberOfTreatments, ExpectedSurvival_t1,survivalDist,
                                                  scaleParameter_t1, shapeParameter_t1, MCsims,
                                                  mu_t2, variance_t2, dist_t2, direction_t2,
                                                  mu_t3, variance_t3, dist_t3, direction_t3,
                                                  mu_t4, variance_t4, dist_t4, direction_t4)
  
  # create economic model from expected survial
  #########################

  NB_t  <- ExpectedSurvival_t*INBSurvivalEndpoint # multiply every element by INBSurvivalEndpoint (1st step in converting to NB)
  
  addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
  NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
  
  # subtract the costs from each column in the vector.
  
  addCost_t <- c(-cost_t1/k ,-cost_t2/k, -cost_t3/k, -cost_t4/k) 
  NB_t  <- NB_t  + rep(addCost_t, each = MCsims)
  
  # each column now represents simulations of the NB of each treatment
  
  # Calculate outputs from NB matrix
  ######################################
  
  VOIoutputs <- NBtoEVPIResults(NB_t,
                                nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                typeOfOutcome, incidence,timeInformation,
                                discountRate ,durationOfResearch,costResearchFunder,
                                MCD_t2, MCD_t3, MCD_t4,
                                utilisation_t1, utilisation_t2,
                                utilisation_t3, utilisation_t4,
                                costHealthSystem, k)
  
  # return the list of results
  ###############################
  
  return(VOIoutputs)
  
}

# assign this model to the generic name of the function
SurvivalQALYFunction <- SurvivalQALYFunction.v.0.1

# test function
# resultsholder <- SurvivalQALYFunction(numberOfTreatments =2 , MCsims = 1000,
#                                                  survivalDist = "exponential",
#                                                  scaleParameter_t1 = 10,shapeParameter_t1 = 1,
#                                                  INBSurvivalEndpoint = 2,
#                                                  mu_t2=0, variance_t2=1, dist_t2="norm", direction_t2= NA,
#                                                  mu_t3=NA, variance_t3=NA, dist_t3=NA, direction_t3=NA,
#                                                  mu_t4=NA, variance_t4=NA, dist_t4=NA, direction_t4=NA,
#                                                  nameOf_t1="1",nameOf_t2="2", nameOf_t3=NA, nameOf_t4=NA,
#                                                  cost_t1 = 1000, cost_t2 = 1000, cost_t3 = NA, cost_t4 = NA,
#                                                  typeOfOutcome="benefit", incidence=1000,timeInformation=15,
#                                                  discountRate=3.5 ,durationOfResearch= 4,costResearchFunder=1000000,
#                                                  MCD_t2=0, MCD_t3=NA, MCD_t4=NA,
#                                                  utilisation_t1=100, utilisation_t2=0,
#                                                  utilisation_t3=NA, utilisation_t4=NA,
#                                                  costHealthSystem = 1000000, k = 13000)

