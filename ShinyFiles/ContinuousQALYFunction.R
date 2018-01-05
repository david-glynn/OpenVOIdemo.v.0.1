##############################
# Continuous QALY function v.0.1 
###############################

#ContinuousQALYFunction.R
########################
# supporting functions 
#########################
# functions required for this to work

# found in SupplementaryFunctions.R


# required to simulate probabilities of outcome
# must be loaded in this order (actually i am not sure if order matters!)
#simDeltaOfOutcomeMatrixContinuous
#simDeltaOfOutcomeNormContinuous
#simDeltaOfOutcomeHalfNormContinuous
# basic population function - calculates the population numbers required in the model
# verybasicPop


# test data
# numberOfTreatments =2
# MCsims = 100
# INBContinEvent = 0.05
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
# cost_t2 = 2000
# cost_t3 = NA
# cost_t4 = NA

ContinuousQALYFunction.v.0.1 <- function(numberOfTreatments, MCsims, INBContinEvent,
                                            mu_t2, variance_t2, dist_t2, direction_t2,
                                            mu_t3, variance_t3, dist_t3, direction_t3,
                                            mu_t4, variance_t4, dist_t4, direction_t4,
                                            nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                            cost_t2, cost_t3, cost_t4,
                                            typeOfOutcome, incidence,timeInformation,
                                            discountRate ,durationOfResearch,costResearchFunder,
                                            MCD_t2, MCD_t3, MCD_t4,
                                            utilisation_t1, utilisation_t2,
                                            utilisation_t3, utilisation_t4,
                                            costHealthSystem, k){
  
  # simulate plausible changes in continuous outcome for each treatment relative to t1 (aka deltas)
  #########################
  
  Delta_t <- simDeltaOfOutcomeMatrixContinuous(numberOfTreatments, MCsims,
                                               mu_t2, variance_t2, dist_t2, direction_t2,
                                               mu_t3, variance_t3, dist_t3, direction_t3,
                                               mu_t4, variance_t4, dist_t4, direction_t4)
  
  # create economic model from deltas
  #########################
  
  NB_t  <- Delta_t*INBContinEvent # multiply every element by net benefit (1st step in converting to NB)
  
  addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
  NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
  
  addCost_t <- c(0 ,-cost_t2/k, -cost_t3/k, -cost_t4/k) # subtract the health effects of costs
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



# test function
# resultsholder <- ContinuousQALYFunction.v.0.1(numberOfTreatments =2 , MCsims = 1000, INBContinEvent = 0.1,
#                                                  mu_t2=0, variance_t2=1, dist_t2="norm", direction_t2= NA,
#                                                  mu_t3=NA, variance_t3=NA, dist_t3=NA, direction_t3=NA,
#                                                  mu_t4=NA, variance_t4=NA, dist_t4=NA, direction_t4=NA,
#                                                  nameOf_t1="1",nameOf_t2="2", nameOf_t3=NA, nameOf_t4=NA,
#                                                  cost_t2= 100, cost_t3= NA, cost_t4=0,
#                                                  typeOfOutcome="benefit", incidence=1000,timeInformation=15,
#                                                  discountRate=3.5 ,durationOfResearch= 4,costResearchFunder=1000000,
#                                                  MCD_t2=0, MCD_t3=NA, MCD_t4=NA,
#                                                  utilisation_t1=100, utilisation_t2=0,
#                                                  utilisation_t3=NA, utilisation_t4=NA,
#                                                  costHealthSystem = 1000000, k = 13000)



