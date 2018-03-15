##############################
# Binary Outcome function v.0.1 
###############################
# calculates P_t (matrix) 
# allow for 4 treatments
# allow MCD
# only EVPI
# does not (and cannot) take account of NHS costs of research - need a different model for this



# Note*** test for this bug!
# good chance there is a bug in $Probability_t_is_max when the primary outcome is a harm


# old inputs - in order
# numberOfTreatments, 
# MCsims, 
# P_t1,               # this is a scalar; replaced by P_t1 as a vector
# mu_t2, variance_t2, dist_t2, direction_t2, # removed and replaced by simOutcomeMatrix inputs
# mu_t3, variance_t3, dist_t3, direction_t3, # removed
# mu_t4, variance_t4, dist_t4, direction_t4, # removed
# nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
# typeOfOutcome, 
# incidence,
# timeInformation,
# discountRate ,
# durationOfResearch,
# costResearchFunder,
# MCD_t2, MCD_t3, MCD_t4,
# utilisation_t1, utilisation_t2,
# utilisation_t3, utilisation_t4,
# currencySymbol


# new inputs
# needs to take: simOutcomeMatrix and all its inputs and feasibility study inputs
typeOfResearch
# numberOfTreatments, 
# MCsims, 
typeOfEndpoint
baselineInput
baselineRange
nEvents, 
nAtRisk,
binaryRelativeScale_t2,
binaryRelativeScale_t3,
binaryRelativeScale_t4,
P_t1, # vectpr
binaryDist_t2,
binaryDist_t3,
binaryDist_t4,
# norm dist
OR_t2,
OR_t3,
OR_t4,
RR_t2,
RR_t3,
RR_t4,
RD_t2,
RD_t3,
RD_t4,
# half norm dist
ORHalfNorm_t2,
ORHalfNorm_t3,
ORHalfNorm_t4,
RRHalfNorm_t2,
RRHalfNorm_t3,
RRHalfNorm_t4,
RDHalfNorm_t2,
RDHalfNorm_t3,
RDHalfNorm_t4,
### continuous
continuousInput_t2,
continuousInput_t3,
continuousInput_t4,
# contin meanAndSE
continMean_t2,
continMean_t3,
continMean_t4,
continSE_t2,
continSE_t3,
continSE_t4,
continDist_t2,
continDist_t3,
continDist_t4,
# contin normal mean difference 
MD_t2,
MD_t3,
MD_t4,
# contin half normal mean difference
MDHalfNorm_t2,
MDHalfNorm_t3,
MDHalfNorm_t4,
### survival
survivalType,
survivalDist_t2,
survivalDist_t3,
survivalDist_t4,
lambda_t1,
gamma_t1,
# survival norm dist 
HR_t2,
HR_t3,
HR_t4,
HRHalfNorm_t2,
HRHalfNorm_t3,
HRHalfNorm_t4
# nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
# typeOfOutcome, 
# incidence,
# timeInformation,
# discountRate ,
# durationOfResearch,
# costResearchFunder,
# MCD_t2, MCD_t3, MCD_t4,
# utilisation_t1, utilisation_t2,
# utilisation_t3, utilisation_t4,
# currencySymbol
# feasibility inputs
durationOfResearchDefinitive, durationOfResearchFeas,
costResearchFunderFeas,costResearchFunderDefinitive,
probabilityOfDefinitiveResearch,








# test data
typeOfResearch = "RCT" 
numberOfTreatments = 3 
MCsims = 1000  
typeOfEndpoint = "continuous"  
baselineInput = "range" 
baselineRange = c(0.2 , 0.6) 
nEvents = 10  
nAtRisk = 20 
binaryRelativeScale_t2 = "OR" 
binaryRelativeScale_t3 = "RR" 
binaryRelativeScale_t4 = "OR" 
P_t1 = rbeta(100, 10, 40) 
binaryDist_t2 = "alwaysPositive" 
binaryDist_t3 ="alwaysPositive"  
binaryDist_t4 = "alwaysPositive" 
# norm dist
OR_t2= c(0.71, 1.18) 
OR_t3 = c(0.71, 1.18) 
OR_t4= c(0.71, 1.18) 
RR_t2 =c(0.9, 1.1) 
RR_t3 =c(0.9, 1.1) 
RR_t4 =c(0.9, 1.1) 
RD_t2 =c(-5, 5) 
RD_t3=c(-5, 5) 
RD_t4=c(-5, 5) 
# half norm dist
ORHalfNorm_t2=2 
ORHalfNorm_t3=2 
ORHalfNorm_t4=2 
RRHalfNorm_t2=2 
RRHalfNorm_t3=2 
RRHalfNorm_t4=2 
RDHalfNorm_t2=2 
RDHalfNorm_t3=2 
RDHalfNorm_t4=2 
### continuous
continuousInput_t2="meanAndSE"  # "range", # "meanAndSE",
continuousInput_t3="range"  # "meanAndSE",
continuousInput_t4="range"  # "meanAndSE" 
# contin meanAndSE
continMean_t2=1 
continMean_t3=1 
continMean_t4=1 
continSE_t2=.5 
continSE_t3=.5 
continSE_t4=.5 
continDist_t2="alwaysPositive"  #  "alwaysNegative"  "norm" 
continDist_t3="alwaysPositive"  #  "alwaysNegative"  "norm" 
continDist_t4="alwaysPositive"  #  "alwaysNegative"  "norm" 
# contin normal mean difference
MD_t2=c(-2, 2) 
MD_t3=c(-2, 2) 
MD_t4=c(-2, 2) 
# contin half normal mean difference
MDHalfNorm_t2=2 
MDHalfNorm_t3=2 
MDHalfNorm_t4=2 
### survival
survivalType = "weibull"  # "exponential"  # "weibull" 
survivalDist_t2="alwaysPositive"  #  "alwaysNegative"  "norm"  
survivalDist_t3="alwaysPositive"  #  "alwaysNegative"  "norm"  
survivalDist_t4="alwaysPositive"  #  "alwaysNegative"  "norm"  
lambda_t1=10 
gamma_t1=1 
# survival norm dist
HR_t2=c(0.71, 1.18) 
HR_t3=c(0.71, 1.18) 
HR_t4=c(0.71, 1.18) 
HRHalfNorm_t2=1.4 
HRHalfNorm_t3=1.4 
HRHalfNorm_t4=1.4 
nameOf_t1="1" 
nameOf_t2="2" 
nameOf_t3="3" 
nameOf_t4="4" 
typeOfOutcome= "benefit"  
incidence=1000 
timeInformation=15 
discountRate=3.5  
durationOfResearch= 4 
costResearchFunder=1000000 
MCD_t2=0  
MCD_t3=0  
MCD_t4=0 
utilisation_t1=100  
utilisation_t2=0 
utilisation_t3=0  
utilisation_t4=0 
currencySymbol="£" 
# feasibility inputs
durationOfResearchDefinitive = 6 
durationOfResearchFeas = 2 
costResearchFunderFeas = 100000 
costResearchFunderDefinitive= 2000000 
probabilityOfDefinitiveResearch = 0.5













# both feasibility and RCT

BinaryOutcomeFunctionMaster <- function(typeOfResearch,
                                     numberOfTreatments, MCsims, typeOfEndpoint,
                                     baselineInput,
                                     baselineRange,nEvents, nAtRisk,
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
                                     HRHalfNorm_t2,HRHalfNorm_t3,HRHalfNorm_t4,
                                     nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                     typeOfOutcome, incidence,timeInformation,
                                     discountRate ,durationOfResearch,costResearchFunder,
                                     MCD_t2, MCD_t3, MCD_t4,
                                     utilisation_t1, utilisation_t2,
                                     utilisation_t3, utilisation_t4,
                                     currencySymbol,
                                     # feasibility inputs
                                     durationOfResearchDefinitive, durationOfResearchFeas,
                                     costResearchFunderFeas,costResearchFunderDefinitive,
                                     probabilityOfDefinitiveResearch
){
  
  # simulate outcome matrix - probabilities of event
  #########################
  set.seed(5)
  # simulate probabilities of event with UNCERTAIN baseline treatment
  if(baselineInput == "range"){
    P_t1 <- BaselineProbCI(MCsims, baselineRange[1], baselineRange[2])$P_t1   
    
  }
  
  if(baselineInput == "eventsNonEvents"){
    P_t1 <- probEvents(MCsims, nEvents, nAtRisk)$P_t1
  }
  
  # simulate probabilities of the event for other treatments
  P_t <- simOutcomeMatrix(numberOfTreatments,
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
  
  # create Binary economic model from probability of event
  #########################
  
  INB_Event <- ifelse(typeOfOutcome== "benefit", 1, -1)
  NB_t  <- P_t*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
  addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
  NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
  # each column now represents simulations of the NB of each treatment
  
  
  
  # Calculate outputs from NB matrix
  ######################################
  
  if(typeOfResearch == "RCT"){
    VOIoutputs <- NBtoEVPIResults(NB_t,
                                  nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                  typeOfOutcome, incidence,timeInformation,
                                  discountRate ,durationOfResearch,costResearchFunder,
                                  MCD_t2, MCD_t3, MCD_t4,
                                  utilisation_t1, utilisation_t2,
                                  utilisation_t3, utilisation_t4,
                                  costHealthSystem = NA, k = NA, currencySymbol)
  }
  
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
                                      costHealthSystemFeas = NA,costHealthSystemDefinitive =NA, k = NA,
                                      currencySymbol)
  }
  
  
  # return the list of results
  ###############################
  
  return(VOIoutputs)
  
}



# test function

BinaryOutcomeFunctionMaster(typeOfResearch = "feasibility" ,
                            numberOfTreatments = 3, 
                            MCsims = 1000, 
                            typeOfEndpoint = "continuous", 
                            baselineInput = "range",
                            baselineRange = c(0.2, 0.6),
                            nEvents = 10, 
                            nAtRisk = 20,
                            binaryRelativeScale_t2 = "OR",
                            binaryRelativeScale_t3 = "RR",
                            binaryRelativeScale_t4 = "OR",
                            P_t1 = rbeta(100, 10, 40),
                            binaryDist_t2 = "alwaysPositive",
                            binaryDist_t3 ="alwaysPositive" ,
                            binaryDist_t4 = "alwaysPositive",
                            # norm dist
                            OR_t2= c(0.71, 1.18),
                            OR_t3 = c(0.71, 1.18),
                            OR_t4= c(0.71, 1.18),
                            RR_t2 =c(0.9, 1.1),
                            RR_t3 =c(0.9, 1.1),
                            RR_t4 =c(0.9, 1.1),
                            RD_t2 =c(-5, 5),
                            RD_t3=c(-5, 5),
                            RD_t4=c(-5, 5),
                            # half norm dist
                            ORHalfNorm_t2=2,
                            ORHalfNorm_t3=2,
                            ORHalfNorm_t4=2,
                            RRHalfNorm_t2=2,
                            RRHalfNorm_t3=2,
                            RRHalfNorm_t4=2,
                            RDHalfNorm_t2=2,
                            RDHalfNorm_t3=2,
                            RDHalfNorm_t4=2,
                            ### continuous
                            continuousInput_t2="meanAndSE", # "range", # "meanAndSE",
                            continuousInput_t3="range", # "meanAndSE",
                            continuousInput_t4="range", # "meanAndSE",
                            # contin meanAndSE
                            continMean_t2=1,
                            continMean_t3=1,
                            continMean_t4=1,
                            continSE_t2=.5,
                            continSE_t3=.5,
                            continSE_t4=.5,
                            continDist_t2="alwaysPositive", #  "alwaysNegative"  "norm",
                            continDist_t3="alwaysPositive", #  "alwaysNegative"  "norm",
                            continDist_t4="alwaysPositive", #  "alwaysNegative"  "norm",
                            # contin normal mean difference
                            MD_t2=c(-2, 2),
                            MD_t3=c(-2, 2),
                            MD_t4=c(-2, 2),
                            # contin half normal mean difference
                            MDHalfNorm_t2=2,
                            MDHalfNorm_t3=2,
                            MDHalfNorm_t4=2,
                            ### survival
                            survivalType = "weibull", # "exponential", # "weibull",
                            survivalDist_t2="alwaysPositive", #  "alwaysNegative"  "norm",,
                            survivalDist_t3="alwaysPositive", #  "alwaysNegative"  "norm",,
                            survivalDist_t4="alwaysPositive", #  "alwaysNegative"  "norm",,
                            lambda_t1=10,
                            gamma_t1=1,
                            # survival norm dist
                            HR_t2=c(0.71, 1.18),
                            HR_t3=c(0.71, 1.18),
                            HR_t4=c(0.71, 1.18),
                            HRHalfNorm_t2=1.4,
                            HRHalfNorm_t3=1.4,
                            HRHalfNorm_t4=1.4,
                            nameOf_t1="1",
                            nameOf_t2="2",
                            nameOf_t3="3",
                            nameOf_t4="4",
                            typeOfOutcome= "benefit", 
                            incidence=1000,
                            timeInformation=15,
                            discountRate=3.5 ,
                            durationOfResearch= 4,
                            costResearchFunder=1000000,
                            MCD_t2=0, 
                            MCD_t3=0, 
                            MCD_t4=0,
                            utilisation_t1=100, 
                            utilisation_t2=0,
                            utilisation_t3=0, 
                            utilisation_t4=0,
                            currencySymbol="$",
                            # feasibility inputs
                            durationOfResearchDefinitive = 6,
                            durationOfResearchFeas = 2,
                            costResearchFunderFeas = 100000,
                            costResearchFunderDefinitive= 2000000,
                            probabilityOfDefinitiveResearch = 0.5
)














########################
# supporting functions 
#########################
# functions required for this to work

# found in SupplementaryFunctions.R
# required to simulate probabilities of outcome
# must be loaded in this order
# simProbOfOutcomeNormBinary
# simProbOfOutcomeHalfNormBinary
# simProbOfOutcomeMatrixBinary
# basic population function - calculates the population numbers required in the model
# verybasicPop


# functions not used here (code exists in NETSCC codes: GenericBinaryOutcome.R)
##
# allow for utilisation based on stat significance
# UtilMatrix.fn 
# allow implementation to depend on an observed MCD
# UtilMatrixMCD.fn 
# Implementation adjusted EVTPI 
# EVTPIadjImp.fn 


# mock data
#MCsims <- 10
#numberOfTreatments <- 2
#P_t1 <- 0.9
#mu_t2 <- 0
#variance_t2 <- 0.1
#dist_t2 <- "norm" 
#direction_t2 <- "alwaysPositive"

#mu_t3 <- 100
#variance_t3 <- 0.001
#dist_t3 <- "norm" 
#direction_t3 <- "alwaysNegative"
#mu_t4 <- 1
#variance_t4 <- 100
#dist_t4 <- "halfNorm" 
#direction_t4 <- "alwaysNegative" 

#nameOf_t1 <- "late PTP"
#nameOf_t2 <- "early PTP"
#nameOf_t3 <- "treatment 3"
#nameOf_t4 <- "treatment 4"

#typeOfOutcome <- "benefit" # "harm" "netHealth" # was Benefit==1 or 0 for benefit or harm
#incidence = 8000 # was Incidence
#timeInformation  = 15 # Time_info  = 15
#discountRate = 3.5  #D_rate = 0.035 ***NB need to divide by 100
#costResearchFunder = 882177 #Cost_research_funder =  882177
#durationOfResearch = 3  # Time_research = 3

#MCD_t2 = 0   # MCD_t <- c(0) # remember MCD is a relative input - if two treatmetns => just one MCD
#MCD_t3 = 0
#MCD_t4 = NA

#utilisation_t1 = 0.5 # check these sum to 1. 
#utilisation_t2 = 0.5
#utilisation_t3 = 0
#utilisation_t4 = NA

####################################


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

BinaryOutcomeFunction.v.0.1 <- function(numberOfTreatments, MCsims, P_t1,
                                        mu_t2, variance_t2, dist_t2, direction_t2,
                                        mu_t3, variance_t3, dist_t3, direction_t3,
                                        mu_t4, variance_t4, dist_t4, direction_t4,
                                        nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                        typeOfOutcome, incidence,timeInformation,
                                        discountRate ,durationOfResearch,costResearchFunder,
                                        MCD_t2, MCD_t3, MCD_t4,
                                        utilisation_t1, utilisation_t2,
                                        utilisation_t3, utilisation_t4,
                                        currencySymbol
                                        ){
  
  # simulate probabilities of event
  #########################
  set.seed(5)
  # simulate probabilities of event with baseline treatment
  P_t1 <- rep(P_t1, MCsims)
  
  # simulate probabilities of the event for other treatments
  P_t <- simProbOfOutcomeMatrixBinary(numberOfTreatments, P_t1,
                               mu_t2, variance_t2, dist_t2, direction_t2,
                               mu_t3, variance_t3, dist_t3, direction_t3,
                               mu_t4, variance_t4, dist_t4, direction_t4)
  
  # create Binary economic model from probability of event
  #########################
  
  INB_Event <- ifelse(typeOfOutcome== "benefit", 1, -1)
  
  NB_t  <- P_t*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
  
  addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
  NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
  
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
                                  costHealthSystem = NA, k = NA, currencySymbol)
  
  # return the list of results
  ###############################
  
  return(VOIoutputs)
  
}

BinaryOutcomeFunction <- BinaryOutcomeFunction.v.0.1

# test function
# resultsholder <- BinaryOutcomeFunction(numberOfTreatments =2 , MCsims = 100000, P_t1 =0.1,
#                                        mu_t2=0, variance_t2=1, dist_t2="norm", direction_t2= NA,
#                                        mu_t3=NA, variance_t3=NA, dist_t3=NA, direction_t3=NA,
#                                        mu_t4=NA, variance_t4=NA, dist_t4=NA, direction_t4=NA,
#                                        nameOf_t1="1",nameOf_t2="2", nameOf_t3=NA, nameOf_t4=NA,
#                                        typeOfOutcome="benefit", incidence=1000,timeInformation=15,
#                                        discountRate=3.5 ,durationOfResearch= 4,costResearchFunder=1000000,
#                                        MCD_t2=0, MCD_t3=NA, MCD_t4=NA,
#                                        utilisation_t1=100, utilisation_t2=0,
#                                        utilisation_t3=NA, utilisation_t4=NA,
#                                        currencySymbol="£")
# resultsholder







############## FEASIBILITY BINARY OUTCOME FUNCTION

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

BinaryOutcomeFunctionFeas.v.0.1 <- function(numberOfTreatments, MCsims, P_t1,
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
                                        probabilityOfDefinitiveResearch, currencySymbol
){
  
  # simulate probabilities of event
  #########################
  
  # simulate probabilities of event with baseline treatment
  P_t1 <- rep(P_t1, MCsims)
  
  # simulate probabilities of the event for other treatments
  P_t <- simProbOfOutcomeMatrixBinary(numberOfTreatments, P_t1,
                                      mu_t2, variance_t2, dist_t2, direction_t2,
                                      mu_t3, variance_t3, dist_t3, direction_t3,
                                      mu_t4, variance_t4, dist_t4, direction_t4)
  
  # create Binary economic model from probability of event
  #########################
  
  INB_Event <- ifelse(typeOfOutcome== "benefit", 1, -1)
  
  NB_t  <- P_t*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
  
  addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
  NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
  
  # each column now represents simulations of the NB of each treatment
  
  # Calculate outputs from NB matrix
  ######################################

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
                                      costHealthSystemFeas = NA,costHealthSystemDefinitive =NA, k = NA,
                                      currencySymbol)
  
  # return the list of results
  ###############################
  
  return(VOIoutputs)
  
}

BinaryOutcomeFunctionFeas <- BinaryOutcomeFunctionFeas.v.0.1



# test function 
# resultsholder <- BinaryOutcomeFunctionFeas(numberOfTreatments =2 , MCsims = 1000, P_t1 =0.5,
#                                        mu_t2=0, variance_t2=1, dist_t2="norm", direction_t2= NA,
#                                        mu_t3=NA, variance_t3=NA, dist_t3=NA, direction_t3=NA,
#                                        mu_t4=NA, variance_t4=NA, dist_t4=NA, direction_t4=NA,
#                                        nameOf_t1="1",nameOf_t2="2", nameOf_t3=NA, nameOf_t4=NA,
#                                        typeOfOutcome="benefit", incidence=1000,timeInformation=15,
#                                        discountRate=3.5,
#                                        MCD_t2=0, MCD_t3=NA, MCD_t4=NA,
#                                        utilisation_t1=100, utilisation_t2=0,
#                                        utilisation_t3=NA, utilisation_t4=NA,
#                                        durationOfResearchDefinitive = 6, durationOfResearchFeas = 2,
#                                        costResearchFunderFeas = 100000,costResearchFunderDefinitive= 2000000,
#                                        probabilityOfDefinitiveResearch = 0.5,
#                                        currencySymbol="£")

