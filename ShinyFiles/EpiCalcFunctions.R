###
# new epi functions
# calls on functions defined in epi input functions
# EpiInputFunctions.R
# 
# input: all epi inputs
# output: outcome_tn (a vector of appropriate length in appropriate units)

library(fdrtool) # required for halfnormal simulations

options(scipen = 999) # turn off scientific notation



# all new epi inputs 
##############################################
# 
# typeOfEndpoint == 'binary', "continuous''survival"
#
# # baseline inputs- binary
# baselineInput: "range", "eventsNonEvents"
# "nEvents" "nAtRisk" # single values 
# range:  c(0.248, 0.469)
# produces: P_t1
# 
# # baseline inputs- survival
# survivalDist: "exponential","weibull"
# scaleParameter_t1, shapeParameter_t1
# produces: ExpectedSurvival_t1
# 
# # baseline inputs- continuous - none!
#
# # relative effect inputs - binary
# binaryRelativeScale_t2: OR", "RR", "RD"
# binaryDist_t2: "alwaysPositive", "alwaysNegative", "norm"
# normal dist
# OR_t2:  c(0.71, 1.18))),
# RR_t2: c(0.9, 1.1))),
# RD_t2:  c(-5, 5)
# half norm dist
# ORHalfNorm_t2: single value (bounds depend on whether the half normal is "alwaysPositive" or "alwaysNegative")
# RRHalfNorm_t2: single value
# RDHalfNorm_t2: single value
#
# # relative effect inputs - continuous
# continuousInput_t2: "range" "meanAndSE
# if meanAndSE: continMean_t2", "continSE_t2"
# continDist_t2: alwaysPositive", alwaysNegative","norm"
# norm dist
# MD_t2: c(-2, 2)
# half norm dist
# MDHalfNorm_t2: single value (bounds depend on whether the half normal is "alwaysPositive" or "alwaysNegative")
#
# # relative effect inputs - survival
# survivalDist_t2: alwaysPositive","alwaysNegative", "norm"
# survivalType = "exponential" # "weibull"
# norm dist 
# HR_t2:  c(0.71, 1.18)
# HRHalfNorm_t2: single value (bounds depend on whether the half normal is "alwaysPositive" or "alwaysNegative")


# simProbOfOutcome test data
# MCsims = 10
# typeOfEndpoint = "binary"
# ## binary
# binaryRelativeScale_tn =  "OR" # "RR", "RD"
# P_t1 = rbeta(10, 10, 40)
# binaryDist_tn = "alwaysPositive" # , "alwaysNegative", "norm"
# # binary normal dist
# OR_tn = c(0.71, 1.18)
# RR_tn = c(0.9, 1.1)
# RD_tn = c(-5, 5)
# # binary half norm dist
# ORHalfNorm_tn = 2 # upper bound if binaryDist_t2 == alwaysPositive, lower bound if binaryDist_t2 == alwaysNegative,
# RRHalfNorm_tn = 2
# RDHalfNorm_tn = 0.7
# #
# ## continuous
# continuousInput_tn =  "range" # "meanAndSE"
# # contin meanAndSE
# continMean_tn = 1
# continSE_tn = 0.5
# continDist_tn =  "alwaysPositive" #  "alwaysNegative"  "norm"
# # contin normal mean difference 
# MD_tn =  c(-2, 2) # 95% intervals
# # contin half normal mean difference
# MDHalfNorm_tn = 20 # upper bound if continDist_tn == alwaysPositive, lower bound if continDist_tn == alwaysNegative,
# #
# ## survival
# survivalType = "exponential" # "weibull"
# survivalDist_tn =  "alwaysPositive" # "alwaysNegative" "norm"
# lambda_t1 = 10
# gamma_t1 = 1
# # survival norm dist 
# HR_tn =   c(0.71, 1.18)
# HRHalfNorm_tn = 1.4




# calculates outcome_tn for all models
simOutcome_tn <- function(
  MCsims, # required for continuous functions
  typeOfEndpoint,
  ### binary
  binaryRelativeScale_tn,
  P_t1,
  binaryDist_tn,
  # norm dist
  OR_tn,
  RR_tn,
  RD_tn,
  # half norm dist
  ORHalfNorm_tn,
  RRHalfNorm_tn,
  RDHalfNorm_tn,
  ### continuous
  continuousInput_tn,
  # contin meanAndSE
  continMean_tn,
  continSE_tn,
  continDist_tn,
  # contin normal mean difference 
  MD_tn,
  # contin half normal mean difference
  MDHalfNorm_tn,
  ### survival
  survivalType,
  survivalDist_tn,
  lambda_t1,
  gamma_t1,
  # survival norm dist 
  HR_tn,
  HRHalfNorm_tn
){
  
  # Binary for odds ratio
  ################
  if(typeOfEndpoint == "binary" & binaryRelativeScale_tn == "OR"){
    
    # for OR normal distribution
    if(binaryDist_tn == "norm"){
      return(oddsRatioCItoP_tn(OR_tn, P_t1))
    }
    # for OR half normal distribution
    if(binaryDist_tn != "norm"){
      return(oddsRatioHalfNormalToP_tn(ORHalfNorm_tn, binaryDist_tn, P_t1))
    }
  }
  
  # Binary for risk ratio
  #################
  if(typeOfEndpoint == "binary" & binaryRelativeScale_tn == "RR"){
    
    # for RR normal distribution
    if(binaryDist_tn == "norm"){
      return(riskRatioCItoP_tn(RR_tn, P_t1))
    }
    # for RR half normal distribution
    if(binaryDist_tn != "norm"){
      return(riskRatioHalfNormalToP_tn(RRHalfNorm_tn, binaryDist_tn, P_t1))
    }
  }
  
  # Binary for risk difference
  ######################
  if(typeOfEndpoint == "binary" & binaryRelativeScale_tn == "RD"){
    
    # for RD normal distribution
    if(binaryDist_tn == "norm"){
      return(riskDifferenceCItoP_tn(RD_tn, P_t1))
    }
    # for RD half normal distribution
    if(binaryDist_tn != "norm"){
      return(riskDifferenceHalfNormalToP_tn(RDHalfNorm_tn, binaryDist_tn, P_t1))
    }
  }

  # Continuous mean and SE
  ################
  if(typeOfEndpoint == "continuous" & continuousInput_tn == "meanAndSE"){
    
    return(meanDifferenceMeanSEToDelta_tn(MCsims, continMean_tn, continSE_tn))
  }
  
  # Continuous mean difference range
  ################
  if(typeOfEndpoint == "continuous" & continuousInput_tn == "range"){
    
    # for continuous normal distribution
    if(continDist_tn == "norm"){
      return(meanDifferenceCIToDelta_tn(MCsims, MD_tn))
    }
    # for continuous half normal
    if(continDist_tn != "norm"){
      return(meanDifferenceHalfNormalToDelta_tn(MCsims, MDHalfNorm_tn, continDist_tn))
    }
  }
  
  
  # survival 
  ##############
  if(typeOfEndpoint == "survival"){
    
    # for hazard ratio normal distribution
    if(survivalDist_tn == "norm"){
      return(hazardRatioCIToExpectedSurvival_tn(survivalType, lambda_t1,gamma_t1, HR_tn, MCsims))
    }
    # for hazard ratio half normal distribution
    if(survivalDist_tn != "norm"){
      return(hazardRatioHalfNormalToExpectedSurvival_tn(survivalType, survivalDist_tn, lambda_t1,gamma_t1,
                                                  HRHalfNorm_tn, MCsims))
    }
  }
  
  
}


# test function - passes basic tests , maybe check more!
# outcome_tn <- simOutcome_tn(
#   
#   MCsims = 100,
#   typeOfEndpoint = "survival",
#   ## binary
#   binaryRelativeScale_tn =  "OR", # "RR", "RD"
#   P_t1 = rbeta(10, 10, 40),
#   
#   binaryDist_tn = "alwaysPositive", # , "alwaysNegative", "norm"
#   # binary normal dist
#   OR_tn = c(0.71, 1.18),
#   RR_tn = c(0.9, 1.1),
#   RD_tn = c(-5, 5),
#   # binary half norm dist
#   ORHalfNorm_tn = 2, # upper bound if binaryDist_t2 == alwaysPositive, lower bound if binaryDist_t2 == alwaysNegative,
#   RRHalfNorm_tn = 2,
#   RDHalfNorm_tn = 0.7,
#   #
#   ## continuous
#   continuousInput_tn =  "range", # "meanAndSE"
#   # contin meanAndSE
#   continMean_tn = 1,
#   continSE_tn = 0.5,
#   continDist_tn =  "alwaysPositive", #  "alwaysNegative"  "norm"
#   # contin normal mean difference 
#   MD_tn =  c(-2, 2), # 95% intervals
#   # contin half normal mean difference
#   MDHalfNorm_tn = 20, # upper bound if continDist_tn == alwaysPositive, lower bound if continDist_tn == alwaysNegative,
#   #
#   ## survival
#   survivalType = "exponential", # "weibull"
#   survivalDist_tn =  "alwaysPositive", # "alwaysNegative" "norm"
#   lambda_t1 = 10,
#   gamma_t1 = 1,
#   # survival norm dist 
#   HR_tn =   c(0.71, 1.18),
#   HRHalfNorm_tn = 1.4 # single value (bounds depend on whether the half normal is "alwaysPositive" or "alwaysNegative")
# )
# outcome_tn



# test data 
# numberOfTreatments = 3
# MCsims = 100 # required for continuous functions
# typeOfEndpoint = "survival" #"binary"
# ### binary
# binaryRelativeScale_t2 = "OR"
# binaryRelativeScale_t3 = "OR"
# binaryRelativeScale_t4 = "OR"
# P_t1 = rbeta(100, 10, 40)
# binaryDist_t2 = "alwaysPositive"
# binaryDist_t3 ="alwaysPositive" 
# binaryDist_t4 = "alwaysPositive"
# # norm dist
# OR_t2= c(0.71, 1.18)
# OR_t3 = c(0.71, 1.18)
# OR_t4= c(0.71, 1.18)
# RR_t2 =c(0.9, 1.1)
# RR_t3 =c(0.9, 1.1)
# RR_t4 =c(0.9, 1.1)
# RD_t2 =c(-5, 5)
# RD_t3=c(-5, 5)
# RD_t4=c(-5, 5)
# # half norm dist
# ORHalfNorm_t2=2
# ORHalfNorm_t3=2
# ORHalfNorm_t4=2
# RRHalfNorm_t2=2
# RRHalfNorm_t3=2
# RRHalfNorm_t4=2
# RDHalfNorm_t2=2
# RDHalfNorm_t3=2
# RDHalfNorm_t4=2
# ### continuous
# continuousInput_t2="range" # "meanAndSE",
# continuousInput_t3="range"# "meanAndSE",
# continuousInput_t4="range" # "meanAndSE",
# # contin meanAndSE
# continMean_t2=1
# continMean_t3=1
# continMean_t4=1
# continSE_t2=.5
# continSE_t3=.5
# continSE_t4=.5
# continDist_t2="alwaysPositive" #  "alwaysNegative"  "norm",
# continDist_t3="alwaysPositive" #  "alwaysNegative"  "norm",
# continDist_t4="alwaysPositive"#  "alwaysNegative"  "norm",
# # contin normal mean difference 
# MD_t2=c(-2, 2)
# MD_t3=c(-2, 2)
# MD_t4=c(-2, 2)
# # contin half normal mean difference
# MDHalfNorm_t2=2
# MDHalfNorm_t3=2
# MDHalfNorm_t4=2
# ### survival
# survivalType = "weibull"
# survivalDist_t2="alwaysPositive"#  "alwaysNegative"  "norm",,
# survivalDist_t3="alwaysPositive" #  "alwaysNegative"  "norm",,
# survivalDist_t4="alwaysPositive" #  "alwaysNegative"  "norm",,
# lambda_t1=10
# gamma_t1=1
# # survival norm dist 
# HR_t2=c(0.71, 1.18)
# HR_t3=c(0.71, 1.18)
# HR_t4=c(0.71, 1.18)
# HRHalfNorm_t2=1.4
# HRHalfNorm_t3=1.4
# HRHalfNorm_t4=1.4



# master function 
# generic for all outcome types: 
# output: outcome_t (a matrix of the appropriate outcomes - used as input into outcomeToNB_t function)

simOutcomeMatrix <- function(numberOfTreatments,
                             MCsims, # required for continuous functions
                             typeOfEndpoint,
                             ### binary
                             binaryRelativeScale_t2,
                             binaryRelativeScale_t3,
                             binaryRelativeScale_t4,
                             P_t1,
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
                             
){
  set.seed(5)
  
  # assign outcome_t1 
  ######################
  if(typeOfEndpoint == "binary"){
    outcome_t1 <- P_t1
  }
  
  if(typeOfEndpoint == "continuous"){
    outcome_t1 <- rep(0, MCsims)
  }
  
  if(typeOfEndpoint == "survival"){
    
    if(survivalType == "exponential"){
      outcome_t1 <- rep(1/lambda_t1, MCsims)
    }
    
    if(survivalType == "weibull"){
      outcome_t1 <- rep(lambda_t1^(-1/gamma_t1)*gamma(1/gamma_t1 +1), MCsims)
    }
    
  }


  # simulate t2 vector
  ######################
  outcome_t2 <- simOutcome_tn(MCsims, typeOfEndpoint, binaryRelativeScale_t2,P_t1,binaryDist_t2,
                              OR_t2,RR_t2,RD_t2,ORHalfNorm_t2,RRHalfNorm_t2,RDHalfNorm_t2,
                              continuousInput_t2,continMean_t2,continSE_t2,continDist_t2,MD_t2,MDHalfNorm_t2,
                              survivalType,survivalDist_t2,lambda_t1,gamma_t1,HR_t2,HRHalfNorm_t2
  )

  # simulate t3
  #################
  outcome_t3 <- if(numberOfTreatments <= 2 ) { # if there is only 2 treatments then this is given a vector of NAs
    rep(NA, MCsims) 
  } else {
    simOutcome_tn(MCsims, typeOfEndpoint, binaryRelativeScale_t3,P_t1,binaryDist_t3,
                  OR_t3,RR_t3,RD_t3,ORHalfNorm_t3,RRHalfNorm_t3,RDHalfNorm_t3,
                  continuousInput_t3,continMean_t3,continSE_t3,continDist_t3,MD_t3,MDHalfNorm_t3,
                  survivalType,survivalDist_t3,lambda_t1,gamma_t1,HR_t3,HRHalfNorm_t3
    )

  }

  # simulate t4
  ###############
  outcome_t4 <- if(numberOfTreatments <= 3 ) { # if there is only 2 treatments then this is given a vector of NAs
    rep(NA, MCsims) 
  } else {
    simOutcome_tn(MCsims, typeOfEndpoint, binaryRelativeScale_t4,P_t1,binaryDist_t4,
                  OR_t4,RR_t4,RD_t4,ORHalfNorm_t4,RRHalfNorm_t4,RDHalfNorm_t4,
                  continuousInput_t4,continMean_t4,continSE_t4,continDist_t4,MD_t4,MDHalfNorm_t4,
                  survivalType,survivalDist_t4,lambda_t1,gamma_t1,HR_t4,HRHalfNorm_t4
    )
    
  }

  # add all vectors to the matrix outcome_t
  ####################################
  # and return this
  outcome_t <- matrix(c(outcome_t1, outcome_t2, outcome_t3, outcome_t4), ncol = 4)

  return(outcome_t)

}



# test function - produces correct output - maybe need to test more
# outcome_t <- simOutcomeMatrix(numberOfTreatments = 3,
#                               MCsims = 100, # required for continuous functions
#                               typeOfEndpoint = "continuous",    # 'binary', "continuous''survival"
#                               ### binary
#                               binaryRelativeScale_t2 = "OR",
#                               binaryRelativeScale_t3 = "RR",
#                               binaryRelativeScale_t4 = "OR",
#                               P_t1 = rbeta(100, 10, 40),
#                               binaryDist_t2 = "alwaysPositive",
#                               binaryDist_t3 ="alwaysPositive" ,
#                               binaryDist_t4 = "alwaysPositive",
#                               # norm dist
#                               OR_t2= c(0.71, 1.18),
#                               OR_t3 = c(0.71, 1.18),
#                               OR_t4= c(0.71, 1.18),
#                               RR_t2 =c(0.9, 1.1),
#                               RR_t3 =c(0.9, 1.1),
#                               RR_t4 =c(0.9, 1.1),
#                               RD_t2 =c(-5, 5),
#                               RD_t3=c(-5, 5),
#                               RD_t4=c(-5, 5),
#                               # half norm dist
#                               ORHalfNorm_t2=2,
#                               ORHalfNorm_t3=2,
#                               ORHalfNorm_t4=2,
#                               RRHalfNorm_t2=2,
#                               RRHalfNorm_t3=2,
#                               RRHalfNorm_t4=2,
#                               RDHalfNorm_t2=2,
#                               RDHalfNorm_t3=2,
#                               RDHalfNorm_t4=2,
#                               ### continuous
#                               continuousInput_t2="meanAndSE", # "range", # "meanAndSE",
#                               continuousInput_t3="range", # "meanAndSE",
#                               continuousInput_t4="range", # "meanAndSE",
#                               # contin meanAndSE
#                               continMean_t2=1,
#                               continMean_t3=1,
#                               continMean_t4=1,
#                               continSE_t2=.5,
#                               continSE_t3=.5,
#                               continSE_t4=.5,
#                               continDist_t2="alwaysPositive", #  "alwaysNegative"  "norm",
#                               continDist_t3="alwaysPositive", #  "alwaysNegative"  "norm",
#                               continDist_t4="alwaysPositive", #  "alwaysNegative"  "norm",
#                               # contin normal mean difference 
#                               MD_t2=c(-2, 2),
#                               MD_t3=c(-2, 2),
#                               MD_t4=c(-2, 2),
#                               # contin half normal mean difference
#                               MDHalfNorm_t2=2,
#                               MDHalfNorm_t3=2,
#                               MDHalfNorm_t4=2,
#                               ### survival
#                               survivalType = "weibull", # "exponential", # "weibull",
#                               survivalDist_t2="alwaysPositive", #  "alwaysNegative"  "norm",,
#                               survivalDist_t3="alwaysPositive", #  "alwaysNegative"  "norm",,
#                               survivalDist_t4="alwaysPositive", #  "alwaysNegative"  "norm",,
#                               lambda_t1=10,
#                               gamma_t1=1,
#                               # survival norm dist 
#                               HR_t2=c(0.71, 1.18),
#                               HR_t3=c(0.71, 1.18),
#                               HR_t4=c(0.71, 1.18),
#                               HRHalfNorm_t2=1.4,
#                               HRHalfNorm_t3=1.4,
#                               HRHalfNorm_t4=1.4)
# outcome_t


