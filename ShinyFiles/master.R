############
# master function


# epi stuff
# # from meta analysis of HTA results in Djulbegovic Cochrane article:
# #  -0.089 mean on log scale i.e. outcomes favour the new treatment
# exp(-0.089) # outcome harm
# exp(+0.089) # outcome benefit (assuming symmetry)
# 
# # from visual inspection of HTA results
# sigma = 0.8 # an approximation
# 
# # CALCULATE CIs
# # outcome is a harm:
# exp(-0.089 + sigma*1.96) # = 0.19 to 4.39
# exp(-0.089 - sigma*1.96) # = 
# # outcome is a benefit:
# exp(0.089 + sigma*1.96) # = 
# exp(0.089 - sigma*1.96) # = 0.23 to 5.24




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





# TEST DATA P1
# typeOfEndpoint = "binary"  # 'binary', "continuous''survival"
# baselineInput = "range"
# MCsims = 100000
# baselineRange = c(0.525, 0.525)
# nEvents = 10
# nAtRisk = 20
# numberOfTreatments = 3
# binaryRelativeScale_t2 = "OR"
# binaryRelativeScale_t3 = "OR"
# binaryRelativeScale_t4 = "OR"
# binaryDist_t2 = "norm"
# binaryDist_t3 ="norm"
# binaryDist_t4 = "alwaysPositive"
# OR_t2= c(0.375, 2.66)
# OR_t3 = c(0.375, 2.66)
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
# continuousInput_t2="meanAndSE"  # "range", # "meanAndSE",
# continuousInput_t3="range"  # "meanAndSE",
# continuousInput_t4="range"  # "meanAndSE",
# continMean_t2=1
# continMean_t3=1
# continMean_t4=1
# continSE_t2=.5
# continSE_t3=.5
# continSE_t4=.5
# continDist_t2="alwaysPositive"  #  "alwaysNegative"  "norm",
# continDist_t3="alwaysPositive"  #  "alwaysNegative"  "norm",
# continDist_t4="alwaysPositive"  #  "alwaysNegative"  "norm",
# MD_t2=c(-2, 2)
# MD_t3=c(-2, 2)
# MD_t4=c(-2, 2)
# MDHalfNorm_t2=2
# MDHalfNorm_t3=2
# MDHalfNorm_t4=2
# survivalType = "weibull"  # "exponential", # "weibull",
# survivalDist_t2="alwaysPositive"  #  "alwaysNegative"  "norm",,
# survivalDist_t3="alwaysPositive"  #  "alwaysNegative"  "norm",,
# survivalDist_t4="alwaysPositive"  #  "alwaysNegative"  "norm",,
# lambda_t1=10
# gamma_t1=1
# HR_t2=c(0.71, 1.18)
# HR_t3=c(0.71, 1.18)
# HR_t4=c(0.71, 1.18)
# HRHalfNorm_t2=1.4
# HRHalfNorm_t3=1.4
# HRHalfNorm_t4=1.4
# typeOfOutcome= "netHealth"  # "benefit" or "harm" "netHealth"
# tCostsDependOnEvent= "No"
# MCD_t2 = 0.0
# MCD_t3 = 0.0
# MCD_t4 = 0.3
# INBBinaryEvent = 0.7409
# cost_t1 = 687
# cost_t2= 1746
# cost_t3 = 2433
# cost_t4 = 60
# k = 15000
# currencySymbol = "£"
# incidence = 1563
# discountRate = 3.5 # NB this is how the discount is added
# timeInformation = 15
# nameOf_t1 = "APs"
# nameOf_t2 = "PI"
# nameOf_t3 = "APs + PI"
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
# typeOfResearch = "feasibility"  # "RCT"   # "feasibility"
# durationOfResearch = 4
# costResearchFunder = 2200000
# utilisation_t1 = 100
# utilisation_t2 = 0
# utilisation_t3 = 0
# utilisation_t4 = 0
# costHealthSystem = 60000
# durationOfResearchDefinitive = 6
# durationOfResearchFeas = 2
# costResearchFunderFeas = 601481
# costResearchFunderDefinitive =  2522710
# probabilityOfDefinitiveResearch = 0.5
# costHealthSystemFeas = 150000
# costHealthSystemDefinitive = 490000






# test data
# typeOfEndpoint = "continuous"  # 'binary'  "continuous''survival"
# baselineInput = "range"
# MCsims = 900000
# baselineRange = c(0.525, 0.525)
# nEvents = 10
# nAtRisk = 20
# numberOfTreatments = 3
# binaryRelativeScale_t2 = "OR"
# binaryRelativeScale_t3 = "OR"
# binaryRelativeScale_t4 = "OR"
# binaryDist_t2 = "norm"
# binaryDist_t3 ="norm"
# binaryDist_t4 = "alwaysPositive"
# OR_t2= c(0.375, 2.66)
# OR_t3 = c(0.375, 2.66)
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
# continuousInput_t2="meanAndSE"  # "range"  # "meanAndSE"
# continuousInput_t3="range"  # "meanAndSE"
# continuousInput_t4="range"  # "meanAndSE"
# continMean_t2=1
# continMean_t3=1
# continMean_t4=1
# continSE_t2=.5
# continSE_t3=.5
# continSE_t4=.5
# continDist_t2="alwaysPositive"  #  "alwaysNegative"  "norm"
# continDist_t3="alwaysPositive"  #  "alwaysNegative"  "norm"
# continDist_t4="alwaysPositive"  #  "alwaysNegative"  "norm"
# MD_t2=c(-2, 2)
# MD_t3=c(-2, 2)
# MD_t4=c(-2, 2)
# MDHalfNorm_t2=2
# MDHalfNorm_t3=2
# MDHalfNorm_t4=2
# survivalType = "weibull"  # "exponential"  # "weibull"
# survivalDist_t2="alwaysPositive"  #  "alwaysNegative"  "norm"
# survivalDist_t3="alwaysPositive"  #  "alwaysNegative"  "norm"
# survivalDist_t4="alwaysPositive"  #  "alwaysNegative"  "norm"
# lambda_t1=10
# gamma_t1=1
# HR_t2=c(0.71, 1.18)
# HR_t3=c(0.71, 1.18)
# HR_t4=c(0.71, 1.18)
# HRHalfNorm_t2=1.4
# HRHalfNorm_t3=1.4
# HRHalfNorm_t4=1.4
# typeOfOutcome= "netHealth"  # "benefit" or "harm" "netHealth"
# tCostsDependOnEvent= "No"
# MCD_t2 = 0.0
# MCD_t3 = 0.0
# MCD_t4 = 0.3
# cost_t1 = 687
# cost_t2= 1746
# cost_t3 = 2433
# cost_t4 = 60
# k = 15000
# currencySymbol = "£"
# incidence = 1563
# discountRate = 3.5   # NB - this is how ui provides the discount rate!
# timeInformation = 15
# nameOf_t1 = "APs"
# nameOf_t2 = "PI"
# nameOf_t3 = "APs + PI"
# nameOf_t4 = "4"
# costEvent_t1 = 10
# costEvent_t2 = 20
# costEvent_t3 = 3
# costEvent_t4 = 40
# costNotEvent_t1 = 50
# costNotEvent_t2 = 60
# costNotEvent_t3 = 70
# costNotEvent_t4 = 80
# numberS0States = 4
# numberS1States = 4
# utility_s01 = 0
# utility_s02 = 0.11
# utility_s03 = 0.41
# utility_s04 = 0.58
# utility_s11 = 0.7
# utility_s12 = 0.81
# utility_s13 = 0.96
# utility_s14 = 1
# lifeDuration_s01 = 0
# lifeDuration_s02 = 7.11
# lifeDuration_s03 = 12.52
# lifeDuration_s04 = 12.52
# lifeDuration_s11 = 16.73
# lifeDuration_s12 = 16.73
# lifeDuration_s13 = 19.23
# lifeDuration_s14 = 19.23
# cost_s01 = 0
# cost_s02 = 45450
# cost_s03 = 154324
# cost_s04 = 154324
# cost_s11 = 27047
# cost_s12 = 27047
# cost_s13 = 19575
# cost_s14 = 19575
# probability_s01 = 0.29
# probability_s02 = 0.07
# probability_s03 = 0.41
# probability_s04 = 0.23
# probability_s11 = 0.42
# probability_s12 = 0.24
# probability_s13 = 0.2
# probability_s14 = 0.14
# deltaUnitUtilityDirection = "decrease"  # "increase" # Q Is an increase in the primary outcome expected to be associated with an increase or decrease in health state utility?
# deltaUnitUtilitySize = 0.1  # Q By how much is a one unit increase in the primary outcome expected to increase/decrease the health state utility?
# treatmentDurationMonths = 12  # Q how long is the treatment effect expected to last
# deltaUnitCostsDirection = "decrease"  # "increase" # Q Is an increase in the primary outcome expected to be associated with disease related cost increases or decreases?
# deltaUnitCostsSize = 200  # Q By how much is a one unit increase in the primary outcome expected to increase/decrease disease related costs?
# treatmentCostsMonthly_t1 = 1000000  # used in survival too
# treatmentCostsMonthly_t2 = 200 # used in survival too
# treatmentCostsMonthly_t3 = 300 # used in survival too
# treatmentCostsMonthly_t4 = 400 # used in survival too
# utilityPreTransition = 0.5  # Q What is the health utility associated with the pre-progression health state?
# monthlyCostPreTransition = 2000  # Q What are the expected monthly disease related costs associated with the pre-transition health state?
# treatUntilProgression_t1 = "Yes"  # "No"  Q Are individuals always treated until progression under the baseline treatment?
# maxDurationOfTreatmentMonths_t1 = NA  # Q what is the maximum number of months that the baseline treatment will be given?
# treatUntilProgression_t2 = "No"  # "No" "Yes"
# maxDurationOfTreatmentMonths_t2 = 12
# treatUntilProgression_t3 = "No"  # "No" "Yes"
# maxDurationOfTreatmentMonths_t3 = 24
# treatUntilProgression_t4 = "No"  # "No" "Yes"
# maxDurationOfTreatmentMonths_t4 = 24
# typeOfResearch = "feasibility"  # "RCT"   # "feasibility"
# durationOfResearch = 4
# costResearchFunder = 2200000
# utilisation_t1 = 100
# utilisation_t2 = 0
# utilisation_t3 = 0
# utilisation_t4 = 0
# costHealthSystem = 60000
# durationOfResearchDefinitive = 6
# durationOfResearchFeas = 2
# costResearchFunderFeas = 601481
# costResearchFunderDefinitive =  2522710
# probabilityOfDefinitiveResearch = 0.5
# costHealthSystemFeas = 150000
# costHealthSystemDefinitive = 490000




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
  numberS0States  ,numberS1States  ,
  utility_s01  , utility_s02 , utility_s03 , utility_s04 ,
  utility_s11 , utility_s12 , utility_s13 , utility_s14 ,
  lifeDuration_s01  , lifeDuration_s02 , lifeDuration_s03 , lifeDuration_s04 ,
  lifeDuration_s11 , lifeDuration_s12 , lifeDuration_s13 , lifeDuration_s14 ,
  cost_s01  , cost_s02 , cost_s03 , cost_s04 ,
  cost_s11 , cost_s12 , cost_s13 , cost_s14 ,
  probability_s01  , probability_s02 , probability_s03 , probability_s04 ,
  probability_s11 , probability_s12 , probability_s13 , probability_s14 ,
  deltaUnitUtilityDirection , deltaUnitUtilitySize , 
  treatmentDurationMonths, deltaUnitCostsDirection, deltaUnitCostsSize,
  treatmentCostsMonthly_t1, treatmentCostsMonthly_t2, treatmentCostsMonthly_t3, treatmentCostsMonthly_t4,
  utilityPreTransition, monthlyCostPreTransition,
  treatUntilProgression_t1 , maxDurationOfTreatmentMonths_t1, 
  treatUntilProgression_t2 ,maxDurationOfTreatmentMonths_t2 ,
  treatUntilProgression_t3 ,maxDurationOfTreatmentMonths_t3 ,
  treatUntilProgression_t4 ,maxDurationOfTreatmentMonths_t4,

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
  NBOutput <- outcomeToNB2_t(outcome_t, MCsims,
                        typeOfEndpoint, typeOfOutcome, tCostsDependOnEvent,
                        MCD_t2, MCD_t3, MCD_t4,
                        cost_t1,cost_t2, cost_t3, cost_t4, k,
                        currencySymbol, 
                        incidence, discountRate, timeInformation,
                        nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                        numberOfTreatments,
                        costEvent_t1,costEvent_t2,costEvent_t3,costEvent_t4,
                        costNotEvent_t1,costNotEvent_t2,costNotEvent_t3,costNotEvent_t4,
                        numberS0States ,numberS1States ,
                        utility_s01 , utility_s02 , utility_s03 , utility_s04 ,
                        utility_s11 , utility_s12 , utility_s13 , utility_s14 ,
                        lifeDuration_s01 , lifeDuration_s02 , lifeDuration_s03 , lifeDuration_s04 ,
                        lifeDuration_s11 , lifeDuration_s12 , lifeDuration_s13 , lifeDuration_s14 ,
                        cost_s01 , cost_s02 , cost_s03 , cost_s04 ,
                        cost_s11 , cost_s12 , cost_s13 , cost_s14 ,
                        probability_s01  , probability_s02 , probability_s03 , probability_s04 ,
                        probability_s11 , probability_s12 , probability_s13 , probability_s14 ,
                        deltaUnitUtilityDirection , deltaUnitUtilitySize , 
                        treatmentDurationMonths, deltaUnitCostsDirection, deltaUnitCostsSize,
                        treatmentCostsMonthly_t1, treatmentCostsMonthly_t2, treatmentCostsMonthly_t3, treatmentCostsMonthly_t4,
                        utilityPreTransition, monthlyCostPreTransition,
                        treatUntilProgression_t1 , maxDurationOfTreatmentMonths_t1, 
                        treatUntilProgression_t2 ,maxDurationOfTreatmentMonths_t2 ,
                        treatUntilProgression_t3 ,maxDurationOfTreatmentMonths_t3 ,
                        treatUntilProgression_t4 ,maxDurationOfTreatmentMonths_t4
                        )
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
# # home
# source("C:/Users/David/Desktop/Work/R files/Shiny/Tool/ShinyFiles/SupplementaryFunctions.R", local = TRUE)
# source("C:/Users/David/Desktop/Work/R files/Shiny/Tool/ShinyFiles/SupplementaryFunctionsFeas.R", local = TRUE)
# source("C:/Users/David/Desktop/Work/R files/Shiny/Tool/ShinyFiles/ReconFunctions.R", local = TRUE)
# source("C:/Users/David/Desktop/Work/R files/Shiny/Tool/ShinyFiles/EpiInputFunctions.R", local = TRUE)
# source("C:/Users/David/Desktop/Work/R files/Shiny/Tool/ShinyFiles/PlottingFunctions.R", local = TRUE)
# source("C:/Users/David/Desktop/Work/R files/Shiny/Tool/ShinyFiles/EpiCalcFunctions.R", local = TRUE)
# source("C:/Users/David/Desktop/Work/R files/Shiny/Tool/ShinyFiles/NBCalcFunctions.R", local = TRUE)

# 
# master(
#     typeOfEndpoint = "binary", # 'binary', "continuous''survival"
#     baselineInput = "range",
#     MCsims = 900000,
#     baselineRange = c(0.525, 0.525) ,
#     nEvents = 10,
#     nAtRisk = 20,
#     numberOfTreatments = 2,
#     binaryRelativeScale_t2 = "OR",
#     binaryRelativeScale_t3 = "OR",
#     binaryRelativeScale_t4 = "OR",
#     binaryDist_t2 = "alwaysPositive", # "alwaysPositive" "alwaysNegative" "norm"
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
#     typeOfOutcome= "benefit", # "benefit" or "harm" "netHealth"
#     tCostsDependOnEvent= "No",
#     MCD_t2 = 0.0,
#     MCD_t3 = 0.0,
#     MCD_t4 = 0.3,
#     cost_t1 = 6879999,
#     cost_t2= 1746,
#     cost_t3 = 2433,
#     cost_t4 = 60,
#     k = 15000,
#     currencySymbol = "£",
#     incidence = 1563,
#     discountRate = 3.5 , # NB - this is how ui provides the discount rate!
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
#     numberS0States = 4,
#     numberS1States = 4 ,
#     utility_s01 = 0,
#     utility_s02 = 0.11,
#     utility_s03 = 0.41,
#     utility_s04 = 0.58,
#     utility_s11 = 0.7,
#     utility_s12 = 0.81,
#     utility_s13 = 0.96,
#     utility_s14 = 1,
#     lifeDuration_s01 = 0,
#     lifeDuration_s02 = 7.11,
#     lifeDuration_s03 = 12.52,
#     lifeDuration_s04 = 12.52,
#     lifeDuration_s11 = 16.73,
#     lifeDuration_s12 = 16.73,
#     lifeDuration_s13 = 19.23,
#     lifeDuration_s14 = 19.23,
#     cost_s01 = 0,
#     cost_s02 = 45450,
#     cost_s03 = 154324,
#     cost_s04 = 154324,
#     cost_s11 = 27047,
#     cost_s12 = 27047,
#     cost_s13 = 19575,
#     cost_s14 = 19575,
#     probability_s01 = 0.29,
#     probability_s02 = 0.07,
#     probability_s03 = 0.41,
#     probability_s04 = 0.23,
#     probability_s11 = 0.42,
#     probability_s12 = 0.24,
#     probability_s13 = 0.2,
#     probability_s14 = 0.14,
#     deltaUnitUtilityDirection = "decrease", # "increase" # Q Is an increase in the primary outcome expected to be associated with an increase or decrease in health state utility?
#     deltaUnitUtilitySize = 0.1, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease the health state utility?
#     treatmentDurationMonths = 12, # Q how long is the treatment effect expected to last
#     deltaUnitCostsDirection = "decrease", # "increase" # Q Is an increase in the primary outcome expected to be associated with disease related cost increases or decreases?
#     deltaUnitCostsSize = 200, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease disease related costs?
#     treatmentCostsMonthly_t1 = 100, # used in survival too
#     treatmentCostsMonthly_t2 = 200,# used in survival too
#     treatmentCostsMonthly_t3 = 300,# used in survival too
#     treatmentCostsMonthly_t4 = 400,# used in survival too
#     utilityPreTransition = 0.5, # Q What is the health utility associated with the pre-progression health state?
#     monthlyCostPreTransition = 2000, # Q What are the expected monthly disease related costs associated with the pre-transition health state?
#     treatUntilProgression_t1 = "Yes", # "No"  Q Are individuals always treated until progression under the baseline treatment?
#     maxDurationOfTreatmentMonths_t1 = NA, # Q what is the maximum number of months that the baseline treatment will be given?
#     treatUntilProgression_t2 = "No", # "No" "Yes"
#     maxDurationOfTreatmentMonths_t2 = 12,
#     treatUntilProgression_t3 = "No", # "No" "Yes"
#     maxDurationOfTreatmentMonths_t3 = 24,
#     treatUntilProgression_t4 = "No", # "No" "Yes"
#     maxDurationOfTreatmentMonths_t4 = 24,
# 
#     typeOfResearch = "feasibility", # "RCT",  # "feasibility",
#     durationOfResearch = 4,
#     costResearchFunder = 2200000,
#     utilisation_t1 = 100,
#     utilisation_t2 = 0,
#     utilisation_t3 = 0,
#     utilisation_t4 = 0,
#     costHealthSystem = 60000,
#     durationOfResearchDefinitive = 6,
#     durationOfResearchFeas = 2,
#     costResearchFunderFeas = 601481,
#     costResearchFunderDefinitive =  2522710,
#     probabilityOfDefinitiveResearch = 0.5,
#     costHealthSystemFeas = 150000,
#     costHealthSystemDefinitive = 490000
# )





































































