## P5
######



source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/SupplementaryFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/SupplementaryFunctionsFeas.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/master.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/ReconFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/EpiInputFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/PlottingFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/EpiCalcFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/NBCalcFunctions.R", local = TRUE)


# Natural outcome
####################################
master(
  typeOfEndpoint = "binary", # 'binary', "continuous''survival"
  baselineInput = "eventsNonEvents", # "eventsNonEvents" "range"
  MCsims = 900000,
  
  baselineRange = NA ,
  
  nEvents = 162,
  nAtRisk = 294,
  numberOfTreatments = 2,
  binaryRelativeScale_t2 = "OR",
  
  binaryRelativeScale_t3 = NA,
  binaryRelativeScale_t4 = NA,
  
  binaryDist_t2 = "norm",
  binaryDist_t3 = NA ,
  binaryDist_t4 = NA,
  
  OR_t2= c(0.23, 5.24),
  
  OR_t3 = NA,
  OR_t4= NA,
  RR_t2 =NA,
  RR_t3 =NA,
  RR_t4 =NA,
  RD_t2 =NA,
  RD_t3=NA,
  RD_t4=NA,
  ORHalfNorm_t2=NA,
  ORHalfNorm_t3=NA,
  ORHalfNorm_t4=NA,
  RRHalfNorm_t2=NA,
  RRHalfNorm_t3=NA,
  RRHalfNorm_t4=NA,
  RDHalfNorm_t2=NA,
  RDHalfNorm_t3=NA,
  RDHalfNorm_t4=NA,
  continuousInput_t2=NA, # "range", # "meanAndSE",
  continuousInput_t3=NA ,# "meanAndSE",
  continuousInput_t4=NA, # "meanAndSE",
  continMean_t2=NA,
  continMean_t3=NA,
  continMean_t4=NA,
  continSE_t2=NA,
  continSE_t3=NA,
  continSE_t4=NA,
  continDist_t2=NA, #  "alwaysNegative"  "norm",
  continDist_t3=NA, #  "alwaysNegative"  "norm",
  continDist_t4=NA, #  "alwaysNegative"  "norm",
  MD_t2=NA,
  MD_t3=NA,
  MD_t4=NA,
  MDHalfNorm_t2=NA,
  MDHalfNorm_t3=NA,
  MDHalfNorm_t4=NA,
  survivalType = NA, # "exponential", # "weibull",
  survivalDist_t2=NA ,#  "alwaysNegative"  "norm",,"alwaysPositive" 
  survivalDist_t3=NA, #  "alwaysNegative"  "norm",,"alwaysPositive" 
  survivalDist_t4=NA ,#  "alwaysNegative"  "norm",,
  lambda_t1= NA ,
  gamma_t1=NA,
  HR_t2=NA,
  HR_t3=NA,
  HR_t4=NA,
  HRHalfNorm_t2=NA,
  HRHalfNorm_t3=NA,
  HRHalfNorm_t4=NA,
  
  typeOfOutcome= "benefit", # "benefit" or "harm" "netHealth"
  
  tCostsDependOnEvent= "No",
  
  MCD_t2 = 0.0,
  
  MCD_t3 = NA,
  MCD_t4 = NA,
  cost_t1 = NA ,
  cost_t2= NA,
  cost_t3 = NA,
  cost_t4 = NA,
  
  k = NA,
  currencySymbol = "£",
  
  incidence = 8800 ,
  discountRate = 3.5 , # NB - this is how ui provides the discount rate!
  timeInformation = 15,
  
  nameOf_t1 = "Late PTP",
  nameOf_t2 = "Early PTP",
  
  nameOf_t3 = NA,
  nameOf_t4 = NA,
  costEvent_t1 = NA,
  costEvent_t2 = NA,
  costEvent_t3 = NA,
  costEvent_t4 = NA,
  costNotEvent_t1 = NA,
  costNotEvent_t2 = NA,
  costNotEvent_t3 = NA,
  costNotEvent_t4 = NA,
  numberS0States = NA, # if event does not occur
  numberS1States = NA , # if event occurs
  utility_s01 = NA, # if recovery does not occur
  utility_s02 = NA,
  utility_s03 = NA,
  utility_s04 = NA,
  utility_s11 = NA, # recovery does occur
  utility_s12 = NA,
  utility_s13 = NA,
  utility_s14 = NA,
  lifeDuration_s01 = NA, #if recovery does not occur
  lifeDuration_s02 = NA,
  lifeDuration_s03 = NA,
  lifeDuration_s04 = NA,
  lifeDuration_s11 = NA, # recovery does occur
  lifeDuration_s12 = NA,
  lifeDuration_s13 = NA,
  lifeDuration_s14 = NA,
  cost_s01 = NA,   #if recovery does not occur
  cost_s02 = NA,
  cost_s03 = NA,
  cost_s04 = NA,
  cost_s11 = NA, # recovery does occur
  cost_s12 = NA,
  cost_s13 = NA,
  cost_s14 = NA,
  probability_s01 = NA, #if recovery does not occur
  probability_s02 = NA,
  probability_s03 = NA,
  probability_s04 = NA,
  probability_s11 = NA, # recovery does occur
  probability_s12 = NA,
  probability_s13 = NA,
  probability_s14 = NA,
  deltaUnitUtilityDirection = NA, # "increase" # Q Is an increase in the primary outcome expected to be associated with an increase or decrease in health state utility?
  deltaUnitUtilitySize = NA, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease the health state utility?
  treatmentDurationMonths = NA, # Q how long is the treatment effect expected to last
  deltaUnitCostsDirection = NA, # "increase" # Q Is an increase in the primary outcome expected to be associated with disease related cost increases or decreases?
  deltaUnitCostsSize = NA, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease disease related costs?
  treatmentCostsMonthly_t1 = NA, # used in survival too
  treatmentCostsMonthly_t2 = NA,# used in survival too
  treatmentCostsMonthly_t3 = NA,# used in survival too
  treatmentCostsMonthly_t4 = NA,# used in survival too
  utilityPreTransition = NA, # Q What is the health utility associated with the pre-progression health state?
  monthlyCostPreTransition = NA, # Q What are the expected monthly disease related costs associated with the pre-transition health state?
  treatUntilProgression_t1 = NA, # "No"  Q Are individuals always treated until progression under the baseline treatment?
  maxDurationOfTreatmentMonths_t1 = NA, # Q what is the maximum number of months that the baseline treatment will be given?
  treatUntilProgression_t2 = NA, # "No" "Yes"
  maxDurationOfTreatmentMonths_t2 = NA,
  treatUntilProgression_t3 = NA, # "No" "Yes"
  maxDurationOfTreatmentMonths_t3 = NA,
  treatUntilProgression_t4 = NA, # "No" "Yes"
  maxDurationOfTreatmentMonths_t4 = NA,
  
  typeOfResearch = "RCT", # "RCT",  # "feasibility",
  
  durationOfResearch = 5,
  costResearchFunder = 2854000,
  utilisation_t1 = 100,
  utilisation_t2 = 0,
  
  utilisation_t3 = NA,
  utilisation_t4 = NA,
  costHealthSystem = NA,
  
  durationOfResearchDefinitive = NA,
  durationOfResearchFeas = NA,
  costResearchFunderFeas = NA,
  costResearchFunderDefinitive =  NA,
  probabilityOfDefinitiveResearch = NA,
  costHealthSystemFeas = NA,
  costHealthSystemDefinitive = NA
)








# costs and QALYs
####################################
master(
  typeOfEndpoint = "binary", # 'binary', "continuous''survival"
  baselineInput = "eventsNonEvents", # "eventsNonEvents" "range"
  MCsims = 900000,
  
  baselineRange = NA ,
  
  nEvents = 162,
  nAtRisk = 294,
  numberOfTreatments = 2,
  binaryRelativeScale_t2 = "OR",
  
  binaryRelativeScale_t3 = NA,
  binaryRelativeScale_t4 = NA,
  
  binaryDist_t2 = "norm",
  binaryDist_t3 = NA ,
  binaryDist_t4 = NA,
  
  OR_t2= c(0.23, 5.24),
  
  OR_t3 = NA,
  OR_t4= NA,
  RR_t2 =NA,
  RR_t3 =NA,
  RR_t4 =NA,
  RD_t2 =NA,
  RD_t3=NA,
  RD_t4=NA,
  ORHalfNorm_t2=NA,
  ORHalfNorm_t3=NA,
  ORHalfNorm_t4=NA,
  RRHalfNorm_t2=NA,
  RRHalfNorm_t3=NA,
  RRHalfNorm_t4=NA,
  RDHalfNorm_t2=NA,
  RDHalfNorm_t3=NA,
  RDHalfNorm_t4=NA,
  continuousInput_t2=NA, # "range", # "meanAndSE",
  continuousInput_t3=NA ,# "meanAndSE",
  continuousInput_t4=NA, # "meanAndSE",
  continMean_t2=NA,
  continMean_t3=NA,
  continMean_t4=NA,
  continSE_t2=NA,
  continSE_t3=NA,
  continSE_t4=NA,
  continDist_t2=NA, #  "alwaysNegative"  "norm",
  continDist_t3=NA, #  "alwaysNegative"  "norm",
  continDist_t4=NA, #  "alwaysNegative"  "norm",
  MD_t2=NA,
  MD_t3=NA,
  MD_t4=NA,
  MDHalfNorm_t2=NA,
  MDHalfNorm_t3=NA,
  MDHalfNorm_t4=NA,
  survivalType = NA, # "exponential", # "weibull",
  survivalDist_t2=NA ,#  "alwaysNegative"  "norm",,"alwaysPositive" 
  survivalDist_t3=NA, #  "alwaysNegative"  "norm",,"alwaysPositive" 
  survivalDist_t4=NA ,#  "alwaysNegative"  "norm",,
  lambda_t1= NA ,
  gamma_t1=NA,
  HR_t2=NA,
  HR_t3=NA,
  HR_t4=NA,
  HRHalfNorm_t2=NA,
  HRHalfNorm_t3=NA,
  HRHalfNorm_t4=NA,
  
  typeOfOutcome= "netHealth", # "benefit" or "harm" "netHealth"
  
  tCostsDependOnEvent= "No",
  
  MCD_t2 = 0.0,
  
  MCD_t3 = NA,
  MCD_t4 = NA,
  
  cost_t1 = 0 ,
  cost_t2= 14.10 ,
  
  cost_t3 = NA,
  cost_t4 = NA,
  
  k = 15000,
  currencySymbol = "£",
  
  incidence = 8800 ,
  discountRate = 3.5 , # NB - this is how ui provides the discount rate!
  timeInformation = 15,
  
  nameOf_t1 = "Late PTP",
  nameOf_t2 = "Early PTP",
  
  nameOf_t3 = NA,
  nameOf_t4 = NA,
  costEvent_t1 = NA,
  costEvent_t2 = NA,
  costEvent_t3 = NA,
  costEvent_t4 = NA,
  costNotEvent_t1 = NA,
  costNotEvent_t2 = NA,
  costNotEvent_t3 = NA,
  costNotEvent_t4 = NA,
  numberS0States = 4, # if event does not occur
  numberS1States = 4 , # if event occurs
  utility_s01 = 0, # if recovery does not occur
  utility_s02 = 0.11,
  utility_s03 = 0.41,
  utility_s04 = 0.58,
  utility_s11 = 0.70, # recovery does occur
  utility_s12 = 0.81,
  utility_s13 = 0.96,
  utility_s14 = 1,
  lifeDuration_s01 = 0, #if recovery does not occur
  lifeDuration_s02 = 7.11,
  lifeDuration_s03 = 12.52,
  lifeDuration_s04 = 12.52,
  lifeDuration_s11 = 16.73, # recovery does occur
  lifeDuration_s12 = 16.73,
  lifeDuration_s13 = 19.23,
  lifeDuration_s14 = 19.23,
  cost_s01 = 0,   #if recovery does not occur
  cost_s02 = 45450,
  cost_s03 = 154324,
  cost_s04 = 154324,
  cost_s11 = 27047, # recovery does occur
  cost_s12 = 27047,
  cost_s13 = 19575,
  cost_s14 = 19575,
  probability_s01 = 0.29, #if recovery does not occur
  probability_s02 = 0.07,
  probability_s03 = 0.41,
  probability_s04 = 0.23,
  probability_s11 = 0.42, # recovery does occur
  probability_s12 = 0.24,
  probability_s13 = 0.20,
  probability_s14 = 0.14,
  
  deltaUnitUtilityDirection = NA, # "increase" # Q Is an increase in the primary outcome expected to be associated with an increase or decrease in health state utility?
  deltaUnitUtilitySize = NA, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease the health state utility?
  treatmentDurationMonths = NA, # Q how long is the treatment effect expected to last
  deltaUnitCostsDirection = NA, # "increase" # Q Is an increase in the primary outcome expected to be associated with disease related cost increases or decreases?
  deltaUnitCostsSize = NA, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease disease related costs?
  treatmentCostsMonthly_t1 = NA, # used in survival too
  treatmentCostsMonthly_t2 = NA,# used in survival too
  treatmentCostsMonthly_t3 = NA,# used in survival too
  treatmentCostsMonthly_t4 = NA,# used in survival too
  utilityPreTransition = NA, # Q What is the health utility associated with the pre-progression health state?
  monthlyCostPreTransition = NA, # Q What are the expected monthly disease related costs associated with the pre-transition health state?
  treatUntilProgression_t1 = NA, # "No"  Q Are individuals always treated until progression under the baseline treatment?
  maxDurationOfTreatmentMonths_t1 = NA, # Q what is the maximum number of months that the baseline treatment will be given?
  treatUntilProgression_t2 = NA, # "No" "Yes"
  maxDurationOfTreatmentMonths_t2 = NA,
  treatUntilProgression_t3 = NA, # "No" "Yes"
  maxDurationOfTreatmentMonths_t3 = NA,
  treatUntilProgression_t4 = NA, # "No" "Yes"
  maxDurationOfTreatmentMonths_t4 = NA,
  
  typeOfResearch = "RCT", # "RCT",  # "feasibility",
  
  durationOfResearch = 5,
  costResearchFunder = 2854000,
  utilisation_t1 = 100,
  utilisation_t2 = 0,
  
  utilisation_t3 = NA,
  utilisation_t4 = NA,
  costHealthSystem = 490000,
  
  durationOfResearchDefinitive = NA,
  durationOfResearchFeas = NA,
  costResearchFunderFeas = NA,
  costResearchFunderDefinitive =  NA,
  probabilityOfDefinitiveResearch = NA,
  costHealthSystemFeas = NA,
  costHealthSystemDefinitive = NA
)
