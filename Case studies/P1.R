## P1
####

source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/SupplementaryFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/SupplementaryFunctionsFeas.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/master.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/ReconFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/EpiInputFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/PlottingFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/EpiCalcFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/NBCalcFunctions.R", local = TRUE)


# primary outcome harm => ci 0.19 to 4.39

# Natural outcome
####################################
master(
  typeOfEndpoint = "binary", # 'binary', "continuous''survival"
  baselineInput = "eventsNonEvents", # "eventsNonEvents" "range"
  MCsims = 900000,
  
  baselineRange = NA ,
  
  nEvents = 29,
  nAtRisk = 61,
  numberOfTreatments = 3,
  binaryRelativeScale_t2 = "OR",
  binaryRelativeScale_t3 = "OR",
  
  binaryRelativeScale_t4 = NA,
  
  binaryDist_t2 = "norm",
  binaryDist_t3 ="norm" ,
  
  binaryDist_t4 = NA,
  
  OR_t2= c(0.19 , 4.39),
  OR_t3 = c(0.19 , 4.39),
  
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
  
  typeOfOutcome= "harm", # "benefit" or "harm" "netHealth"
  
  tCostsDependOnEvent= "No",
  
  MCD_t2 = 0.0,
  MCD_t3 = 0.0,
  
  MCD_t4 = NA,
  cost_t1 = NA ,
  cost_t2= NA,
  cost_t3 = NA,
  cost_t4 = NA,
  
  k = NA,
  currencySymbol = "£",
  
  incidence = 1563 ,
  discountRate = 3.5 , # NB - this is how ui provides the discount rate!
  timeInformation = 15,
  
  nameOf_t1 = "APs",
  nameOf_t2 = "PI",
  nameOf_t3 = "PI + AP",
  
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
  utility_s01 = NA, # if relapse does not occur
  utility_s02 = NA,
  utility_s03 = NA,
  utility_s04 = NA,
  utility_s11 = NA, # relapse does occur
  utility_s12 = NA,
  utility_s13 = NA,
  utility_s14 = NA,
  lifeDuration_s01 = NA, #if relapse does not occur
  lifeDuration_s02 = NA,
  lifeDuration_s03 = NA,
  lifeDuration_s04 = NA,
  lifeDuration_s11 = NA, # relapse does occur
  lifeDuration_s12 = NA,
  lifeDuration_s13 = NA,
  lifeDuration_s14 = NA,
  cost_s01 = NA,   #if relapse does not occur
  cost_s02 = NA,
  cost_s03 = NA,
  cost_s04 = NA,
  cost_s11 = NA, # relapse does occur
  cost_s12 = NA,
  cost_s13 = NA,
  cost_s14 = NA,
  probability_s01 = NA, #if relapse does not occur
  probability_s02 = NA,
  probability_s03 = NA,
  probability_s04 = NA,
  probability_s11 = NA, # relapse does occur
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
  
  typeOfResearch = "feasibility", # "RCT",  # "feasibility",
  
  durationOfResearch = NA,
  costResearchFunder = NA,
  
  utilisation_t1 = 100,
  utilisation_t2 = 0,
  utilisation_t3 = 0,
  
  utilisation_t4 = NA,
  costHealthSystem = NA,
  
  durationOfResearchDefinitive = 6,
  durationOfResearchFeas = 2,
  costResearchFunderFeas = 601481,
  costResearchFunderDefinitive =  2500000,
  probabilityOfDefinitiveResearch = 0.5,
  costHealthSystemFeas = 150000,
  costHealthSystemDefinitive = 490000
)








# costs and QALYs
####################################
master(
  typeOfEndpoint = "binary", # 'binary', "continuous''survival"
  
  baselineInput = "eventsNonEvents", # "eventsNonEvents" "range"
  
  MCsims = 900000,
  
  baselineRange = NA ,
  
  nEvents = 29,
  nAtRisk = 61,
  numberOfTreatments = 3,
  binaryRelativeScale_t2 = "OR",
  binaryRelativeScale_t3 = "OR",
  binaryRelativeScale_t4 = "OR",
  binaryDist_t2 = "norm",
  binaryDist_t3 ="norm" ,
  
  binaryDist_t4 = "alwaysPositive",
  
  OR_t2= c(0.19 , 4.39),
  OR_t3 = c(0.19 , 4.39),
  
  OR_t4= c(0.71, 1.18),
  RR_t2 =c(0.9, 1.1),
  RR_t3 =c(0.9, 1.1),
  RR_t4 =c(0.9, 1.1),
  RD_t2 =c(-5, 5),
  RD_t3=c(-5, 5),
  RD_t4=c(-5, 5),
  ORHalfNorm_t2=2,
  ORHalfNorm_t3=2,
  ORHalfNorm_t4=2,
  RRHalfNorm_t2=2,
  RRHalfNorm_t3=2,
  RRHalfNorm_t4=2,
  RDHalfNorm_t2=2,
  RDHalfNorm_t3=2,
  RDHalfNorm_t4=2,
  continuousInput_t2="meanAndSE", # "range", # "meanAndSE",
  continuousInput_t3="range" ,# "meanAndSE",
  continuousInput_t4="range", # "meanAndSE",
  continMean_t2=1,
  continMean_t3=1,
  continMean_t4=1,
  continSE_t2=.5,
  continSE_t3=.5,
  continSE_t4=.5,
  continDist_t2="alwaysPositive", #  "alwaysNegative"  "norm",
  continDist_t3="alwaysPositive", #  "alwaysNegative"  "norm",
  continDist_t4="alwaysPositive", #  "alwaysNegative"  "norm",
  MD_t2=c(-2, 2),
  MD_t3=c(-2, 2),
  MD_t4=c(-2, 2),
  MDHalfNorm_t2=2,
  MDHalfNorm_t3=2,
  MDHalfNorm_t4=2,
  survivalType = "exponential", # "exponential", # "weibull",
  survivalDist_t2="norm" ,#  "alwaysNegative"  "norm",,"alwaysPositive" 
  survivalDist_t3="norm", #  "alwaysNegative"  "norm",,"alwaysPositive" 
  survivalDist_t4="norm" ,#  "alwaysNegative"  "norm",,
  lambda_t1= 0.0277 ,
  gamma_t1=1,
  HR_t2=c(0.71, 1.18),
  HR_t3=c(0.71, 1.18),
  HR_t4=c(0.71, 1.18),
  HRHalfNorm_t2=1.4,
  HRHalfNorm_t3=1.4,
  HRHalfNorm_t4=1.4,
  
  typeOfOutcome= "netHealth", # "benefit" or "harm" "netHealth"
  
  tCostsDependOnEvent= "No",
  MCD_t2 = 0.0,
  MCD_t3 = 0.0,
  MCD_t4 = 0.3,
  
  cost_t1 = 687 ,
  cost_t2= 2910,
  cost_t3 = 3597,
  
  cost_t4 = NA,
  
  k = 15000,
  currencySymbol = "£",
  
  incidence = 1563 ,
  discountRate = 3.5 , # NB - this is how ui provides the discount rate!
  timeInformation = 15,
  
  nameOf_t1 = "APs",
  nameOf_t2 = "PI",
  nameOf_t3 = "PI + AP",
  
  nameOf_t4 = NA,
  costEvent_t1 = NA,
  costEvent_t2 = NA,
  costEvent_t3 = NA,
  costEvent_t4 = NA,
  costNotEvent_t1 = NA,
  costNotEvent_t2 = NA,
  costNotEvent_t3 = NA,
  costNotEvent_t4 = NA,
  
  numberS0States = 1, # if event does not occur
  numberS1States = 1, # if event occurs
  utility_s01 = 0.94, # if relapse does not occur
  
  utility_s02 = NA,
  utility_s03 = NA,
  utility_s04 = NA,
  
  utility_s11 = 0.805, # relapse does occur
  utility_s12 = NA,
  utility_s13 = NA,
  utility_s14 = NA,
  
  lifeDuration_s01 = 1, #if relapse does not occur
  lifeDuration_s02 = NA,
  lifeDuration_s03 = NA,
  lifeDuration_s04 = NA,
  lifeDuration_s11 = 1, # relapse does occur
  lifeDuration_s12 = NA,
  lifeDuration_s13 = NA,
  lifeDuration_s14 = NA,
  cost_s01 = 5401,   #if relapse does not occur
  cost_s02 = NA,
  cost_s03 = NA,
  cost_s04 = NA,
  cost_s11 = 19210, # relapse does occur
  cost_s12 = NA,
  cost_s13 = NA,
  cost_s14 = NA,
  probability_s01 = 1, #if relapse does not occur
  probability_s02 = NA,
  probability_s03 = NA,
  probability_s04 = NA,
  probability_s11 = 1, # relapse does occur
  probability_s12 = NA,
  probability_s13 = NA,
  probability_s14 = NA,
  
  deltaUnitUtilityDirection = "decrease", # "increase" # Q Is an increase in the primary outcome expected to be associated with an increase or decrease in health state utility?
  deltaUnitUtilitySize = 0.1, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease the health state utility?
  treatmentDurationMonths = 12, # Q how long is the treatment effect expected to last
  deltaUnitCostsDirection = "decrease", # "increase" # Q Is an increase in the primary outcome expected to be associated with disease related cost increases or decreases?
  deltaUnitCostsSize = 200, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease disease related costs?
  treatmentCostsMonthly_t1 = 6042, # used in survival too
  treatmentCostsMonthly_t2 = 6042,# used in survival too
  treatmentCostsMonthly_t3 = 6042,# used in survival too
  treatmentCostsMonthly_t4 = 400,# used in survival too
  utilityPreTransition = 0.79, # Q What is the health utility associated with the pre-progression health state?
  monthlyCostPreTransition = 100, # Q What are the expected monthly disease related costs associated with the pre-transition health state?
  treatUntilProgression_t1 = "Yes", # "No"  Q Are individuals always treated until progression under the baseline treatment?
  maxDurationOfTreatmentMonths_t1 = NA, # Q what is the maximum number of months that the baseline treatment will be given?
  treatUntilProgression_t2 = "No", # "No" "Yes"
  maxDurationOfTreatmentMonths_t2 = 12,
  treatUntilProgression_t3 = "No", # "No" "Yes"
  maxDurationOfTreatmentMonths_t3 = 6,
  treatUntilProgression_t4 = "No", # "No" "Yes"
  maxDurationOfTreatmentMonths_t4 = 24,
  
  typeOfResearch = "feasibility", # "RCT",  # "feasibility",
  
  durationOfResearch = 6,
  costResearchFunder = 2522710,
  
  utilisation_t1 = 100,
  utilisation_t2 = 0,
  utilisation_t3 = 0,
  utilisation_t4 = 0,
  costHealthSystem = 62410967,
  
  durationOfResearchDefinitive = 6,
  durationOfResearchFeas = 2,
  costResearchFunderFeas = 601481,
  costResearchFunderDefinitive =  2500000,
  probabilityOfDefinitiveResearch = 0.5,
  costHealthSystemFeas = 150000,
  costHealthSystemDefinitive = 490000
)

