## P4
####

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
  baselineInput = "range", # "eventsNonEvents" "range"
  MCsims = 900000,
  
  baselineRange = c(0.3, 0.3) ,
  
  nEvents = 29,
  nAtRisk = 61,
  
  numberOfTreatments = 2,
  binaryRelativeScale_t2 = "OR",
  
  binaryRelativeScale_t3 = "OR",
  binaryRelativeScale_t4 = "OR",
  binaryDist_t2 = "norm",
  binaryDist_t3 ="norm" ,
  binaryDist_t4 = "alwaysPositive",
  
  OR_t2= c(1, 6.46),
  
  OR_t3 = c(0.13, 6.46),
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
  
  typeOfOutcome= "benefit", # "benefit" or "harm" "netHealth"
  
  tCostsDependOnEvent= "No",
  MCD_t2 = 0.0,
  
  MCD_t3 = 0.0,
  MCD_t4 = 0.3,
  
  cost_t1 = 687 ,
  cost_t2= 2910,
  cost_t3 = 3597,
  cost_t4 = 60,
  
  k = 15000,
  currencySymbol = "£",
  
  incidence = 355000 ,
  discountRate = 3.5 , # NB - this is how ui provides the discount rate!
  timeInformation = 15,
  
  nameOf_t1 = "Current practice",
  nameOf_t2 = "Information booklet",
  
  nameOf_t3 = "PI + AP",
  nameOf_t4 = "4",
  costEvent_t1 = 10,
  costEvent_t2 = 20,
  costEvent_t3 = 3,
  costEvent_t4 = 40,
  costNotEvent_t1 = 50,
  costNotEvent_t2 = 60,
  costNotEvent_t3 = 70,
  costNotEvent_t4 = 80,
  
  numberS0States = 1, # if event does not occur
  numberS1States = 2 , # if event occurs
  utility_s01 = 0.799, # if death at home does not occur
  
  utility_s02 = NA,
  utility_s03 = NA,
  utility_s04 = NA,
  
  utility_s11 = 0.799, # relapse does occur
  utility_s12 = 0.67,
  utility_s13 = NA,
  utility_s14 = NA,
  
  lifeDuration_s01 = 1, #if relapse does not occur
  lifeDuration_s02 = NA,
  lifeDuration_s03 = NA,
  lifeDuration_s04 = NA,
  lifeDuration_s11 = 0.5, # relapse does occur
  lifeDuration_s12 = 0.5,
  lifeDuration_s13 = NA,
  lifeDuration_s14 = NA,
  cost_s01 = 12726,   #if relapse does not occur
  cost_s02 = NA,
  cost_s03 = NA,
  cost_s04 = NA,
  cost_s11 = 12726, # relapse does occur
  cost_s12 = 33018,
  cost_s13 = NA,
  cost_s14 = NA,
  probability_s01 = 1, #if relapse does not occur
  probability_s02 = NA,
  probability_s03 = NA,
  probability_s04 = NA,
  probability_s11 = 1, # relapse does occur
  probability_s12 = 1,
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
  
  typeOfResearch = "RCT", # "RCT",  # "feasibility",
  
  durationOfResearch = 6,
  costResearchFunder = 882177,
  
  utilisation_t1 = 100,
  utilisation_t2 = 0,
  
  utilisation_t3 = 0,
  utilisation_t4 = 0,
  costHealthSystem = 4104,
  
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
  baselineInput = "range", # "eventsNonEvents" "range"
  MCsims = 900000,
  
  baselineRange = c(0.3, 0.3) ,
  
  nEvents = 29,
  nAtRisk = 61,
  
  numberOfTreatments = 2,
  binaryRelativeScale_t2 = "OR",
  
  binaryRelativeScale_t3 = "OR",
  binaryRelativeScale_t4 = "OR",
  binaryDist_t2 = "norm",
  binaryDist_t3 ="norm" ,
  binaryDist_t4 = "alwaysPositive",
  
  OR_t2= c(1, 6.46),
  
  OR_t3 = c(0.13, 6.46),
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
  
  cost_t1 = 0 ,
  cost_t2= 9.06,
  
  cost_t3 = 3597,
  cost_t4 = 60,
  
  k = 15000,
  currencySymbol = "£",
  
  incidence = 355000 ,
  discountRate = 3.5 , # NB - this is how ui provides the discount rate!
  timeInformation = 15,
  
  nameOf_t1 = "Current practice",
  nameOf_t2 = "Information booklet",
  
  nameOf_t3 = "PI + AP",
  nameOf_t4 = "4",
  costEvent_t1 = 10,
  costEvent_t2 = 20,
  costEvent_t3 = 3,
  costEvent_t4 = 40,
  costNotEvent_t1 = 50,
  costNotEvent_t2 = 60,
  costNotEvent_t3 = 70,
  costNotEvent_t4 = 80,
  
  numberS0States = 1, # if death at home does not occur - in hospital
  numberS1States = 1 , # if death at home occurs
  utility_s01 = 0, # if death at home does not occur
  
  utility_s02 = NA,
  utility_s03 = NA,
  utility_s04 = NA,
  
  utility_s11 = 0, # death at home does occur
  utility_s12 = NA,
  utility_s13 = NA,
  utility_s14 = NA,
  
  lifeDuration_s01 = 0, #if death at home does not occur
  lifeDuration_s02 = NA,
  lifeDuration_s03 = NA,
  lifeDuration_s04 = NA,
  lifeDuration_s11 = 0, # death at home does occur
  lifeDuration_s12 = NA,
  lifeDuration_s13 = NA,
  lifeDuration_s14 = NA,
  cost_s01 = 3000,   #if death at home does not occur
  cost_s02 = NA,
  cost_s03 = NA,
  cost_s04 = NA,
  cost_s11 = 2108, # death at home does occur
  cost_s12 = NA,
  cost_s13 = NA,
  cost_s14 = NA,
  probability_s01 = 1, #if death at home does not occur
  probability_s02 = NA,
  probability_s03 = NA,
  probability_s04 = NA,
  probability_s11 = 1, # death at home does occur
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
  
  typeOfResearch = "RCT", # "RCT",  # "feasibility",
  
  durationOfResearch = 6,
  costResearchFunder = 882177,
  
  utilisation_t1 = 100,
  utilisation_t2 = 0,
  
  utilisation_t3 = 0,
  utilisation_t4 = 0,
  costHealthSystem = 4104,
  
  durationOfResearchDefinitive = 6,
  durationOfResearchFeas = 2,
  costResearchFunderFeas = 601481,
  costResearchFunderDefinitive =  2500000,
  probabilityOfDefinitiveResearch = 0.5,
  costHealthSystemFeas = 150000,
  costHealthSystemDefinitive = 490000
)

