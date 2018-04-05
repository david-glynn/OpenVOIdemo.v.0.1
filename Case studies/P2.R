####
# P2


source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/SupplementaryFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/SupplementaryFunctionsFeas.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/master.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/ReconFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/EpiInputFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/PlottingFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/EpiCalcFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/NBCalcFunctions.R", local = TRUE)



# From this we can assign a distribution of continuous effect on the natural MMSE scale 
# which reflects a 0.5% probability of treatment success i.e. 
# there will be a 0.5% chance of a reduction in decline of 1.4 points or more. 
# By fitting a distribution to this probability of success the uncertain effect 
# of each treatment on decline can be described by a 
# normal distribution with mean zero and standard error of 0.54. 
# This means that on average we expect each treatment not to have any effect on 
# decline but there is a chance that each treatment could either slow down or speed up decline. 
# Small reductions in decline are more likely than large reductions and there is 
# only a 0.5% chance of reduction in decline larger than 1.4 points. 
# We use this estimate for each treatment. 
# This assumes that the effects of each monotherapy and their combination are independent of each other i.e., it is possible for each monotherapy to have positive effects but for the combination to have negative effects. Further research is required to use the empirical evidence base to better inform this judgement.


# **NB just mean difference!
# LCI and UCI 
0 - 1.96*0.54
0 + 1.96*0.54


# natural outcome
########################

master(
  typeOfEndpoint = "continuous", # 'binary', "continuous''survival"
  
  baselineInput = "range",
  
  MCsims = 900000,
  
  baselineRange = c(0.525, 0.525) ,
  nEvents = 10,
  nAtRisk = 20,
  
  numberOfTreatments = 4,
  
  binaryRelativeScale_t2 = "OR",
  binaryRelativeScale_t3 = "OR",
  binaryRelativeScale_t4 = "OR",
  binaryDist_t2 = "norm",
  binaryDist_t3 ="norm" ,
  binaryDist_t4 = "alwaysPositive",
  OR_t2= NA,
  OR_t3 = NA,
  OR_t4= NA,
  RR_t2 =NA,
  RR_t3 =NA,
  RR_t4 =NA,
  RD_t2 =NA,
  RD_t3=NA,
  RD_t4=NA,
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
  continuousInput_t3="meanAndSE" ,# "meanAndSE",
  continuousInput_t4="meanAndSE", # "meanAndSE",
  continMean_t2=0,
  continMean_t3=0,
  continMean_t4=0,
  continSE_t2= 0.54,
  continSE_t3= 0.54,
  continSE_t4= 0.54,
  
  continDist_t2="norm", #  "alwaysNegative"  "norm",
  continDist_t3="norm", #  "alwaysNegative"  "norm",
  continDist_t4="norm", #  "alwaysNegative"  "norm",
  MD_t2=c(-2, 2),
  MD_t3=c(-2, 2),
  MD_t4=c(-2, 2),
  MDHalfNorm_t2=2,
  MDHalfNorm_t3=2,
  MDHalfNorm_t4=2,
  survivalType = "weibull", # "exponential", # "weibull",
  survivalDist_t2="alwaysPositive" ,#  "alwaysNegative"  "norm",,
  survivalDist_t3="alwaysPositive", #  "alwaysNegative"  "norm",,
  survivalDist_t4="alwaysPositive" ,#  "alwaysNegative"  "norm",,
  lambda_t1=10,
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
  MCD_t4 = 0.0,
  
  cost_t1 = 0,
  cost_t2= 1907,
  cost_t3 = 355,
  cost_t4 = 2262,
  k = 15000,
  
  currencySymbol = "£",
  incidence = 100000,
  discountRate = 3.5 , # NB - this is how ui provides the discount rate!
  timeInformation = 20,
  nameOf_t1 = "No treatment",
  nameOf_t2 = "Exenatide",
  nameOf_t3 = "Telmisartan",
  nameOf_t4 = "Combination",
  
  costEvent_t1 = 10,
  costEvent_t2 = 20,
  costEvent_t3 = 3,
  costEvent_t4 = 40,
  costNotEvent_t1 = 50,
  costNotEvent_t2 = 60,
  costNotEvent_t3 = 70,
  costNotEvent_t4 = 80,
  numberS0States = 4,
  numberS1States = 4 ,
  utility_s01 = 0,
  utility_s02 = 0.11,
  utility_s03 = 0.41,
  utility_s04 = 0.58,
  utility_s11 = 0.7,
  utility_s12 = 0.81,
  utility_s13 = 0.96,
  utility_s14 = 1,
  lifeDuration_s01 = 0,
  lifeDuration_s02 = 7.11,
  lifeDuration_s03 = 12.52,
  lifeDuration_s04 = 12.52,
  lifeDuration_s11 = 16.73,
  lifeDuration_s12 = 16.73,
  lifeDuration_s13 = 19.23,
  lifeDuration_s14 = 19.23,
  cost_s01 = 0,
  cost_s02 = 45450,
  cost_s03 = 154324,
  cost_s04 = 154324,
  cost_s11 = 27047,
  cost_s12 = 27047,
  cost_s13 = 19575,
  cost_s14 = 19575,
  probability_s01 = 0.29,
  probability_s02 = 0.07,
  probability_s03 = 0.41,
  probability_s04 = 0.23,
  probability_s11 = 0.42,
  probability_s12 = 0.24,
  probability_s13 = 0.2,
  probability_s14 = 0.14,
  
  deltaUnitUtilityDirection = "increase", # "increase" "decrease"   # Q Is an increase in the primary outcome expected to be associated with an increase or decrease in health state utility?
  deltaUnitUtilitySize = 0.01242, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease the health state utility?
  treatmentDurationMonths = 12, # Q how long is the treatment effect expected to last
  deltaUnitCostsDirection = "decrease", # "increase" # Q Is an increase in the primary outcome expected to be associated with disease related cost increases or decreases?
  deltaUnitCostsSize = 14, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease disease related costs?
  treatmentCostsMonthly_t1 = 0, # used in survival too
  treatmentCostsMonthly_t2 = 73.36,# used in survival too
  treatmentCostsMonthly_t3 = 14.83 ,# used in survival too
  treatmentCostsMonthly_t4 = 73.36 + 14.83,# used in survival too
  
  utilityPreTransition = 0.5, # Q What is the health utility associated with the pre-progression health state?
  monthlyCostPreTransition = 2000, # Q What are the expected monthly disease related costs associated with the pre-transition health state?
  treatUntilProgression_t1 = "Yes", # "No"  Q Are individuals always treated until progression under the baseline treatment?
  maxDurationOfTreatmentMonths_t1 = NA, # Q what is the maximum number of months that the baseline treatment will be given?
  treatUntilProgression_t2 = "No", # "No" "Yes"
  maxDurationOfTreatmentMonths_t2 = 12,
  treatUntilProgression_t3 = "No", # "No" "Yes"
  maxDurationOfTreatmentMonths_t3 = 24,
  treatUntilProgression_t4 = "No", # "No" "Yes"
  maxDurationOfTreatmentMonths_t4 = 24,
  
  typeOfResearch = "RCT", # "RCT",  # "feasibility",
  durationOfResearch = 6,
  costResearchFunder = 3310883,
  utilisation_t1 = 100,
  utilisation_t2 = 0,
  utilisation_t3 = 0,
  utilisation_t4 = 0,
  costHealthSystem = 1297789,
  
  durationOfResearchDefinitive = 6,
  durationOfResearchFeas = 2,
  costResearchFunderFeas = 601481,
  costResearchFunderDefinitive =  2522710,
  probabilityOfDefinitiveResearch = 0.5,
  costHealthSystemFeas = 150000,
  costHealthSystemDefinitive = 490000
)



# cost and QALY analysis - MCD is incorporated
###################################


# relationship between MMSE and utility
# p201 Bond 2012
# 
MMSE <- c(4.5,12,17.5,23,28)
Utility <- c(0.33, 0.49, 0.5, 0.49, 0.69)
data <- data.frame(MMSE = MMSE, Utility = Utility)

lm(Utility ~ MMSE, data = data)
# from this a unit change in MMSE translates to a 0.01242 increase in utility

master(
    typeOfEndpoint = "continuous", # 'binary', "continuous''survival"
    
    baselineInput = "range",
    
    MCsims = 900000,
    
    baselineRange = c(0.525, 0.525) ,
    nEvents = 10,
    nAtRisk = 20,
    
    numberOfTreatments = 4,
    
    binaryRelativeScale_t2 = "OR",
    binaryRelativeScale_t3 = "OR",
    binaryRelativeScale_t4 = "OR",
    binaryDist_t2 = "norm",
    binaryDist_t3 ="norm" ,
    binaryDist_t4 = "alwaysPositive",
    OR_t2= c(0.375, 2.66),
    OR_t3 = c(0.375, 2.66),
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
    continuousInput_t3="meanAndSE" ,# "meanAndSE",
    continuousInput_t4="meanAndSE", # "meanAndSE",
    continMean_t2=0,
    continMean_t3=0,
    continMean_t4=0,
    continSE_t2= 0.54,
    continSE_t3= 0.54,
    continSE_t4= 0.54,
    
    continDist_t2="norm", #  "alwaysNegative"  "norm",
    continDist_t3="norm", #  "alwaysNegative"  "norm",
    continDist_t4="norm", #  "alwaysNegative"  "norm",
    MD_t2=c(-2, 2),
    MD_t3=c(-2, 2),
    MD_t4=c(-2, 2),
    MDHalfNorm_t2=2,
    MDHalfNorm_t3=2,
    MDHalfNorm_t4=2,
    survivalType = "weibull", # "exponential", # "weibull",
    survivalDist_t2="alwaysPositive" ,#  "alwaysNegative"  "norm",,
    survivalDist_t3="alwaysPositive", #  "alwaysNegative"  "norm",,
    survivalDist_t4="alwaysPositive" ,#  "alwaysNegative"  "norm",,
    lambda_t1=10,
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
    MCD_t4 = 0.0,
    
    cost_t1 = 0,
    cost_t2= 1907,
    cost_t3 = 355,
    cost_t4 = 2262,
    
    k = 15000,
    currencySymbol = "£",
    incidence = 100000,
    discountRate = 3.5 , # NB - this is how ui provides the discount rate!
    timeInformation = 20,
    nameOf_t1 = "No treatment",
    nameOf_t2 = "Exenatide",
    nameOf_t3 = "Telmisartan",
    nameOf_t4 = "Combination",
    
    costEvent_t1 = 10,
    costEvent_t2 = 20,
    costEvent_t3 = 3,
    costEvent_t4 = 40,
    costNotEvent_t1 = 50,
    costNotEvent_t2 = 60,
    costNotEvent_t3 = 70,
    costNotEvent_t4 = 80,
    numberS0States = 4,
    numberS1States = 4 ,
    utility_s01 = 0,
    utility_s02 = 0.11,
    utility_s03 = 0.41,
    utility_s04 = 0.58,
    utility_s11 = 0.7,
    utility_s12 = 0.81,
    utility_s13 = 0.96,
    utility_s14 = 1,
    lifeDuration_s01 = 0,
    lifeDuration_s02 = 7.11,
    lifeDuration_s03 = 12.52,
    lifeDuration_s04 = 12.52,
    lifeDuration_s11 = 16.73,
    lifeDuration_s12 = 16.73,
    lifeDuration_s13 = 19.23,
    lifeDuration_s14 = 19.23,
    cost_s01 = 0,
    cost_s02 = 45450,
    cost_s03 = 154324,
    cost_s04 = 154324,
    cost_s11 = 27047,
    cost_s12 = 27047,
    cost_s13 = 19575,
    cost_s14 = 19575,
    probability_s01 = 0.29,
    probability_s02 = 0.07,
    probability_s03 = 0.41,
    probability_s04 = 0.23,
    probability_s11 = 0.42,
    probability_s12 = 0.24,
    probability_s13 = 0.2,
    probability_s14 = 0.14,
    
    deltaUnitUtilityDirection = "increase", # "increase" "decrease"   # Q Is an increase in the primary outcome expected to be associated with an increase or decrease in health state utility?
    deltaUnitUtilitySize = 0.01242, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease the health state utility?
    treatmentDurationMonths = 12, # Q how long is the treatment effect expected to last
    deltaUnitCostsDirection = "decrease", # "increase" # Q Is an increase in the primary outcome expected to be associated with disease related cost increases or decreases?
    deltaUnitCostsSize = 14, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease disease related costs?
    treatmentCostsMonthly_t1 = 0, # used in survival too
    treatmentCostsMonthly_t2 = 73.36,# used in survival too
    treatmentCostsMonthly_t3 = 14.83 ,# used in survival too
    treatmentCostsMonthly_t4 = 73.36 + 14.83,# used in survival too
    
    utilityPreTransition = 0.5, # Q What is the health utility associated with the pre-progression health state?
    monthlyCostPreTransition = 2000, # Q What are the expected monthly disease related costs associated with the pre-transition health state?
    treatUntilProgression_t1 = "Yes", # "No"  Q Are individuals always treated until progression under the baseline treatment?
    maxDurationOfTreatmentMonths_t1 = NA, # Q what is the maximum number of months that the baseline treatment will be given?
    treatUntilProgression_t2 = "No", # "No" "Yes"
    maxDurationOfTreatmentMonths_t2 = 12,
    treatUntilProgression_t3 = "No", # "No" "Yes"
    maxDurationOfTreatmentMonths_t3 = 24,
    treatUntilProgression_t4 = "No", # "No" "Yes"
    maxDurationOfTreatmentMonths_t4 = 24,

    typeOfResearch = "RCT", # "RCT",  # "feasibility",
    durationOfResearch = 6,
    costResearchFunder = 3310883,
    utilisation_t1 = 100,
    utilisation_t2 = 0,
    utilisation_t3 = 0,
    utilisation_t4 = 0,
    costHealthSystem = 1297789,
    
    durationOfResearchDefinitive = 6,
    durationOfResearchFeas = 2,
    costResearchFunderFeas = 601481,
    costResearchFunderDefinitive =  2522710,
    probabilityOfDefinitiveResearch = 0.5,
    costHealthSystemFeas = 150000,
    costHealthSystemDefinitive = 490000
)