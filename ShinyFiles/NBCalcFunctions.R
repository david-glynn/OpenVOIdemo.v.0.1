###
# NB calc functions 

# input: outcome_t matrix and convert to NB_t matrix
# output: NB_t (to be used in NBtoEVPIResults or NBtoEVPIResultsFeas)

# can be used in reconsideration of evidence function: the NB step!


# test data
# outcome_t <- matrix(rnorm(40, 1, 1), ncol = 4)
# MCsims = 10
# typeOfEndpoint= "binary"
# typeOfOutcome= "netHealth"
# tCostsDependOnEvent= "No"
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
# numberOfTreatments = 4
# costEvent_t1 = 10
# costEvent_t2 = 20
# costEvent_t3 = 30
# costEvent_t4 = 40
# costNotEvent_t1 = 50
# costNotEvent_t2 = 60
# costNotEvent_t3 = 70
# costNotEvent_t4 = 80
# INBContinEvent = 0.5
# INBSurvivalEndpoint = 0.5



outcomeToNB_t <- function(outcome_t, MCsims,
                          typeOfEndpoint, typeOfOutcome, tCostsDependOnEvent,
                          MCD_t2, MCD_t3, MCD_t4,
                          INBBinaryEvent,
                          cost_t1,cost_t2, cost_t3, cost_t4, k,
                          currencySymbol, 
                          incidence, discountRate, timeInformation,
                          nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                          numberOfTreatments,
                          costEvent_t1,costEvent_t2,costEvent_t3,costEvent_t4,
                          costNotEvent_t1,costNotEvent_t2,costNotEvent_t3,costNotEvent_t4,
                          INBContinEvent,
                          INBSurvivalEndpoint){

  ########################
  # BINARY
  #########################
  
  # binary natural outcome 
  #############################
  if(typeOfEndpoint == "binary" & typeOfOutcome != "netHealth"){
    
    INB_Event <- ifelse(typeOfOutcome== "benefit", 1, -1)
    NB_t  <- outcome_t*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    # each column now represents simulations of the NB of each treatment
    
    # return list
    NBOutput <- list(NB_t = NB_t, tableTreatmentCostsDF = NA)
    return(NBOutput)
  }
  
  # binary QALYs and costs do not depend on the outcome
  ##################################################
  if(typeOfEndpoint == "binary" & typeOfOutcome == "netHealth" & tCostsDependOnEvent == "No"){
    
    NB_t  <- outcome_t*INBBinaryEvent # multiply every element by INBBinaryEvent (1st step in converting to NB)
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    # subtract the costs from each column in the vector.
    addCost_t <- c(-cost_t1/k ,-cost_t2/k, -cost_t3/k, -cost_t4/k) 
    NB_t  <- NB_t  + rep(addCost_t, each = MCsims) # each column now represents simulations of the NB of each treatment
    
    # generate and format costs table (assuming treatment costs do NOT depend on event)
    cost_t <- c(cost_t1, cost_t2, cost_t3, cost_t4)
    Cost_per_individual <- paste0(currencySymbol,formatC(cost_t, big.mark = ',', format = 'd'))
    Yearly_costs <- paste0(currencySymbol,formatC(cost_t*incidence, big.mark = ',', format = 'd'))
    Additional_cost_per_year <- paste0(currencySymbol,formatC((cost_t - cost_t1)*incidence, big.mark = ',', format = 'd'))
    popTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
    Total_Costs <- paste0(currencySymbol,formatC(cost_t*popTotal, big.mark = ',', format = 'd'))
    Treatment_name <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)
    tableTreatmentCostsDF <- as.data.frame(cbind(Treatment_name, Cost_per_individual, Yearly_costs, Additional_cost_per_year, Total_Costs))
    tableTreatmentCostsDF <- tableTreatmentCostsDF[1:numberOfTreatments,]

    # return list
    NBOutput <- list(NB_t = NB_t, tableTreatmentCostsDF = tableTreatmentCostsDF)
    return(NBOutput)
  }
  
  # binary QALYs and costs DO depend on the outcome
  ##################################################
  if(typeOfEndpoint == "binary" & typeOfOutcome == "netHealth" & tCostsDependOnEvent == "Yes"){
    
    costEvent_t <- c(costEvent_t1,costEvent_t2,costEvent_t3,costEvent_t4)
    costNotEvent_t <- c(costNotEvent_t1,costNotEvent_t2,costNotEvent_t3,costNotEvent_t4)
    # for each row of outcome_t calculate the QALY benefit of the event occuring minus the costs associated with the event/not event
    outputNB <- apply(outcome_t, 1, function(x)
      x*INBBinaryEvent + (x*-costEvent_t/k) + ((1-x)*-costNotEvent_t/k))
    NB_t <- t(outputNB) # need to transpose output to get in correct NB_t matrix form
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the matrix
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    
    # generate and format costs table (assuming treatment costs DO depend on event)
    EP_t <- apply(outcome_t, 2, mean) # expected probability of outcomes
    expectedCost_t <- EP_t*costEvent_t + (1 - EP_t)*costNotEvent_t # expected cost per person for each treatment
    Cost_per_individual <- paste0(currencySymbol,formatC(expectedCost_t, big.mark = ',', format = 'd'))
    Yearly_costs <- paste0(currencySymbol,formatC(expectedCost_t*incidence, big.mark = ',', format = 'd'))
    Additional_cost_per_year <- paste0(currencySymbol,formatC((expectedCost_t - expectedCost_t[1])*incidence, big.mark = ',', format = 'd'))
    popTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
    Total_Costs <- paste0(currencySymbol,formatC(expectedCost_t*popTotal, big.mark = ',', format = 'd'))
    Treatment_name <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)
    tableTreatmentCostsDF <- as.data.frame(cbind(Treatment_name, Cost_per_individual, Yearly_costs, Additional_cost_per_year, Total_Costs))
    tableTreatmentCostsDF <- tableTreatmentCostsDF[1:numberOfTreatments,]
    
    # return list
    NBOutput <- list(NB_t = NB_t, tableTreatmentCostsDF = tableTreatmentCostsDF)
    return(NBOutput)
  }

  
  
  ########################
  # CONTINUOUS
  #########################
  
  # continuous natural outcome 
  #############################
  if(typeOfEndpoint == "continuous" & typeOfOutcome != "netHealth"){
    
    INB_Event <- ifelse(typeOfOutcome== "benefit", 1, -1)
    NB_t  <- outcome_t*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    
    # return list
    NBOutput <- list(NB_t = NB_t, tableTreatmentCostsDF = NA)
    return(NBOutput)
  }
  
  
  # continuous QALY outcome 
  ###########################
  if(typeOfEndpoint == "continuous" & typeOfOutcome == "netHealth"){
    
    NB_t  <- outcome_t*INBContinEvent # multiply every element by net benefit (1st step in converting to NB)
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    addCost_t <- c(-cost_t1/k ,-cost_t2/k, -cost_t3/k, -cost_t4/k) # subtract the health effects of costs
    NB_t  <- NB_t  + rep(addCost_t, each = MCsims)
    
    # each column now represents simulations of the NB of each treatment
    # generate and format costs table (assuming treatment costs do not depend on continuous event)
    cost_t <- c(cost_t1, cost_t2, cost_t3, cost_t4)
    Cost_per_individual <- paste0(currencySymbol,formatC(cost_t, big.mark = ',', format = 'd'))
    Yearly_costs <- paste0(currencySymbol,formatC(cost_t*incidence, big.mark = ',', format = 'd'))
    Additional_cost_per_year <- paste0(currencySymbol,formatC((cost_t - cost_t1)*incidence, big.mark = ',', format = 'd'))
    popTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
    Total_Costs <- paste0(currencySymbol,formatC(cost_t*popTotal, big.mark = ',', format = 'd'))
    Treatment_name <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)
    tableTreatmentCostsDF <- as.data.frame(cbind(Treatment_name, Cost_per_individual, Yearly_costs, Additional_cost_per_year, Total_Costs))
    tableTreatmentCostsDF <- tableTreatmentCostsDF[1:numberOfTreatments,]
    
    # return list
    NBOutput <- list(NB_t = NB_t, tableTreatmentCostsDF = tableTreatmentCostsDF)
    return(NBOutput)
  }
  
  
  ########################
  # SURVIVAL
  #########################
  
  # survival natural outcome 
  #############################
  if(typeOfEndpoint == "survival" & typeOfOutcome != "netHealth"){
    
    INB_Event <- ifelse(typeOfOutcome== "benefit", 1, -1)
    NB_t  <- outcome_t*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    
    # return list
    NBOutput <- list(NB_t = NB_t, tableTreatmentCostsDF = NA)
    return(NBOutput)
  }
  
  # survival QALY outcome 
  ###########################
  if(typeOfEndpoint == "survival" & typeOfOutcome == "netHealth"){
    
    NB_t  <- outcome_t*INBSurvivalEndpoint # multiply every element by INBSurvivalEndpoint (1st step in converting to NB)
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    # subtract the costs from each column in the vector.
    addCost_t <- c(-cost_t1/k ,-cost_t2/k, -cost_t3/k, -cost_t4/k) 
    NB_t  <- NB_t  + rep(addCost_t, each = MCsims)
    
    # generate and format costs table (assuming treatment costs do not depend on duration of survival)
    cost_t <- c(cost_t1, cost_t2, cost_t3, cost_t4)
    Cost_per_individual <- paste0(currencySymbol,formatC(cost_t, big.mark = ',', format = 'd'))
    Yearly_costs <- paste0(currencySymbol,formatC(cost_t*incidence, big.mark = ',', format = 'd'))
    Additional_cost_per_year <- paste0(currencySymbol,formatC((cost_t - cost_t1)*incidence, big.mark = ',', format = 'd'))
    popTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
    Total_Costs <- paste0(currencySymbol,formatC(cost_t*popTotal, big.mark = ',', format = 'd'))
    Treatment_name <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)
    tableTreatmentCostsDF <- as.data.frame(cbind(Treatment_name, Cost_per_individual, Yearly_costs, Additional_cost_per_year, Total_Costs))
    tableTreatmentCostsDF <- tableTreatmentCostsDF[1:numberOfTreatments,]
    
    # return list
    NBOutput <- list(NB_t = NB_t, tableTreatmentCostsDF = tableTreatmentCostsDF)
    return(NBOutput)
    
  }
  
}



# test function
# outcomeToNB_t(outcome_t <- matrix(rnorm(40, 1, 1), ncol = 4),
#               MCsims = 10,
#               typeOfEndpoint= "survival",
#               typeOfOutcome= "netHealth",
#               tCostsDependOnEvent= "Yes",
#               MCD_t2 = 0.1,
#               MCD_t3 = 0.2,
#               MCD_t4 = 0.3,
#               INBBinaryEvent = 2,
#               cost_t1 = 20,
#               cost_t2= 30,
#               cost_t3 = 40,
#               cost_t4 = 60,
#               k = 13000,
#               currencySymbol = "£",
#               incidence = 100,
#               discountRate = 0.03 ,
#               timeInformation = 15,
#               nameOf_t1 = "1",
#               nameOf_t2 = "2",
#               nameOf_t3 = "3",
#               nameOf_t4 = "4",
#               numberOfTreatments = 4,
#               costEvent_t1 = 10,
#               costEvent_t2 = 20,
#               costEvent_t3 = 30,
#               costEvent_t4 = 40,
#               costNotEvent_t1 = 50,
#               costNotEvent_t2 = 60,
#               costNotEvent_t3 = 70,
#               costNotEvent_t4 = 80,
#               INBContinEvent = 0.5,
#               INBSurvivalEndpoint = 0.5)
































################################# new stuff ##################################


# # remove: 
# INBBinaryEvent
# INBContinEvent,
# INBSurvivalEndpoin


# test data
# outcome_t <- matrix(rnorm(40, 1, 1), ncol = 4)
# MCsims = 10
# typeOfEndpoint= "continuous"
# typeOfOutcome= "netHealth"
# tCostsDependOnEvent= "No"
# MCD_t2 = 0.1
# MCD_t3 = 0.2
# MCD_t4 = 0.3
# cost_t1 = 20
# cost_t2= 30
# cost_t3 = 40
# cost_t4 = 60
# k = 15000
# currencySymbol = "£"
# incidence = 100
# discountRate = 0.03
# timeInformation = 15
# nameOf_t1 = "1"
# nameOf_t2 = "2"
# nameOf_t3 = "3"
# nameOf_t4 = "4"
# numberOfTreatments = 4
# costEvent_t1 = 10
# costEvent_t2 = 20
# costEvent_t3 = 30
# costEvent_t4 = 40
# costNotEvent_t1 = 50
# costNotEvent_t2 = 60
# costNotEvent_t3 = 70
# costNotEvent_t4 = 80
# 
# # new inputs
# 
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
# # continuous NB
# deltaUnitUtilityDirection = "decrease" # "increase" # Q Is an increase in the primary outcome expected to be associated with an increase or decrease in health state utility?
# deltaUnitUtilitySize = 0.1 # Q By how much is a one unit increase in the primary outcome expected to increase/decrease the health state utility?
# treatmentDurationMonths = 12 # Q how long is the treatment effect expected to last
# deltaUnitCostsDirection = "decrease" # "increase" # Q Is an increase in the primary outcome expected to be associated with disease related cost increases or decreases?
# deltaUnitCostsSize = 200 # Q By how much is a one unit increase in the primary outcome expected to increase/decrease disease related costs?
# treatmentCostsMonthly_t1 = 100 # used in survival too
# treatmentCostsMonthly_t2 = 200# used in survival too
# treatmentCostsMonthly_t3 = 300# used in survival too
# treatmentCostsMonthly_t4 = 400# used in survival too
# # survival NB
# utilityPreTransition = 0.5 # Q What is the health utility associated with the pre-progression health state?
# monthlyCostPreTransition = 2000 # Q What are the expected monthly disease related costs associated with the pre-transition health state?
# treatUntilProgression_t1 = "Yes" # "No"  Q Are individuals always treated until progression under the baseline treatment?
# maxDurationOfTreatmentMonths_t1 = NA # Q what is the maximum number of months that the baseline treatment will be given?
# treatUntilProgression_t2 = "No" # "No" "Yes"
# maxDurationOfTreatmentMonths_t2 = 12
# treatUntilProgression_t3 = "No" # "No" "Yes"
# maxDurationOfTreatmentMonths_t3 = 24
# treatUntilProgression_t4 = "No" # "No" "Yes"
# maxDurationOfTreatmentMonths_t4 = 24


outcomeToNB2_t <- function(outcome_t, MCsims,
                          typeOfEndpoint, typeOfOutcome, tCostsDependOnEvent,
                          MCD_t2, MCD_t3, MCD_t4,
                          cost_t1,cost_t2, cost_t3, cost_t4, k,
                          currencySymbol, 
                          incidence, discountRate, timeInformation,
                          nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                          numberOfTreatments,
                          costEvent_t1,costEvent_t2,costEvent_t3,costEvent_t4,
                          costNotEvent_t1,costNotEvent_t2,costNotEvent_t3,costNotEvent_t4,
                          numberS0States = NA,numberS1States = NA,
                          utility_s01 = NA, utility_s02= NA, utility_s03= NA, utility_s04= NA,
                          utility_s11= NA, utility_s12= NA, utility_s13= NA, utility_s14= NA,
                          lifeDuration_s01 = NA, lifeDuration_s02= NA, lifeDuration_s03= NA, lifeDuration_s04= NA,
                          lifeDuration_s11= NA, lifeDuration_s12= NA, lifeDuration_s13= NA, lifeDuration_s14= NA,
                          cost_s01 = NA, cost_s02= NA, cost_s03= NA, cost_s04= NA,
                          cost_s11= NA, cost_s12= NA, cost_s13= NA, cost_s14= NA,
                          probability_s01 = NA, probability_s02= NA, probability_s03= NA, probability_s04= NA,
                          probability_s11= NA, probability_s12= NA, probability_s13= NA, probability_s14= NA,
                          deltaUnitUtilityDirection , deltaUnitUtilitySize , 
                          treatmentDurationMonths, deltaUnitCostsDirection, deltaUnitCostsSize,
                          treatmentCostsMonthly_t1, treatmentCostsMonthly_t2, treatmentCostsMonthly_t3, treatmentCostsMonthly_t4,
                          utilityPreTransition, monthlyCostPreTransition,
                          treatUntilProgression_t1 , maxDurationOfTreatmentMonths_t1, 
                          treatUntilProgression_t2 ,maxDurationOfTreatmentMonths_t2 ,
                          treatUntilProgression_t3 ,maxDurationOfTreatmentMonths_t3 ,
                          treatUntilProgression_t4 ,maxDurationOfTreatmentMonths_t4
                          
                          ){
  
  ########################
  # BINARY
  #########################
  
  # binary natural outcome 
  #############################
  if(typeOfEndpoint == "binary" & typeOfOutcome != "netHealth"){
    
    INB_Event <- ifelse(typeOfOutcome== "benefit", 1, -1)
    NB_t  <- outcome_t*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    # each column now represents simulations of the NB of each treatment
    
    # return list
    NBOutput <- list(NB_t = NB_t, tableTreatmentCostsDF = NA)
    return(NBOutput)
  }
  
  # binary QALYs and costs do not depend on the outcome
  ##################################################
  if(typeOfEndpoint == "binary" & typeOfOutcome == "netHealth" & tCostsDependOnEvent == "No"){
    
    # calculate INBBinaryEvent
    
    # utility of each state if the event occurs (s1)/ does not occur(s0)
    utility_s0 <- c(utility_s01, utility_s02, utility_s03, utility_s04)
    utility_s1 <- c(utility_s11, utility_s12, utility_s13, utility_s14)
    
    # life expectancy in each state
    lifeDuration_s0 <- c(lifeDuration_s01, lifeDuration_s02, lifeDuration_s03, lifeDuration_s04)
    lifeDuration_s1 <- c(lifeDuration_s11, lifeDuration_s12, lifeDuration_s13, lifeDuration_s14)
    
    # cost of each state if the event occurs (s1)/ does not occur(s0)
    cost_s0 <- c(cost_s01, cost_s02, cost_s03, cost_s04)
    cost_s1 <- c(cost_s11, cost_s12, cost_s13, cost_s14)
    
    # probability of each state if the event occurs (s1)/ does not occur(s0)
    probability_s0 <- c(probability_s01, probability_s02, probability_s03, probability_s04)
    probability_s1 <- c(probability_s11, probability_s12, probability_s13, probability_s14)
    
    # calculate expected benefit if the event occurs (s1)/ does not occur(s0) 
    ENB_s0 <- (utility_s0*lifeDuration_s0 - cost_s0/k)[1:numberS0States] %*% probability_s0[1:numberS0States]
    ENB_s1 <- (utility_s1*lifeDuration_s1 - cost_s1/k)[1:numberS1States] %*% probability_s1[1:numberS1States]
    
    INBBinaryEvent <- as.numeric(ENB_s1 - ENB_s0)
    
    NB_t  <- outcome_t*INBBinaryEvent # multiply every element by INBBinaryEvent (1st step in converting to NB)
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    # subtract the costs from each column in the vector.
    addCost_t <- c(-cost_t1/k ,-cost_t2/k, -cost_t3/k, -cost_t4/k) 
    NB_t  <- NB_t  + rep(addCost_t, each = MCsims) # each column now represents simulations of the NB of each treatment
    
    # generate and format costs table (assuming treatment costs do NOT depend on event)
    cost_t <- c(cost_t1, cost_t2, cost_t3, cost_t4)
    Cost_per_individual <- paste0(currencySymbol,formatC(cost_t, big.mark = ',', format = 'd'))
    Yearly_costs <- paste0(currencySymbol,formatC(cost_t*incidence, big.mark = ',', format = 'd'))
    Additional_cost_per_year <- paste0(currencySymbol,formatC((cost_t - cost_t1)*incidence, big.mark = ',', format = 'd'))
    popTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
    Total_Costs <- paste0(currencySymbol,formatC(cost_t*popTotal, big.mark = ',', format = 'd'))
    Treatment_name <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)
    tableTreatmentCostsDF <- as.data.frame(cbind(Treatment_name, Cost_per_individual, Yearly_costs, Additional_cost_per_year, Total_Costs))
    tableTreatmentCostsDF <- tableTreatmentCostsDF[1:numberOfTreatments,]
    
    # return list
    NBOutput <- list(NB_t = NB_t, tableTreatmentCostsDF = tableTreatmentCostsDF)
    return(NBOutput)
  }
  
  # binary QALYs and costs DO depend on the outcome
  ##################################################
  if(typeOfEndpoint == "binary" & typeOfOutcome == "netHealth" & tCostsDependOnEvent == "Yes"){
    
    # calculate INBBinaryEvent
    
    # utility of each state if the event occurs (s1)/ does not occur(s0)
    utility_s0 <- c(utility_s01, utility_s02, utility_s03, utility_s04)
    utility_s1 <- c(utility_s11, utility_s12, utility_s13, utility_s14)
    
    # life expectancy in each state
    lifeDuration_s0 <- c(lifeDuration_s01, lifeDuration_s02, lifeDuration_s03, lifeDuration_s04)
    lifeDuration_s1 <- c(lifeDuration_s11, lifeDuration_s12, lifeDuration_s13, lifeDuration_s14)
    
    # cost of each state if the event occurs (s1)/ does not occur(s0)
    cost_s0 <- c(cost_s01, cost_s02, cost_s03, cost_s04)
    cost_s1 <- c(cost_s11, cost_s12, cost_s13, cost_s14)
    
    # probability of each state if the event occurs (s1)/ does not occur(s0)
    probability_s0 <- c(probability_s01, probability_s02, probability_s03, probability_s04)
    probability_s1 <- c(probability_s11, probability_s12, probability_s13, probability_s14)
    
    # calculate expected benefit if the event occurs (s1)/ does not occur(s0) 
    ENB_s0 <- (utility_s0*lifeDuration_s0 - cost_s0/k)[1:numberS0States] %*% probability_s0[1:numberS0States]
    ENB_s1 <- (utility_s1*lifeDuration_s1 - cost_s1/k)[1:numberS1States] %*% probability_s1[1:numberS1States]
    
    INBBinaryEvent <- as.numeric(ENB_s1 - ENB_s0)
    
    
    costEvent_t <- c(costEvent_t1,costEvent_t2,costEvent_t3,costEvent_t4)
    costNotEvent_t <- c(costNotEvent_t1,costNotEvent_t2,costNotEvent_t3,costNotEvent_t4)
    # for each row of outcome_t calculate the QALY benefit of the event occuring minus the costs associated with the event/not event
    outputNB <- apply(outcome_t, 1, function(x)
      x*INBBinaryEvent + (x*-costEvent_t/k) + ((1-x)*-costNotEvent_t/k))
    NB_t <- t(outputNB) # need to transpose output to get in correct NB_t matrix form
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the matrix
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    
    # generate and format costs table (assuming treatment costs DO depend on event)
    EP_t <- apply(outcome_t, 2, mean) # expected probability of outcomes
    expectedCost_t <- EP_t*costEvent_t + (1 - EP_t)*costNotEvent_t # expected cost per person for each treatment
    Cost_per_individual <- paste0(currencySymbol,formatC(expectedCost_t, big.mark = ',', format = 'd'))
    Yearly_costs <- paste0(currencySymbol,formatC(expectedCost_t*incidence, big.mark = ',', format = 'd'))
    Additional_cost_per_year <- paste0(currencySymbol,formatC((expectedCost_t - expectedCost_t[1])*incidence, big.mark = ',', format = 'd'))
    popTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
    Total_Costs <- paste0(currencySymbol,formatC(expectedCost_t*popTotal, big.mark = ',', format = 'd'))
    Treatment_name <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)
    tableTreatmentCostsDF <- as.data.frame(cbind(Treatment_name, Cost_per_individual, Yearly_costs, Additional_cost_per_year, Total_Costs))
    tableTreatmentCostsDF <- tableTreatmentCostsDF[1:numberOfTreatments,]
    
    # return list
    NBOutput <- list(NB_t = NB_t, tableTreatmentCostsDF = tableTreatmentCostsDF)
    return(NBOutput)
  }
  
  
  
  ########################
  # CONTINUOUS
  #########################
  
  # continuous natural outcome 
  #############################
  if(typeOfEndpoint == "continuous" & typeOfOutcome != "netHealth"){
    
    INB_Event <- ifelse(typeOfOutcome== "benefit", 1, -1)
    NB_t  <- outcome_t*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    
    # return list
    NBOutput <- list(NB_t = NB_t, tableTreatmentCostsDF = NA)
    return(NBOutput)
  }
  
  
  # continuous QALY outcome 
  ###########################
  if(typeOfEndpoint == "continuous" & typeOfOutcome == "netHealth"){
    
    # calculate INBContinEvent
    ####

    deltaUnitCostsMonthly <- deltaUnitCostsSize*ifelse(deltaUnitCostsDirection == "increase", 1, -1)
    deltaUnitUtility <- deltaUnitUtilitySize*ifelse(deltaUnitUtilityDirection == "increase", 1, -1)

    INBContinEvent <- deltaUnitUtility*(treatmentDurationMonths/12) - (deltaUnitCostsMonthly*treatmentDurationMonths)/k
    
    NB_t  <- outcome_t*INBContinEvent # multiply every element by net benefit (1st step in converting to NB)
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    
    # costs depend on the duration of treatment effect - treat while it is effective
    addCost_t <- c(-(treatmentCostsMonthly_t1*treatmentDurationMonths)/k ,
                   -(treatmentCostsMonthly_t2*treatmentDurationMonths)/k ,
                   -(treatmentCostsMonthly_t3*treatmentDurationMonths)/k ,
                   -(treatmentCostsMonthly_t4*treatmentDurationMonths)/k ) # subtract the health effects of costs
    NB_t  <- NB_t  + rep(addCost_t, each = MCsims)
    
    # each column now represents simulations of the NB of each treatment
    # generate and format costs table (assuming treatment costs do not depend on continuous event)
    cost_t <- c(treatmentCostsMonthly_t1*treatmentDurationMonths, 
                treatmentCostsMonthly_t2*treatmentDurationMonths, 
                treatmentCostsMonthly_t3*treatmentDurationMonths, 
                treatmentCostsMonthly_t4*treatmentDurationMonths)
    # Cost_per_individual <- paste0(currencySymbol,formatC(cost_t, big.mark = ',', format = 'd'))
    # Yearly_costs <- paste0(currencySymbol,formatC(cost_t*incidence, big.mark = ',', format = 'd'))
    # Additional_cost_per_year <- paste0(currencySymbol,formatC((cost_t - cost_t[1])*incidence, big.mark = ',', format = 'd'))
    Cost_per_individual <- cost_t
    Yearly_costs <- cost_t*incidence
    Additional_cost_per_year <- (cost_t - cost_t[1])*incidence
    popTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
    # Total_Costs <- paste0(currencySymbol,formatC(cost_t*popTotal, big.mark = ',', format = 'd'))
    Total_Costs <- cost_t*popTotal
    
    Treatment_name <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)
    tableTreatmentCostsDF <- as.data.frame(cbind(Treatment_name, Cost_per_individual, Yearly_costs, Additional_cost_per_year, Total_Costs))
    tableTreatmentCostsDF <- tableTreatmentCostsDF[1:numberOfTreatments,]
    
    # return list
    NBOutput <- list(NB_t = NB_t, tableTreatmentCostsDF = tableTreatmentCostsDF)
    return(NBOutput)
  }
  
  
  ########################
  # SURVIVAL
  #########################
  
  # survival natural outcome 
  #############################
  if(typeOfEndpoint == "survival" & typeOfOutcome != "netHealth"){
    
    INB_Event <- ifelse(typeOfOutcome== "benefit", 1, -1)
    NB_t  <- outcome_t*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    
    # return list
    NBOutput <- list(NB_t = NB_t, tableTreatmentCostsDF = NA)
    return(NBOutput)
  }
  
  # survival QALY outcome 
  ###########################
  if(typeOfEndpoint == "survival" & typeOfOutcome == "netHealth"){
    
    # calculate INBSurvivalEndpoint (qaly gain per month of extra survival)
    
    INBSurvivalEndpoint <- utilityPreTransition/12 - monthlyCostPreTransition/k
    
    NB_t  <- outcome_t*INBSurvivalEndpoint # multiply every element by INBSurvivalEndpoint (1st step in converting to NB)
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    
    # treatment cost may depend on duration of treatment
    # test data
    # treatUntilProgression_tn = "No" 
    # outcome_t <- matrix(rnorm(40, 1, 1), ncol = 4)
    # tn = 2
    # maxDurationOfTreatmentMonths_tn = 0.5
    # takes account of a max duration of treatment for each treatment (tn = 1, 2, 3, 4...)
    durationOfTreatmentCalc <- function(treatUntilProgression_tn, outcome_t, tn, maxDurationOfTreatmentMonths_tn){
      if(treatUntilProgression_tn == "Yes"){
        # treatmetn duration is just the survival time
        treatmentDurationMonths_tn <- outcome_t[,tn]
      } else {
        # replace each sim which is greater than the max with the max
        treatmentDurationMonths_tn <- ifelse(outcome_t[,tn] > maxDurationOfTreatmentMonths_tn, maxDurationOfTreatmentMonths_tn, outcome_t[,tn])
      }
      return(treatmentDurationMonths_tn)
    }
    
    
    treatmentDurationMonths_t1 <- durationOfTreatmentCalc(treatUntilProgression_t1, outcome_t, 1, maxDurationOfTreatmentMonths_t1)
    treatmentDurationMonths_t2 <- durationOfTreatmentCalc(treatUntilProgression_t2, outcome_t, 2, maxDurationOfTreatmentMonths_t2)
    treatmentDurationMonths_t3 <- durationOfTreatmentCalc(treatUntilProgression_t3, outcome_t, 3, maxDurationOfTreatmentMonths_t3)
    treatmentDurationMonths_t4 <- durationOfTreatmentCalc(treatUntilProgression_t4, outcome_t, 4, maxDurationOfTreatmentMonths_t4)
    
    addCost_t <- c(-(treatmentDurationMonths_t1*treatmentCostsMonthly_t1)/k ,
                   -(treatmentDurationMonths_t2*treatmentCostsMonthly_t2)/k,
                   -(treatmentDurationMonths_t3*treatmentCostsMonthly_t3)/k, 
                   -(treatmentDurationMonths_t4*treatmentCostsMonthly_t4)/k) 
    NB_t  <- NB_t  + addCost_t
    
    # generate and format costs table (treatment costs can depend on survival)
    # calculate expected costs
    cost_t1 <- mean(treatmentDurationMonths_t1*treatmentCostsMonthly_t1)
    cost_t2 <- mean(treatmentDurationMonths_t2*treatmentCostsMonthly_t2)
    cost_t3 <- mean(treatmentDurationMonths_t3*treatmentCostsMonthly_t3)
    cost_t4 <- mean(treatmentDurationMonths_t4*treatmentCostsMonthly_t4)
    cost_t <- c(cost_t1, cost_t2, cost_t3, cost_t4)
    Cost_per_individual <- paste0(currencySymbol,formatC(cost_t, big.mark = ',', format = 'd'))
    Yearly_costs <- paste0(currencySymbol,formatC(cost_t*incidence, big.mark = ',', format = 'd'))
    Additional_cost_per_year <- paste0(currencySymbol,formatC((cost_t - cost_t1)*incidence, big.mark = ',', format = 'd'))
    popTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
    Total_Costs <- paste0(currencySymbol,formatC(cost_t*popTotal, big.mark = ',', format = 'd'))
    Treatment_name <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)
    tableTreatmentCostsDF <- as.data.frame(cbind(Treatment_name, Cost_per_individual, Yearly_costs, Additional_cost_per_year, Total_Costs))
    tableTreatmentCostsDF <- tableTreatmentCostsDF[1:numberOfTreatments,]
    
    # return list
    NBOutput <- list(NB_t = NB_t, tableTreatmentCostsDF = tableTreatmentCostsDF)
    return(NBOutput)
    
  }
  
}



# test function
# outcomeToNB2_t(outcome_t <- matrix(rnorm(40, 1, 1), ncol = 4),
#               MCsims = 10,
#               typeOfEndpoint= "continuous", # "survival", # "binary", "continuous"
#               typeOfOutcome= "netHealth",
#               tCostsDependOnEvent= "No", # "Yes", "No"
#               MCD_t2 = 0.1,
#               MCD_t3 = 0.2,
#               MCD_t4 = 0.3,
#               cost_t1 = 20,
#               cost_t2= 30,
#               cost_t3 = 40,
#               cost_t4 = 60,
#               k = 13000,
#               currencySymbol = "£",
#               incidence = 100,
#               discountRate = 0.03 ,
#               timeInformation = 15,
#               nameOf_t1 = "1",
#               nameOf_t2 = "2",
#               nameOf_t3 = "3",
#               nameOf_t4 = "4",
#               numberOfTreatments = 4,
#               costEvent_t1 = 10,
#               costEvent_t2 = 20,
#               costEvent_t3 = 30,
#               costEvent_t4 = 40,
#               costNotEvent_t1 = 50,
#               costNotEvent_t2 = 60,
#               costNotEvent_t3 = 70,
#               costNotEvent_t4 = 80,
# 
#               numberS0States = 4,
#               numberS1States = 4 ,
#               utility_s01 = 0,
#               utility_s02 = 0.11,
#               utility_s03 = 0.41,
#               utility_s04 = 0.58,
#               utility_s11 = 0.7,
#               utility_s12 = 0.81,
#               utility_s13 = 0.96,
#               utility_s14 = 1,
#               lifeDuration_s01 = 0,
#               lifeDuration_s02 = 7.11,
#               lifeDuration_s03 = 12.52,
#               lifeDuration_s04 = 12.52,
#               lifeDuration_s11 = 16.73,
#               lifeDuration_s12 = 16.73,
#               lifeDuration_s13 = 19.23,
#               lifeDuration_s14 = 19.23,
#               cost_s01 = 0,
#               cost_s02 = 45450,
#               cost_s03 = 154324,
#               cost_s04 = 154324,
#               cost_s11 = 27047,
#               cost_s12 = 27047,
#               cost_s13 = 19575,
#               cost_s14 = 19575,
#               probability_s01 = 0.29,
#               probability_s02 = 0.07,
#               probability_s03 = 0.41,
#               probability_s04 = 0.23,
#               probability_s11 = 0.42,
#               probability_s12 = 0.24,
#               probability_s13 = 0.2,
#               probability_s14 = 0.14,
#               # continuous NB
#               deltaUnitUtilityDirection = "decrease", # "increase" # Q Is an increase in the primary outcome expected to be associated with an increase or decrease in health state utility?
#               deltaUnitUtilitySize = 0.1, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease the health state utility?
#               treatmentDurationMonths = 12, # Q how long is the treatment effect expected to last
#               deltaUnitCostsDirection = "decrease", # "increase" # Q Is an increase in the primary outcome expected to be associated with disease related cost increases or decreases?
#               deltaUnitCostsSize = 200, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease disease related costs?
#               treatmentCostsMonthly_t1 = 100, # used in survival too
#               treatmentCostsMonthly_t2 = 200,# used in survival too
#               treatmentCostsMonthly_t3 = 300,# used in survival too
#               treatmentCostsMonthly_t4 = 400,# used in survival too
#               # survival NB
#               utilityPreTransition = 0.5, # Q What is the health utility associated with the pre-progression health state?
#               monthlyCostPreTransition = 2000, # Q What are the expected monthly disease related costs associated with the pre-transition health state?
#               treatUntilProgression_t1 = "Yes", # "No"  Q Are individuals always treated until progression under the baseline treatment?
#               maxDurationOfTreatmentMonths_t1 = NA, # Q what is the maximum number of months that the baseline treatment will be given?
#               treatUntilProgression_t2 = "No", # "No" "Yes"
#               maxDurationOfTreatmentMonths_t2 = 12,
#               treatUntilProgression_t3 = "No", # "No" "Yes"
#               maxDurationOfTreatmentMonths_t3 = 24,
#               treatUntilProgression_t4 = "No", # "No" "Yes"
#               maxDurationOfTreatmentMonths_t4 = 24
#               )



















