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




















































