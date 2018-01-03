# Functions for use in VOI models
# 
# Author: David Glynn
# Date: 12 Dec 2017; revised 
###############################################################################
library(fdrtool) # required for halfnormal simulations


# test data
#P_t0 <- rep(0.3, 10)
#mu <- 0
#variance <- 0.2

# function to simulate the probability of the event with a particular treatment
# for a normal distribution on relative effect

simProbOfOutcomeNormBinary <- function(P_t1, mu, variance){

  Odds_t1 <- P_t1 / (1 - P_t1)
  LO_t1 <- log(Odds_t1)
  LOR_tn <- rnorm(length(P_t1), mu, sqrt(variance)) # simulate normal log odds ratio
  LO_tn <- LO_t1 + LOR_tn # combine baseline and relative effect and convert back to probability
  Odds_tn <- exp(LO_tn)
  Odds_tn / (Odds_tn + 1) # output P_tn a vector of probabilities
}

# test data
# direction <- "alwaysPositive" # takes value "alwaysPositive" or "alwaysNegative" 
  


# function to simulate the probability of the event with a particular treatment
# for a HALF normal distribution on relative effect
simProbOfOutcomeHalfNormBinary <- function(P_t1, direction, variance){
  
  if(is.na(direction)){P_tn <- NA} else {  # check that there is a value for direction
    
  Odds_t1 <- P_t1 / (1 - P_t1)
  LO_t1 <- log(Odds_t1)
  if (direction == "alwaysPositive"){
    LOR_tn <- -rhalfnorm(length(P_t1), theta =  sd2theta(sqrt(variance)) ) # simulate normal log odds ratio
    # draws from a negative halfnormal
  } else {
    LOR_tn <- rhalfnorm(length(P_t1), theta =  sd2theta(sqrt(variance)) ) # simulate normal log odds ratio
    # draws from a positive halfnormal
  }
  LO_tn <- LO_t1 + LOR_tn # combine baseline and relative effect and convert back to probability
  Odds_tn <- exp(LO_tn)
  Odds_tn / (Odds_tn + 1) # output P_tn a vector of probabilities
  
  }

}


# test data
#numberOfTreatments <- 4
#P_t1 <- rep(0.9, 10)
#mu_t1 <- 0
#variance_t1 <- 0.1
#mu_t2 <- 1
#variance_t2 <- 100
#mu_t3 <- 100
#variance_t3 <- 0.001
#dist_t1 <- "norm" 
#direction_t1 <- "alwaysPositive"
#dist_t2 <- "halfNorm" 
#direction_t2 <- "alwaysNegative" 
#dist_t3 <- "norm" 
#direction_t3 <- "alwaysNegative" 
 

#numberOfTreatments =2 
#MCsims = 100
#P_t1 =0.5
#mu_t2=0
#variance_t2=1
#dist_t2="norm"
#direction_t2= NA
#mu_t3=NA
#variance_t3=NA
#dist_t3=NA
#direction_t3=NA
#mu_t4=NA 
#variance_t4=NA
#dist_t4=NA
#direction_t4=NA
#nameOf_t1="1"
#nameOf_t2="2"
#nameOf_t3=NA
#nameOf_t4=NA
#typeOfOutcome="benefit"
#incidence=1000
#timeInformation=15
#discountRate=3.5 
#durationOfResearch= 4
#costResearchFunder=1000000
#MCD_t2=0
#MCD_t3=NA
#MCD_t4=NA
#utilisation_t1=100
#utilisation_t2=0
#utilisation_t3=NA
#utilisation_t4=NA
#P_t1 <- rep(P_t1, MCsims)


# master function which uses the above functions to create the P_t1 matrix
# requires simProbOfOutcomeNormBinary and simProbOfOutcomeHalfNormBinary

simProbOfOutcomeMatrixBinary <- function(numberOfTreatments, P_t1,
                                   mu_t2, variance_t2, dist_t2, direction_t2,
                                   mu_t3, variance_t3, dist_t3, direction_t3,
                                   mu_t4, variance_t4, dist_t4, direction_t4
                                   ){
  
  # simulate the probabilities for t2
  P_t2 <- if (dist_t2 == "norm") {
    simProbOfOutcomeNormBinary(P_t1, mu_t2, variance_t2)
  } else {
    simProbOfOutcomeHalfNormBinary(P_t1, direction_t2, variance_t2)
  }
  
  # simulate the probabilities for t3
  P_t3 <- if(numberOfTreatments <= 2 ) { # if there is only 2 treatments then this is given a vector of NAs
    rep(NA, length(P_t1))
  } else {
    
    if (dist_t3 == "norm") {
      simProbOfOutcomeNormBinary(P_t1, mu_t3, variance_t3)
    } else {
      simProbOfOutcomeHalfNormBinary(P_t1, direction_t3, variance_t3)
    }
    
  }
    
  # simulate the probabilities for t4
  P_t4 <- if(numberOfTreatments <= 3 ) { # if there is only 2 treatments then this is given a vector of NAs
    rep(NA, length(P_t1))
  } else {
    
    if (dist_t4 == "norm") {
      simProbOfOutcomeNormBinary(P_t1, mu_t4, variance_t4)
    } else {
      simProbOfOutcomeHalfNormBinary(P_t1, direction_t4, variance_t4)
    }
    
  }
  
  # add all vectors (P_t1 , P_t2..) to the matrix P_t
  # and return this
  P_t <- matrix(c(P_t1, P_t2, P_t3, P_t4), ncol = 4)

  P_t
  
}

# test simulation
#simProbOfOutcomeMatrixBinary (numberOfTreatments = 3, P_t1 = rep(0.1, 10),
#                        mu_t2 = 0, variance_t2 = 0.1, dist_t2 = "norm",  direction_t2 = "alwaysPositive",
#                        mu_t3 = 0.2, variance_t3 = 0.1, dist_t3 = "halfNorm", direction_t3 = "alwaysPositive",
#                        mu_t4 = NA, variance_t4 = NA, dist_t4 = "halfNorm", direction_t4 = NA
#                        )




# basic population function - calculates the population numbers required in the model

verybasicPop <- function(incidence, discountRate, durationOfResearch, timeInformation){
  
  #                                        time end                time start  
  PopTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
  popDuringResearch <-  ((incidence) /-discountRate) * (exp(-discountRate*durationOfResearch) - exp(-discountRate*0))
  popAfterResearch <-  ((incidence) /-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*durationOfResearch))
  
  output <- list(PopTotal = PopTotal,
                 popDuringResearch = popDuringResearch,
                 popAfterResearch = popAfterResearch)
  
  return(output)
}


# # test data for NBtoEVPIResults
# nameOf_t1 <- "late PTP"
# nameOf_t2 <- "early PTP"
# nameOf_t3 <- "treatment 3"
# nameOf_t4 <- "treatment 4"
# typeOfOutcome <- "benefit" # "harm" "netHealth" # was Benefit==1 or 0 for benefit or harm
# incidence = 8000 # was Incidence
# timeInformation  = 15 # Time_info  = 15
# discountRate = 3.5  #D_rate = 0.035 ***NB need to divide by 100
# costResearchFunder = 882177 #Cost_research_funder =  882177
# durationOfResearch = 3  # Time_research = 3
# utilisation_t1 = 0.5 # check these sum to 1. 
# utilisation_t2 = 0.5
# utilisation_t3 = 0
# utilisation_t4 = NA
# 
# NB_t <- simProbOfOutcomeMatrixBinary (numberOfTreatments = 3, P_t1 = rep(0.1, 10),
#                           mu_t2 = 0, variance_t2 = 0.1, dist_t2 = "norm",  direction_t2 = "alwaysPositive",
#                           mu_t3 = 0.2, variance_t3 = 0.1, dist_t3 = "halfNorm", direction_t3 = "alwaysPositive",
#                           mu_t4 = NA, variance_t4 = NA, dist_t4 = "halfNorm", direction_t4 = NA
#                           )
# 
# costHealthSystem = 100000 # **note this!
# k = 13000 # **note this
# 
# 

# failed!
#testData_NBtoEVPIResults <- list(nameOf_t1 <- "late PTP",nameOf_t2 <- "early PTP",nameOf_t3 <- "treatment 3"
#                                 ,nameOf_t4 <- "treatment 4",typeOfOutcome <- "benefit" ,incidence = 8000 # was Incidence
#                                 ,timeInformation  = 15 ,discountRate = 3.5  ,costResearchFunder = 882177 #Cost_research_funder =  882177
#                                 ,durationOfResearch = 3  ,utilisation_t1 = 0.5 ,utilisation_t2 = 0.5
#                                 ,utilisation_t3 = 0,utilisation_t4 = NA
#                                 ,NB_t <- simProbOfOutcomeMatrixBinary (numberOfTreatments = 3, P_t1 = rep(0.1, 10),
#                                                                       mu_t2 = 0, variance_t2 = 0.1, dist_t2 = "norm",  direction_t2 = "alwaysPositive",
#                                                                       mu_t3 = 0.2, variance_t3 = 0.1, dist_t3 = "halfNorm", direction_t3 = "alwaysPositive",
#                                                                       mu_t4 = NA, variance_t4 = NA, dist_t4 = "halfNorm", direction_t4 = NA)
#                                 ,costHealthSystem = NA ,k = NA )


# takes in a matrix of net benefits and outputs all relevant EVPI metrics
# Requires: verybasicPop
# Consider: adding convergence check! make sure current implementation outputs calculating properly 

NBtoEVPIResults <- function(NB_t,
                            nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                            typeOfOutcome, incidence,timeInformation,
                            discountRate ,durationOfResearch,costResearchFunder,
                            MCD_t2, MCD_t3, MCD_t4,
                            utilisation_t1, utilisation_t2,
                            utilisation_t3, utilisation_t4,
                            costHealthSystem = NA, k = NA){
  
  # define variables required
  MCsims <- nrow(NB_t) # impled number of simulations
  utilisation_t1 <- utilisation_t1/100 # to convert to decimal value
  utilisation_t2 <- utilisation_t1/100
  utilisation_t3 <- utilisation_t1/100
  utilisation_t4 <- utilisation_t1/100
  Utilisation_t <- c(utilisation_t1, utilisation_t2, utilisation_t3, utilisation_t4)
  discountRate <- discountRate/100
  
  
  # expected outcome with each treatment (uninformed prior)
  ENB_t  <- apply(NB_t , 2, mean)
  
  # Best outcome with current information
  NB_EVTCI  = max(ENB_t , na.rm = TRUE)
  
  # optimalTreatment: tells you which treatment is best given current information
  optimalTreatment <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)[which(ENB_t  == max(ENB_t , na.rm = TRUE))]
  
  # Expected value of treating with perfect information
  NB_VTPI  <- apply(NB_t , 1, max, na.rm = TRUE) #so I can check convergence - COULD ADD THIS CHECK
  NB_EVTPI  <- mean(NB_VTPI )
  NB_EVPI  <-  NB_EVTPI  - NB_EVTCI 
  
  # probability each treatment has highest NB - provides vector of probabilities
  # for the column of simulated NBs for each treatment (x)
  # take the sum of the number of times that that treatment is the maximum NB
  # divide by the number of sumulations to get the probability
  Probability_t_is_max <- apply(NB_t , 2, function(x) sum(x==NB_VTPI , na.rm = TRUE))/MCsims
  
  probTreatment1isMax <- Probability_t_is_max[1]
  probTreatment2isMax <- Probability_t_is_max[2]
  probTreatment3isMax <- Probability_t_is_max[3]
  probTreatment4isMax <- Probability_t_is_max[4]
  
  #############################################
  # population 
  Popoutputs <- verybasicPop(incidence, discountRate, durationOfResearch, timeInformation)
  
  popDuringResearch <- Popoutputs$popDuringResearch 
  popAfterResearch <- Popoutputs$popAfterResearch 
  PopTotal <- Popoutputs$PopTotal
  
  
  ########### BASIC TRIAL ANALYSIS ################################################
  
  # YEARLY OUTCOMES #
  valueOfResearchPerYear <- NB_EVTPI *incidence - NB_EVTCI *incidence
  valueOfImplementationPerYear <- incidence*NB_EVTCI  - sum(ENB_t * Utilisation_t*incidence, na.rm = TRUE)
  
  # histogram of effects per year
  #               # NB per simulation with max(ENB_t ) - max NB per simulation
  #                 # best treament with current evidence - max NB per simulation
  # calculate loss from not having perfect information each year
  NB_loss_maxt <- NB_t[,which(ENB_t  == max(ENB_t , na.rm = TRUE))] - NB_VTPI 
  Hist_value_of_trial_per_year <- hist(-NB_loss_maxt*incidence)
  # convert to probability plot, not density
  Hist_value_of_trial_per_year$density = Hist_value_of_trial_per_year$counts/sum(Hist_value_of_trial_per_year$counts)*100
  #plot(Hist_value_of_trial_per_year,freq=FALSE,
  #     main = "Consequences of uncertainty (per year)",
  #     xlab = "Primary outcomes",
  #     ylab = "Probability (%)")
  
  # output the list which is required to produce the VOI histogram - the plot will be constructed with
  # this output so that it can be publised in shinyapps.io
  ListForhistVOIYear <-  Hist_value_of_trial_per_year
  
  # this was a previous failed attempt, would not publish on shinyapps.io
  # base graphics draw directly on a device.
  #histVOIYear <- recordPlot() #record the histogram from the device
  #plot.new() ## clean up device
  
  # FULL TIME OUTCOMES #
  
  ## Cell_A : net benefit of current situation with current utilisation
  # take the weighted average of the expected NB for each treatment scaled up to full population
  Cell_A <- sum(ENB_t *Utilisation_t*PopTotal, na.rm = TRUE)
  
  ## Cell_B: need to add this?
  
  ## Cell_C : maximum Net benfit of implemetation (Early access - approval)  
  Cell_C <- PopTotal*NB_EVTCI 

  ## Cell_D : maximum Net benfit of information (delay access for information)
  # "instant trial with perfect information"
  # Pure definition of Cell D
  Cell_D <- NB_EVTPI *PopTotal
  
  # assume perfect and instant implementation/information
  # and no costs of research imposed on health system
  maxvalueOfImplementation <- Cell_C - Cell_A # max value of early access
  maxvalueOfResearch <- Cell_D - Cell_C 
  
  
  # calculating the benefits of research (under differnt assumptions)
  ########################################
  # perfect info and perfect implementation (includes that it is instant)
  NB_instant_research_perfect_info_imp <- Cell_D
  
  # cu = current utilisation. NOT instant trial - while trial is running just keep whatever treatment 
  # utilisation is theere at the start of the trial
  # Below is the same as OIR if the use of the new treatment is restricted
  NB_cu_perfect_info_imp <- sum(ENB_t*Utilisation_t*popDuringResearch, na.rm = TRUE) + popAfterResearch*NB_EVTPI  
  
  # maxt = use the best treatmet according to current NB. NOT instant trial - 
  # instantly and perfectly implement best treatment while trial is running 
  # below is the same as AWR if the new treatment is the best with current information
  NB_maxt_perfect_info_imp <- popDuringResearch*NB_EVTCI  + popAfterResearch*NB_EVTPI 
  
  healthOpportunityCostsOfResearch <- - costHealthSystem/k
  
  # this is the pure information value under different types of research and implementation assumptions
  valueOfResearchWithCurrentImplementation <- if(typeOfOutcome == "netHealth") {
    NB_cu_perfect_info_imp - Cell_C - costHealthSystem/k   # subtract costs to health system if it is in NB
  } else {
    NB_cu_perfect_info_imp - Cell_C
  }
  
  # the value of research if the best treatment is implemented during the trial
  # aka the pure informaiton value of the research
  valueOfResearchWithPerfectImplementation <- if (typeOfOutcome == "netHealth"){
    NB_maxt_perfect_info_imp - Cell_C - costHealthSystem/k   # subtract costs to health system if it is in NB
  } else {
    NB_maxt_perfect_info_imp - Cell_C
  }
    
  
  # ICER of research relative to early access (assumed to be costless to the agency)
  # all other costs assumed to be captured by the MCD
  ICER_ResearchWithCurrentImplementation <- costResearchFunder/valueOfResearchWithCurrentImplementation
  ICER_ResearchWithPerfectImplementation <- costResearchFunder/valueOfResearchWithPerfectImplementation

  valuePer15KResearchSpend <- (valueOfResearchWithPerfectImplementation/costResearchFunder)*15000
  
  
  
  # complete list of outputs
  ###########################
  NBtoEVPIResults <- list(
    optimalTreatment = optimalTreatment,
    probTreatment1isMax = probTreatment1isMax, 
    probTreatment2isMax = probTreatment2isMax, 
    probTreatment3isMax = probTreatment3isMax, 
    probTreatment4isMax = probTreatment4isMax,
    popDuringResearch = popDuringResearch,
    popAfterResearch = popAfterResearch,
    PopTotal = PopTotal, 
    #histVOIYear = histVOIYear, 
    ListForhistVOIYear = ListForhistVOIYear,
    valueOfResearchPerYear = valueOfResearchPerYear,
    valueOfImplementationPerYear = valueOfImplementationPerYear,
    Cell_A = Cell_A,
    Cell_C = Cell_C,
    Cell_D = Cell_D,
    maxvalueOfImplementation = maxvalueOfImplementation,
    maxvalueOfResearch = maxvalueOfResearch,
    healthOpportunityCostsOfResearch = healthOpportunityCostsOfResearch,
    valueOfResearchWithCurrentImplementation = valueOfResearchWithCurrentImplementation,
    valueOfResearchWithPerfectImplementation = valueOfResearchWithPerfectImplementation,
    ICER_ResearchWithCurrentImplementation = ICER_ResearchWithCurrentImplementation,
    ICER_ResearchWithPerfectImplementation = ICER_ResearchWithPerfectImplementation,
    valuePer15KResearchSpend = valuePer15KResearchSpend
    
    
  )
  
  # return this list from the function
  NBtoEVPIResults
  
}


# TEST THE function
#do.call(NBtoEVPIResults , as.list(testData_NBtoEVPIResults))





# test the function:
# # costruct input matrix
# NB_t <- simProbOfOutcomeMatrixBinary (numberOfTreatments = 3, P_t1 = rep(0.1, 10),
#                                       mu_t2 = 0, variance_t2 = 0.1, dist_t2 = "norm",  direction_t2 = "alwaysPositive",
#                                       mu_t3 = 0.2, variance_t3 = 0.1, dist_t3 = "halfNorm", direction_t3 = "alwaysPositive",
#                                       mu_t4 = NA, variance_t4 = NA, dist_t4 = "halfNorm", direction_t4 = NA
# )

# resultlist <- NBtoEVPIResults(NB_t = NB_t,
#                 nameOf_t1 = "1",nameOf_t2 = "2", nameOf_t3 = "3", nameOf_t4 = "4",
#                 typeOfOutcome = "benefit", incidence = 1000 ,timeInformation = 15,
#                 discountRate = 3.5 ,durationOfResearch = 5,costResearchFunder = 1500000,
#                 MCD_t2 = 0, MCD_t3 = 0, MCD_t4 = 0,
#                 utilisation_t1 = 50, utilisation_t2 = 50,
#                 utilisation_t3 = 0, utilisation_t4 =0,
#                 costHealthSystem = NA, k = NA)

#resultlist$histVOIYear
