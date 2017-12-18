# must load the required functions! SupplementaryFunctions.R

library(shiny)



########################################################################################
# load up required functions

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


# test data
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
#utilisation_t1 = 0.5 # check these sum to 1. 
#utilisation_t2 = 0.5
#utilisation_t3 = 0
#utilisation_t4 = NA

#NB_t <- simProbOfOutcomeMatrixBinary (numberOfTreatments = 3, P_t1 = rep(0.1, 10),
#                          mu_t2 = 0, variance_t2 = 0.1, dist_t2 = "norm",  direction_t2 = "alwaysPositive",
#                          mu_t3 = 0.2, variance_t3 = 0.1, dist_t3 = "halfNorm", direction_t3 = "alwaysPositive",
#                          mu_t4 = NA, variance_t4 = NA, dist_t4 = "halfNorm", direction_t4 = NA
#                          )

#costHealthSystem = 100000 # **note this!
#k = 13000 # **note this

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
  plot(Hist_value_of_trial_per_year,freq=FALSE,
       main = "Consequences of uncertainty (per year)",
       xlab = "Primary outcomes",
       ylab = "Probability (%)")
  
  # base graphics draw directly on a device.
  histVOIYear <- recordPlot() #record the histogram from the device
  plot.new() ## clean up device
  
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
    histVOIYear = histVOIYear, 
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





BinaryOutcomeFunction.v.0.1 <- function(numberOfTreatments, MCsims, P_t1,
                                        mu_t2, variance_t2, dist_t2, direction_t2,
                                        mu_t3, variance_t3, dist_t3, direction_t3,
                                        mu_t4, variance_t4, dist_t4, direction_t4,
                                        nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                        typeOfOutcome, incidence,timeInformation,
                                        discountRate ,durationOfResearch,costResearchFunder,
                                        MCD_t2, MCD_t3, MCD_t4,
                                        utilisation_t1, utilisation_t2,
                                        utilisation_t3, utilisation_t4){
  
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
  
  VOIoutputs <- NBtoEVPIResults(NB_t,
                                nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                typeOfOutcome, incidence,timeInformation,
                                discountRate ,durationOfResearch,costResearchFunder,
                                MCD_t2, MCD_t3, MCD_t4,
                                utilisation_t1, utilisation_t2,
                                utilisation_t3, utilisation_t4,
                                costHealthSystem = NA, k = NA)
  
  # return the list of results
  ###############################
  
  return(VOIoutputs)
  
}



# finish loading required functions 
########################################################################################



shinyServer(function(input, output) {
  
  VOIResults <- reactiveValues()
  
  observeEvent(input$runRCT, {
    if(input$typeOfOutcome == "netHealth"){
      resultsHolder <- reactive({
        # VOI function taking user inputs and returning results
        # for RCT cost and QALY analysis
      })
      # assign results to VOIResults list
      
    }else{
      resultsHolder <- reactive({
        BinaryOutcomeFunction.v.0.1(numberOfTreatments = input$numberOfTreatments , 
                                    MCsims = input$MCsims, P_t1 =input$P_t1,
                                    mu_t2=input$mu_t2, variance_t2=input$variance_t2 ,
                                    dist_t2=input$dist_t2 , direction_t2= input$direction_t2,
                                    mu_t3=input$mu_t3 , variance_t3=input$variance_t3 ,
                                    dist_t3=input$dist_t3 , direction_t3=input$direction_t3 ,
                                    mu_t4=input$mu_t4 , variance_t4=input$variance_t4 ,
                                    dist_t4=input$dist_t4 , direction_t4=input$direction_t4 ,
                                    nameOf_t1=input$nameOf_t1 ,nameOf_t2=input$nameOf_t2 ,
                                    nameOf_t3=input$nameOf_t3 , nameOf_t4=input$nameOf_t4 ,
                                    typeOfOutcome=input$typeOfOutcome ,
                                    incidence=input$incidence,
                                    timeInformation=input$timeInformation ,
                                    discountRate=input$discountRate  ,
                                    durationOfResearch= input$durationOfResearch,
                                    costResearchFunder=input$costResearchFunder ,
                                    MCD_t2=input$MCD_t2 , MCD_t3=input$MCD_t3 ,
                                    MCD_t4=input$MCD_t4 ,
                                    utilisation_t1=input$utilisation_t1 ,
                                    utilisation_t2=input$utilisation_t2 ,
                                    utilisation_t3=input$utilisation_t3 ,
                                    utilisation_t4=input$utilisation_t4 )
      })
      VOIResults$optimalTreatment <- resultsHolder()$optimalTreatment
      VOIResults$probTreatment1isMax <- resultsHolder()$probTreatment1isMax 
      VOIResults$probTreatment2isMax <- resultsHolder()$probTreatment2isMax 
      VOIResults$probTreatment3isMax <- resultsHolder()$probTreatment3isMax 
      VOIResults$probTreatment4isMax <- resultsHolder()$probTreatment4isMax
      VOIResults$popDuringResearch <- resultsHolder()$popDuringResearch
      VOIResults$popAfterResearch <- resultsHolder()$popAfterResearch
      VOIResults$PopTotal <- resultsHolder()$PopTotal 
      VOIResults$histVOIYear <- resultsHolder()$histVOIYear 
      VOIResults$valueOfResearchPerYear <- resultsHolder()$valueOfResearchPerYear
      VOIResults$valueOfImplementationPerYear <- resultsHolder()$valueOfImplementationPerYear
      VOIResults$Cell_A <- resultsHolder()$Cell_A
      VOIResults$Cell_C <- resultsHolder()$Cell_C
      VOIResults$Cell_D <- resultsHolder()$Cell_D
      VOIResults$maxvalueOfImplementation <- resultsHolder()$maxvalueOfImplementation
      VOIResults$maxvalueOfResearch <- resultsHolder()$maxvalueOfResearch
      VOIResults$healthOpportunityCostsOfResearch <- resultsHolder()$healthOpportunityCostsOfResearch
      VOIResults$valueOfResearchWithCurrentImplementation <- resultsHolder()$valueOfResearchWithCurrentImplementation
      VOIResults$valueOfResearchWithPerfectImplementation <- resultsHolder()$valueOfResearchWithPerfectImplementation
      VOIResults$ICER_ResearchWithCurrentImplementation <- resultsHolder()$ICER_ResearchWithCurrentImplementation
      VOIResults$ICER_ResearchWithPerfectImplementation <- resultsHolder()$ICER_ResearchWithPerfectImplementation
      VOIResults$valuePer15KResearchSpend <- resultsHolder()$valuePer15KResearchSpend 
    }
  })
  
  observeEvent(input$runFeas, {
    #  similar code for: feasibility study
  })
  observeEvent(input$runRec, {
    #  similar code for: reconsideration of evidence
  })

  
  output$nameOf_t1 <- renderText({input$nameOf_t1})
  output$nameOf_t2 <- renderText({input$nameOf_t2})
  output$nameOf_t3 <- renderText({input$nameOf_t3})
  output$nameOf_t4 <- renderText({input$nameOf_t4})
  output$nameOfOutcome <- renderText({input$nameOfOutcome})
  
  output$histVOIYear <- renderPlot({VOIResults$histVOIYear})
  
  output$optimalTreatment <- renderText({VOIResults$optimalTreatment})
  output$probTreatment1isMax <- renderText({VOIResults$probTreatment1isMax })
  output$probTreatment2isMax <- renderText({VOIResults$probTreatment2isMax })
  output$probTreatment3isMax <- renderText({VOIResults$probTreatment3isMax })
  output$probTreatment4isMax <- renderText({VOIResults$probTreatment4isMax})
  output$popDuringResearch <- renderText({VOIResults$popDuringResearch})
  output$popAfterResearch <- renderText({VOIResults$popAfterResearch})
  output$PopTotal <- renderText({VOIResults$PopTotal })
  output$valueOfResearchPerYear <- renderText({VOIResults$valueOfResearchPerYear})
  output$valueOfImplementationPerYear <- renderText({VOIResults$valueOfImplementationPerYear})
  output$Cell_A <- renderText({VOIResults$Cell_A})
  output$Cell_C <- renderText({VOIResults$Cell_C})
  output$Cell_D <- renderText({VOIResults$Cell_D})
  output$maxvalueOfImplementation <- renderText({VOIResults$maxvalueOfImplementation})
  output$maxvalueOfResearch <- renderText({VOIResults$maxvalueOfResearch})
  output$healthOpportunityCostsOfResearch <-   renderText({VOIResults$healthOpportunityCostsOfResearch})
  output$valueOfResearchWithCurrentImplementation <- renderText({VOIResults$valueOfResearchWithCurrentImplementation})
  output$valueOfResearchWithPerfectImplementation <- renderText({VOIResults$valueOfResearchWithPerfectImplementation})
  output$ICER_ResearchWithCurrentImplementation <- renderText({VOIResults$ICER_ResearchWithCurrentImplementation})
  output$ICER_ResearchWithPerfectImplementation <- renderText({VOIResults$ICER_ResearchWithPerfectImplementation})
  output$valuePer15KResearchSpend <- renderText({VOIResults$valuePer15KResearchSpend})
  
})