#####################
# Supplementary functions for Feasibility studies
######################
options(scipen = 999) # turn off scientific notation


############################
# feasibility population function - calculates the population numbers required for feasibility analysis
############################
# works well but tiny leftover when result should be zero : -4.656613e-10
# note discount rate converted 

feasibilityPop <- function(incidence, discountRate, durationOfResearchDefinitive, timeInformation, durationOfResearchFeas){
  
  discountRate <- discountRate/100 # convert from 3.5 to 0.035
  
  #                                        time end                time start  
  PopTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
  
  durationOfResearchFeas_adj <- ifelse(durationOfResearchFeas < timeInformation, durationOfResearchFeas, timeInformation )  # need to make sure the delay is not longer than timeInformation
  PopDuringFeasResearch <- (incidence/-discountRate) * (exp(-discountRate*durationOfResearchFeas_adj) - exp(-discountRate*0))
  
  durationOfResearchDefinitive_end_adj <- ifelse(durationOfResearchFeas_adj + durationOfResearchDefinitive > timeInformation, 
                                  timeInformation,
                                  durationOfResearchFeas_adj + durationOfResearchDefinitive)
  
  PopDuringDefinitiveResearch <-  ((incidence) /-discountRate) * (exp(-discountRate*durationOfResearchDefinitive_end_adj) - exp(-discountRate*durationOfResearchFeas_adj))
  
  
  PopAfterDefinitiveResearch <-  ((incidence) /-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*durationOfResearchDefinitive_end_adj))
  
  output <- list(PopTotal = PopTotal,
                 PopDuringFeasResearch = PopDuringFeasResearch,
                 PopDuringDefinitiveResearch = PopDuringDefinitiveResearch,
                 PopAfterDefinitiveResearch = PopAfterDefinitiveResearch)
  
  return(output)
}

# test feasibilityPop funtion
# pops <- feasibilityPop(incidence =  355000*0.73 ,
#             timeInformation =  5   ,
#             discountRate =  0.035,
#             durationOfResearchDefinitive =  3,
#             durationOfResearchFeas = 6)
# pops$PopTotal - pops$PopDuringFeasResearch - pops$PopDuringDefinitiveResearch - pops$PopAfterDefinitiveResearch





# # test data for NBtoEVPIResultsFeas
nameOf_t1 <- "late PTP"
nameOf_t2 <- "early PTP"
nameOf_t3 <- "treatment 3"
nameOf_t4 <- "treatment 4"
typeOfOutcome <- "benefit" # "harm" "netHealth" # was Benefit==1 or 0 for benefit or harm
incidence = 8000 # was Incidence
timeInformation  = 15 # Time_info  = 15
discountRate = 3.5  #D_rate = 0.035 ***NB need to divide by 100
costResearchFunderFeas = 100000
costResearchFunderDefinitive = 882177 #Cost_research_funder =  882177
durationOfResearchDefinitive = 3 #durationOfResearch = 3  # Time_research = 3
durationOfResearchFeas = 1
utilisation_t1 = 50 # check these sum to 100***.
utilisation_t2 = 50
utilisation_t3 = 0
utilisation_t4 = NA
NB_t <- simProbOfOutcomeMatrixBinary (numberOfTreatments = 3, P_t1 = rep(0.1, 10),
                          mu_t2 = 0, variance_t2 = 0.1, dist_t2 = "norm",  direction_t2 = "alwaysPositive",
                          mu_t3 = 0.2, variance_t3 = 0.1, dist_t3 = "halfNorm", direction_t3 = "alwaysPositive",
                          mu_t4 = NA, variance_t4 = NA, dist_t4 = "halfNorm", direction_t4 = NA
                          )
ProbabilityOfDefinitiveResearch = 0.5
costHealthSystemFeas = 100000 # Cost_research_pilot_NHS
costHealthSystemDefinitive = 2000000
k = 13000 

# takes in a matrix of net benefits and outputs all relevant EVPI metrics
# Requires: feasibilityPop
# Consider: adding convergence check! make sure current implementation outputs calculating properly 
# ---- should i have NA's in the default inputs???

NBtoEVPIResultsFeas <- function(NB_t,
                            nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                            typeOfOutcome, incidence,timeInformation,
                            discountRate ,durationOfResearchDefinitive,
                            durationOfResearchFeas,costResearchFunderFeas,
                            costResearchFunderDefinitive,
                            MCD_t2, MCD_t3, MCD_t4,
                            utilisation_t1, utilisation_t2,
                            utilisation_t3, utilisation_t4,
                            ProbabilityOfDefinitiveResearch,
                            costHealthSystemFeas = NA,costHealthSystemDefinitive =NA, k = NA){
  
  # define variables required
  MCsims <- nrow(NB_t) # impled number of simulations
  Utilisation_t <- c(utilisation_t1/100, utilisation_t2/100, utilisation_t3/100, utilisation_t4/100)

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
  
  Popoutputs <- feasibilityPop(incidence, discountRate, durationOfResearchDefinitive, 
                               timeInformation, durationOfResearchFeas)
    
  
  PopTotal <-  Popoutputs$PopTotal
  PopDuringFeasResearch <- Popoutputs$PopDuringFeasResearch
  PopDuringDefinitiveResearch <- Popoutputs$PopDuringDefinitiveResearch
  PopAfterDefinitiveResearch <- Popoutputs$PopAfterDefinitiveResearch
  
  
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
  # --- return to this -----
  #NB_cu_perfect_info_imp <- sum(ENB_t*Utilisation_t*popDuringResearch, na.rm = TRUE) + popAfterResearch*NB_EVTPI  
  
  # maxt = use the best treatmet according to current NB. NOT instant trial - 
  # instantly and perfectly implement best treatment while trial is running 
  # below is the same as AWR if the new treatment is the best with current information
  # --- return to this -----
  #NB_maxt_perfect_info_imp <- popDuringResearch*NB_EVTCI  + popAfterResearch*NB_EVTPI 
  
  # gross net benefit with perfect implementation during the trial
  NB_E_maxt_trial <- 
    # if its a net benefit analysis the NHS cost of pilot is always subtracted
    ifelse(typeOfOutcome == "netHealth" ,- costHealthSystemFeas/k, 0) +
    # If definitive trial HAPPENS
    ProbabilityOfDefinitiveResearch*(
      PopDuringFeasResearch*NB_EVTCI + PopDuringDefinitiveResearch*NB_EVTCI +
        PopAfterDefinitiveResearch*NB_EVTPI + ifelse(typeOfOutcome == "netHealth" ,- costHealthSystemDefinitive/k, 0)
    ) +
    # If definitive trial DOES NOT happen (everybody just gets best treatment)
    (1 - ProbabilityOfDefinitiveResearch)*(
      PopTotal*NB_EVTCI 
    )
  
  # expected value of treating with current utilisation
  # 
  NB_EVTCU <-  sum(ENB_t*Utilisation_t, na.rm = TRUE)
  
  # gross net benefit with current implementation during the trial
  NB_E_cu_trial <- 
    # if its a net benefit analysis the NHS cost of pilot is always subtracted
    ifelse(typeOfOutcome == "netHealth" ,- costHealthSystemFeas/k, 0) +
    # If definitive trial HAPPENS
    ProbabilityOfDefinitiveResearch*(
      PopDuringFeasResearch*NB_EVTCU + PopDuringDefinitiveResearch*NB_EVTCU +
        PopAfterDefinitiveResearch*NB_EVTPI + ifelse(typeOfOutcome == "netHealth" ,- costHealthSystemDefinitive/k, 0)
    ) +
    # If definitive trial DOES NOT happen (everybody just gets best treatment)
    (1 - ProbabilityOfDefinitiveResearch)*(
      PopTotal*NB_EVTCU 
    )
  
  # this is the pure information value under different types of research and implementation assumptions
  # Cell_C : maximum Net benfit of implemetation (Early access - approval)  
  # the value here is the value of the feasibility (with imperfect implementation) 
  # compared to implementing the optimal treatment with 
  # current information
  valueOfResearchWithCurrentImplementation <- NB_E_cu_trial - Cell_C 
  
  # same as above but for perfect implementation
  valueOfResearchWithPerfectImplementation <- NB_E_maxt_trial - Cell_C 
  
  # expected research funder costs
  # new output for pilot studies
  ExpectedCostResearchFunder <- 
    costResearchFunderFeas +  # always incur this cost
    # If definitive trial HAPPENS
    ProbabilityOfDefinitiveResearch*costResearchFunderDefinitive
    
  # ICER of research relative to early access (assumed to be costless to the agency)
  # all other costs assumed to be captured by the MCD
  ICER_ResearchWithCurrentImplementation <- ExpectedCostResearchFunder/valueOfResearchWithCurrentImplementation
  ICER_ResearchWithPerfectImplementation <- ExpectedCostResearchFunder/valueOfResearchWithPerfectImplementation
  
  valuePer15KResearchSpend <- (valueOfResearchWithPerfectImplementation/ExpectedCostResearchFunder)*15000
  
  # expected op costs of the research
  healthOpportunityCostsOfResearch <- 
  -costHealthSystemFeas/k +  # always incur this cost
    # If definitive trial HAPPENS
    ProbabilityOfDefinitiveResearch*-costHealthSystemDefinitive/k
  
  
  # complete list of outputs
  ###########################
  NBtoEVPIResults <- list(
    optimalTreatment = optimalTreatment,
    probTreatment1isMax = probTreatment1isMax, 
    probTreatment2isMax = probTreatment2isMax, 
    probTreatment3isMax = probTreatment3isMax, 
    probTreatment4isMax = probTreatment4isMax,
    #popDuringResearch = popDuringResearch, # removed
    #popAfterResearch = popAfterResearch,   # removed
    PopTotal = PopTotal,
    PopDuringFeasResearch = PopDuringFeasResearch,                      # new output
    PopDuringDefinitiveResearch = PopDuringDefinitiveResearch,          #new output
    PopAfterDefinitiveResearch = PopAfterDefinitiveResearch,            # new output
    PopTotal = PopTotal, 
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
    valuePer15KResearchSpend = valuePer15KResearchSpend,
    ExpectedCostResearchFunder = ExpectedCostResearchFunder # new output for pilot studies
    
    
  )
  
  # return this list from the function
  NBtoEVPIResults
  
}


# test the function- inputs from P1 used
# # costruct input matrix
NB_t <- simProbOfOutcomeMatrixBinary (numberOfTreatments = 3, P_t1 = rep(0.525, 10000),
                                      mu_t2 = 0, variance_t2 = 0.25, dist_t2 = "norm",  direction_t2 = "alwaysPositive",
                                      mu_t3 = 0, variance_t3 = 0.25, dist_t3 = "norm", direction_t3 = "alwaysPositive",
                                      mu_t4 = NA, variance_t4 = NA, dist_t4 = "halfNorm", direction_t4 = NA
)
resultlist <- NBtoEVPIResultsFeas(NB_t = NB_t,
                nameOf_t1 = "PI",nameOf_t2 = "APs", nameOf_t3 = "PI + APs", nameOf_t4 = "4",
                typeOfOutcome = "benefit", incidence = 1563 ,timeInformation = 15,
                discountRate = 3.5 ,durationOfResearchDefinitive = 6,durationOfResearchFeas = 2,
                costResearchFunderFeas = 601480,costResearchFunderDefinitive = 2522710,
                MCD_t2 = 0, MCD_t3 = 0, MCD_t4 = 0,
                utilisation_t1 = 100, utilisation_t2 = 0,
                utilisation_t3 = 0, utilisation_t4 =0,
                ProbabilityOfDefinitiveResearch = 0.5, 
                costHealthSystemFeas = 150000, costHealthSystemDefinitive = 490000,k = 15000)

