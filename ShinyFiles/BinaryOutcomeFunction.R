##############################
# Binary Outcome function v.0.1 
###############################
# calculates P_t (matrix) 
# allow for 4 treatments
# allow MCD
# only EVPI
# does not (and cannot) take account of NHS costs of research - need a different model for this



# Note*** test for this bug!
# good chance there is a bug in $Probability_t_is_max when the primary outcome is a harm


########################
# supporting functions 
#########################
# functions required for this to work

# found in SupplementaryFunctions.R
# required to simulate probabilities of outcome
# must be loaded in this order
# simProbOfOutcomeNormBinary
# simProbOfOutcomeHalfNormBinary
# simProbOfOutcomeMatrixBinary
# basic population function - calculates the population numbers required in the model
# verybasicPop


# functions not used here (code exists in NETSCC codes: GenericBinaryOutcome.R)
##
# allow for utilisation based on stat significance
# UtilMatrix.fn 
# allow implementation to depend on an observed MCD
# UtilMatrixMCD.fn 
# Implementation adjusted EVTPI 
# EVTPIadjImp.fn 




# mock data
numberOfTreatments <- 2
P_t0 <- rep(0.9, 10)
mu_t1 <- 0
variance_t1 <- 0.1
dist_t1 <- "norm" 
direction_t1 <- "alwaysPositive"
#mu_t2 <- 1
#variance_t2 <- 100
#dist_t2 <- "halfNorm" 
#direction_t2 <- "alwaysNegative" 
#mu_t3 <- 100
#variance_t3 <- 0.001
#dist_t3 <- "norm" 
#direction_t3 <- "alwaysNegative"

typeOfOutcome <- "benefit" # "harm" "netHealth" # was Benefit==1 or 0 for benefit or harm
incidence = 8000 # was Incidence
timeInformation  = 15 # Time_info  = 15
discountRate = 3.5  #D_rate = 0.035 ***NB need to divide by 100
costResearchFunder = 882177 #Cost_research_funder =  882177
durationOfResearch = 3  # Time_research = 3

MCD_t1 = 0   # MCD_t <- c(0) # remember MCD is a relative input - if two treatmetns => just one MCD
MCD_t2 = NA
MCD_t3 = NA

utilisation_t0 = # check these sum to 1. 
utilisation_t1
utilisation_t2
utilisation_t3






BinaryOutcomeFunction.v.0.1 <- function(numberOfTreatments, P_t0,
                                        mu_t1, variance_t1, dist_t1, direction_t1,
                                        mu_t2, variance_t2, dist_t2, direction_t2,
                                        mu_t3, variance_t3, dist_t3, direction_t3,
                                        
                                        Benefit,MCD_t , Incidence, 
                                        Time_info ,Utilisation_t,D_rate ,Time_research ,
                                        Cost_research_funder){
  
  
  # simulate probabilities of the event
  P_t <- simProbOfOutcomeMatrixBinary(numberOfTreatments, P_t0,
                               mu_t1, variance_t1, dist_t1, direction_t1,
                               mu_t2, variance_t2, dist_t2, direction_t2,
                               mu_t3, variance_t3, dist_t3, direction_t3)
  
  # create economic model from probability of event
  
  INB_Event <- ifelse(Benefit==1, 1, -1)
  
  nt <- dim(P_t)[2] # the number of treatment implied by the input matrix
  nsim <- dim(P_t)[1]
  
  #paste("NB_t", )
  #paste0("NB_t", "1", "_U")
  
  
  #NB_t0_U <- P_t0_U*INB_Event
  #NB_t1_U <- (P_t1_U + MCD_t1)*INB_Event 
  #NB_t2_U <- (P_t2_U + MCD_t2)*INB_Event 
  
  NB_t_U <- P_t*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
  
  addMCD_t <- c(0, MCD_t)   # add the MCD to each column in the vector to convert to net benefit
  NB_t_U <- NB_t_U + rep(addMCD_t, each = nsim)
  
  # each column now represents simulations of the NB of each treatment
  
  # pre trial expected NB of outcomes - use these variables later in calculation!
  #E_NB_t0_U <- mean(NB_t0_U) # 
  #E_NB_t1_U <- mean(NB_t1_U) # 
  #E_NB_t2_U <- mean(NB_t2_U) # 
  
  
  
  # trial inputs
  ########################
  
  # expected outcome with each treatment (uninformed prior)
  ENB_t_U <- apply(NB_t_U, 2, mean)
  
  # Best outcome with uninformed prior 
  NB_EVTCI_U = max(ENB_t_U)
  Optimal_t <- paste0("t_" ,which(ENB_t_U == max(ENB_t_U)) - 1) # tells you which treatment is best
  
  # EVTPI with uninformed prior and natural outcome
  #EVTPI_U  <- mean(apply(INB_t_U , 1, max))
  NB_VTPI_U <- apply(NB_t_U, 1, max) #so I can check convergence
  
  
  NB_EVTPI_U <- mean(NB_VTPI_U)
  NB_EVPI_U <-  NB_EVTPI_U - NB_EVTCI_U
  
  # probability each treatment has highest NB - provides vector of probabilities
  # for the column of simulated NBs for each treatment (x)
  # take the sum of the number of times that that treatment is the maximum NB
  # divide by the number of sumulations to get the probability
  Probability_t_is_max <- apply(NB_t_U, 2, function(x) sum(x==NB_VTPI_U))/nsim
  
  #############################################
  # population 
  
  Popoutputs <- verybasicPop(Incidence, D_rate, Time_research, Time_info)
  
  Pop_during_research <- Popoutputs$Pop_during_research 
  Pop_after_research <- Popoutputs$Pop_after_research 
  Pop_total <- Popoutputs$Pop_total
  
  
  ########### BASIC TRIAL ANALYSIS ################################################
  # 
  
  # YEARLY OUTCOMES #
  Value_of_trial_per_year <- NB_EVTPI_U*Incidence - NB_EVTCI_U*Incidence
  Value_of_implementation_per_year <- Incidence*NB_EVTCI_U - sum(ENB_t_U*Utilisation_t*Incidence)
  
  # histogram of effects per year
  #               # NB per simulation with max(ENB_t_U) - max NB per simulation
  #                 # best treament with current evidence - max NB per simulation
  NB_loss_maxt <- NB_t_U[,which(ENB_t_U == max(ENB_t_U))] - NB_VTPI_U
  Hist_value_of_trial_per_year <- hist(-NB_loss_maxt*Incidence)
  # convert to probability plot, not density
  Hist_value_of_trial_per_year$density = Hist_value_of_trial_per_year$counts/sum(Hist_value_of_trial_per_year$counts)*100
  plot(Hist_value_of_trial_per_year,freq=FALSE,
       main = "Consequences of uncertainty (per year)",
       xlab = "Primary outcomes",
       ylab = "Probability (%)")
  
  
  # FULL TIME OUTCOMES #
  
  ## Cell_A : net benefit of current situation with current utilisation
  # take the weighted average of the expected NB for each treatment scaled up to full population
  Cell_A <- sum(ENB_t_U*Utilisation_t*Pop_total)
  
  
  ## Cell_C : maximum Net benfit of implemetation (Early access - approval)  
  Cell_C <- Pop_total*NB_EVTCI_U
  NB_maxt_U <- Cell_C  # AKA nb_maxt_U
  
  ## Cell_D : maximum Net benfit of information (delay access for information)
  # "instant trial with perfect information"
  # Pure definition of Cell D
  Cell_D <- NB_EVTPI_U*Pop_total
  
  # assume perfect and instant implementation/information
  Max_value_of_implementation <- Cell_C - Cell_A # max value of early access
  Max_value_of_research <- Cell_D - Cell_C 
  
  

  
  # calculating the benefits of research (under differnt assumptions)
  ########################################
  # perfect info and perfect implementation (includes that it is instant)
  NB_instant_research_perfect_info_imp <- Cell_D

  # cu = current utilisation. NOT instant trial - while trial is running just keep whatever treatment 
  # utilisation is theere at the start of the trial
  # Below is the same as OIR if the use of the new treatment is restricted
  NB_cu_perfect_info_imp <- sum(ENB_t_U*Utilisation_t*Pop_during_research) + Pop_after_research*NB_EVTPI_U 

  # maxt = use the best treatmet according to current NB. NOT instant trial - 
  # instantly and perfectly implement best treatment while trial is running 
  # below is the same as AWR if the new treatment is the best with current information
  NB_maxt_perfect_info_imp <- Pop_during_research*NB_EVTCI_U + Pop_after_research*NB_EVTPI_U

  # this is the pure information value under different types of research and implementation assumptions
  Value_of_instant_research_perfect_info_imp <- NB_instant_research_perfect_info_imp - Cell_C
  Value_of_instant_research_perfect_info_stat_sig <- NB_instant_research_perfect_info_stat_sig - Cell_C
  Value_of_cu_perfect_info_imp <- NB_cu_perfect_info_imp - Cell_C
  Value_of_cu_perfect_info_stat_sig <- NB_cu_perfect_info_stat_sig - Cell_C
  Value_of_maxt_perfect_info_imp <- NB_maxt_perfect_info_imp - Cell_C
  Value_of_maxt_perfect_info_stat_sig <- NB_maxt_perfect_info_stat_sig - Cell_C
  
  
  # ICER of research relative to early access (assumed to be costless to the agency)
  # all other costs assumed to be captured by the MCD
  ICER_instant_research_perfect_info_imp <- Cost_research_funder/Value_of_instant_research_perfect_info_imp
  ICER_instant_research_perfect_info_stat_sig <- Cost_research_funder/Value_of_instant_research_perfect_info_stat_sig
  ICER_cu_perfect_info_imp <- Cost_research_funder/Value_of_cu_perfect_info_imp
  ICER_cu_perfect_info_stat_sig <- Cost_research_funder/Value_of_cu_perfect_info_stat_sig
  ICER_maxt_perfect_info_imp <- Cost_research_funder/Value_of_maxt_perfect_info_imp
  ICER_maxt_perfect_info_stat_sig <- Cost_research_funder/Value_of_maxt_perfect_info_stat_sig
  
  
  
  
  
  #Total_NB_research <- Cell_D -  Cell_A
  
  # % of total value which is implementation value
  #Perc_implementation_NB <- ( Cell_C -  Cell_A)/( Cell_D -  Cell_A)*100
  # % of total value which is pure information value
  #Perc_information_NB <- ( Cell_D -  Cell_C)/( Cell_D -  Cell_A)*100
  # max value of implementation (early access) : Cell_C - Cell_A
  # max value of research : Cell_D - Cell_A
  
  
  
  VOIoutputs <- list(
    Optimal_t = Optimal_t,
    Value_of_trial_per_year = Value_of_trial_per_year,
    Value_of_implementation_per_year = Value_of_implementation_per_year,
    Probability_t_is_max = Probability_t_is_max,
    Cell_A = Cell_A,
    Cell_C = Cell_C,
    Cell_D = Cell_D,
    Max_value_of_implementation = Max_value_of_implementation,
    Max_value_of_research = Max_value_of_research,
    NB_instant_research_perfect_info_imp =NB_instant_research_perfect_info_imp,
    
    NB_instant_research_perfect_info_stat_sig =NB_instant_research_perfect_info_stat_sig,
    
    NB_cu_perfect_info_imp =NB_cu_perfect_info_imp,
    NB_cu_perfect_info_stat_sig =NB_cu_perfect_info_stat_sig,
    
    NB_maxt_perfect_info_imp =NB_maxt_perfect_info_imp,
    NB_maxt_perfect_info_stat_sig =NB_maxt_perfect_info_stat_sig,
    
    Value_of_instant_research_perfect_info_imp =Value_of_instant_research_perfect_info_imp,
    Value_of_instant_research_perfect_info_stat_sig =Value_of_instant_research_perfect_info_stat_sig,
    Value_of_cu_perfect_info_imp =Value_of_cu_perfect_info_imp,
    Value_of_cu_perfect_info_stat_sig =Value_of_cu_perfect_info_stat_sig,
    Value_of_maxt_perfect_info_imp =Value_of_maxt_perfect_info_imp,
    Value_of_maxt_perfect_info_stat_sig =Value_of_maxt_perfect_info_stat_sig,
    
    ICER_instant_research_perfect_info_imp =ICER_instant_research_perfect_info_imp,
    ICER_instant_research_perfect_info_stat_sig =ICER_instant_research_perfect_info_stat_sig,
    ICER_cu_perfect_info_imp =ICER_cu_perfect_info_imp,
    ICER_cu_perfect_info_stat_sig =ICER_cu_perfect_info_stat_sig,
    ICER_maxt_perfect_info_imp =ICER_maxt_perfect_info_imp,
    ICER_maxt_perfect_info_stat_sig =ICER_maxt_perfect_info_stat_sig
    
    
  )
  return(VOIoutputs)
  
}

