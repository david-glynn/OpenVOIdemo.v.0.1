#
# uses functions from EpiInputFunctions.R
# probCI() exactVectorNormal() so far
# 
# wrapper functions which take inputs and create approprate functions
# one for:
# 1) baseline probability
# 2) distribution of relative effect inputs
# 3) probability for each intervention
# 4) all probabilities with current evidence
# 5) .. continuous and survival outcomes

# BASELINE PROBABILITY plotting
###########################

# input: all inputs relevant to plotting the baseline probability
# output: plot (if no input: outputs an appropriate blank plot)

# takes all inputs and decides which function to run
# usage: output$baselinePlot <- renderPlot({ baselinePlot(input$...) })

# improvements: take away y axis ticks and label? 
# improve resolution of graph 

# test data
#baselineProbExpression <- "natural" #  # "events" #   NA # if nothing has been entered yet
#sliderBaselineProb <- c(0.80, 0.90) # if "natural" expression (assume 95% CI)
#eventsBaselineProb <- 10 # if "events"
#atRiskBaselineProb <- 30 # if "events"



baselinePlot <- function(baselineProbExpression, 
                         sliderBaselineProb,
                         eventsBaselineProb,
                         atRiskBaselineProb){
  
  # if NA, quit function and return blank plot - contains no axis etc..
  if(is.na(baselineProbExpression)){
    return(
      plot(1, type="n",main = "Baseline Probability of outcome", xlab="Probability", ylab="Density", xlim=c(0, 1), ylim=c(0, 10))
    )
  }
  
  # slider input
  if(baselineProbExpression == "natural"){
    
    
    
    if(sliderBaselineProb[1] == sliderBaselineProb[2]){
      # code for baseline estimates with NO uncertainty
      
      plot(1, type="n",main = "Baseline Probability of outcome", xlab="Probability", ylab="Density", xlim=c(0, 1), ylim=c(0, 15))
      lines(x = c(sliderBaselineProb[1], sliderBaselineProb[1]), y = c(0, 15), col= "firebrick", lwd = 2)
      
    } else {
      # code for baseline estimates with WITH uncertainty
      
      # from EpiInputFunctions.R
      probCIOutput <- probCI(sliderBaselineProb[1], sliderBaselineProb[2])
      mu <- probCIOutput$mu # mean on LO scale
      sigma <- probCIOutput$sigma # sigma on LO scale
      mu_prob <- probCIOutput$mu_prob # natural mean
      
      # from EpiInputFunctions.R - takes perfect samples from theoretical density
      LO_vector <- exactVectorNormal(mu, sigma)
      Odds_vector <- exp(LO_vector)
      Prob_vector <- Odds_vector/(1 + Odds_vector)
      
      plot(density(Prob_vector),main = "Baseline Probability of outcome", xlab="Probability", ylab="Density", xlim=c(0, 1), col= "firebrick", lwd = 2)
      points(x = mu_prob, y = 0,col= "firebrick" )
    }
    
  }
  
  # events input
  if(baselineProbExpression == "events"){
    
    # beta parameter for beta distribution
    nonEventsBaselineProb <- atRiskBaselineProb - eventsBaselineProb
    x_axis <- seq(0, 1, length.out = 100)
    y_axis <- dbeta(x_axis, eventsBaselineProb, nonEventsBaselineProb)
    
    plot(x = x_axis, y = y_axis ,type = "l", main = "Baseline Probability of outcome", xlab="Probability", ylab="Density", xlim=c(0, 1), col= "firebrick", lwd = 2)

  }
  
}

# test function - works well
#baselinePlot(baselineProbExpression = NA, # "events", # "natural", #
#             sliderBaselineProb = c(0.01, 0.99),
#             eventsBaselineProb = 1,
#             atRiskBaselineProb = 2)












#######################################################################################

#######################################################################################
# input plotting functions - removed from epiinputfunctions.R
#######################################################################################





# Binary endpoint, baseline probability 2) Exact sampling: from normal distribution 
############
# given mu and sigma (calculated above) creates vector of samples in direct proportion to normal theoretical distribution
# within a user defined range. This vector can be manipulated to convert to probability distribution
# v.quick!

# inputs: mu, sigma
# outputs: exactVector (vector of samples) 

# define function
exactVectorNormal <- function( mu, sigma){
  
  # just need to sample reasonablely close to mean
  lowerBound <- mu - 6*sigma
  upperBound <- mu + 6*sigma
  # vector of points at which samples will be taken
  # lower bound , upper bound , and length.out = resolution of curve
  x_axis <- seq(lowerBound, upperBound, length.out = 1000) # 1000 appears to work well
  
  # gives a vecor of values for each point in x_axis vector which represent how likely they are
  # in the theoretical distribution (normal with mean mu and sigma)
  density_vector <- dnorm(x_axis, mu, sigma)
  
  # normalise this vector to give highest value in density_vector the value of 1 (the most likely point)
  normalised_density_vector <- density_vector/(max(density_vector))
  
  # take samples of each point in x_axis proportionate to its frequency in the theoretical distribution
  # try values to see how it works: 1000 samples of most likely point
  exactVector <- rep(x_axis, times = normalised_density_vector*1000) 
  
  return(exactVector )
  
}

# test function
#exactVector <- exactVectorNormal(10, 2.1)
#plot(density(exactVector))




# Binary endpoint: baseline probability: PLOT ALL FUNCTION baseline probability from different methods
##########################################################
# there are 3 methods to input baseline probability
# 1) single value: no uncertainty in baseline
# 2) UCI and LCI for probability: input using slider normalParameters() calculates mu and sigma
# 3) nEvents, nAtRisk: used to draw a beta distribution

# require above functions


# test data
#input <- list()
#input$baselineInput ="confidenceBounds"   # "events" # singleValue
#nEvents = 10
#nAtRisk = 30
#prob_UCI = 0.2
#prob_LCI = 0.01
#P_t1 = 0.3

# input: mu , sigma or alpha, beta, input$baselineInput
# output: plot object

# wrap in a function??

# if(input$baselineInput == "events"){
#   
#   plotBetaEvents(nEvents, nAtRisk)
#   
# } 

# also conditional on the confidence bounds not being equal
# if(input$baselineInput == "confidenceBounds"){
#   
#   normalParameters <-  probCI(prob_UCI, prob_LCI)
#   LO_vector <- exactVectorNormal(normalParameters$mu, normalParameters$sigma)
#   Odds_vector <- exp(LO_vector)
#   Prob_vector <- Odds_vector/(1 + Odds_vector)
#   
#   plot(density(Prob_vector), xlim = c(0, 1))
#   
# }


# or if confidence bounds are equal
# if(input$baselineInput == "singleValue"){
#   
#   plot(1, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 10))
#   abline(v = P_t1)
#   
# }





# Binary endpoint: baseline probability 3) plot function (no randomness)
######################
# used??
# no randomness: based on exact densities from beta distribution
# inputs: nEvents, nAtRisk
# output: plot object
#plotBetaEvents <- function(nEvents, nAtRisk){
#  
#  # beta parameter for beta distribution
#  nNonEvents <- nAtRisk - nEvents
#  
#  x_axis <- seq(0, 1, length.out = 100)
#  y_axis <- dbeta(x_axis, nEvents, nNonEvents)
#  
#  plot <- plot(x = x_axis, y = y_axis, type = "l")
#  return(plot)
#}
# test plot function
#plotBetaEvents(2, 20)








# Binary endpoint: find beta distribtion parameter estimates from vector of probabilitites
##################################################
# supplementary function
# useful to plot inputs neatly by smoothing monte carlo error
# 

# inputs: P_tn (some probability vector)
# outputs: alpha_hat, beta_hat

# test data
#P_tn <- rbeta(10000, 13.5, 10 )

# define function
aproxBetaParams <- function(P_tn){
  
  # stop function if input is an NA
  
  # function finds alpha and beta parameter estimates from data for a beta function
  # optimising function with initial values
  suppressWarnings( # suppress that NaNs are produced - the function appears to work well
    fit_beta <- fitdistr(P_tn,"beta",list(shape1=1,shape2=1)) 
  )
  
  # round the result (hopefully should mean that identical inputs will give identical estimates)
  alpha_hat <- round(fit_beta$estimate[1],1) 
  beta_hat <- round(fit_beta$estimate[2],1)
  
  outputs <- list(alpha_hat = alpha_hat, beta_hat = beta_hat)
  return(outputs)
}

# test function
#aproxBetaParams(P_tn)




# Binary endpoint: individually plot P_t2, P_t3, P_t4 with uncertain baseline and MCD
############################################




# Binary endpoint: comparative plot of probabilities with uncertain baseline
############################################
# requires MASS package
# put all on one diagram 
# fit a beta distribution to 


# function
# input: alpha_hat_t1, beta_hat_t1, alpha_hat_t2, beta_hat_t2, alpha_hat_t3, beta_hat_t3, alpha_hat_t4, beta_hat_t4 
# output: 

# test data 
# P_t <- simProbOfOutcomeMatrixBinary (numberOfTreatments = 3, P_t1 = rep(0.1, 50000),
#                        mu_t2 = 0, variance_t2 = 0.1, dist_t2 = "norm",  direction_t2 = "alwaysPositive",
#                        mu_t3 = 0.2, variance_t3 = 0.1, dist_t3 = "halfNorm", direction_t3 = "alwaysPositive",
#                        mu_t4 = NA, variance_t4 = NA, dist_t4 = "halfNorm", direction_t4 = NA
#                        )
# beta_params_t2 <- aproxBetaParams(P_t[,2])
# beta_params_t3 <- aproxBetaParams(P_t[,3])
# beta_params_t4 <- aproxBetaParams(P_t[,4])


# define function
# <- function(nEvents, nAtRisk, MCsims){


# function finds alpha and beta parameter estimates from data for a beta function
# optimising function with initial values
# fit_beta <- fitdistr(sims,"beta",list(shape1=1,shape2=1)) 
#    alpha_hat <- round(fit_beta$estimate[1],1) 
#    beta_hat <- round(fit_beta$estimate[2],1)
#    x_axis <- seq(0, 1, length.out = 100)
# plot(density(sims))
# lines(x_axis, dbeta(x_axis,alpha_hat, beta_hat ))





# plot user inputs
# use exactVectorNormal with sigma_LRR, mu_LRR to take exact samples from normal on LRR scale
# take exponent of these draws and plot to get smooth RR plot
# include mu_RR, RR_UCI and RR_LCI on this plot

