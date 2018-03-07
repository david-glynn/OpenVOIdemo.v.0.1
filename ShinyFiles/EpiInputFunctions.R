#######################
# epi input functions
#######################

# library(MASS)
library(fdrtool)


##########################################
##########################################
# BINARY endpoint
##########################################
###########################################

########################################
# Binary endpoint: baseline probability
#######################################

# there are 2 methods to input baseline probability
# 2) UCI and LCI for probability: input using slider normalParameters() calculates mu and sigma
# 3) nEvents, nAtRisk: used to draw a beta distribution




# Binary endpoint, baseline probability 2) from LCI and UCI on baseline probability
############################################
# note: this is done as in Excel model by converting probability to odds
# (slighlty modified as linearlity/ normality holds better on the log scale so this is used here)
# ***theoretically possible to simulate negative probability values with this method? - probably not thanks to exponeitation etc
# also possible to do this by treating the LCI and UCI as a 95% interval on a beta distribtion
# and finding the alpha and beta parameters which best fit the LCI and UCI provided by the user
# these parameters are found by choosing alpha and beta to minimise a loss function:
# squared distance from LCI + squared distance from UCI as described in: https://www.johndcook.com/quantiles_parameters.pdf
# loss function in R
# (qbeta(0.025, alpha_hat, beta_hat) - LCI)^2 + (qbeta(0.975, alpha_hat, beta_hat) - UCI)^2
# not fully sure if this works!

# function
# input: prob_UCI and prob_LCI
# output: mu_prob (exact expected probability), P_t1
#  

# assumes normal log odds distribution

# test data (from Excel model)
#prob_UCI = 0.469
#prob_LCI = 0.248

# define function
BaselineProbCI <- function(MCsims, prob_LCI, prob_UCI){
  
  # if there is no uncertainty in the slider input
  if(prob_LCI == prob_UCI){
    
    mu_prob <- prob_LCI
    P_t1 <- rep(mu_prob,MCsims)
    
    outputs <- list(P_t1 = P_t1, mu_prob = mu_prob)
    
  } else {
    
    # convert prob CIs to odds CIs
    Odds_UCI <- prob_UCI/(1 - prob_UCI)
    Odds_LCI <- prob_LCI/(1 - prob_LCI)
    
    # convert to log scale as sigma is symmetrical around the mean on this scale
    LO_UCI <- log(Odds_UCI)
    LO_LCI <- log(Odds_LCI)
    
    sigma <- abs(LO_UCI - LO_LCI)/(2*1.96)
    mu <- LO_LCI + 1.96*sigma # mean on log odds scale
    mu_OR <- exp(mu)
    mu_prob <- mu_OR/(1 + mu_OR)
    
    LO <- rnorm(MCsims, mu, sigma)
    Odds <- exp(LO)
    P_t1 <- Odds/(1 + Odds)
    
    
    outputs <- list(P_t1 = P_t1, mu_prob = mu_prob)
    
  }
  
  
  return(outputs)
  
}

# test function (from Excel model)
#BaselineProbCI(10000, 0.248,0.469)
#BaselineProbCI(10, 0.2, 0.2)

# consistency test
#x <- BaselineProbCI(1009999, 0.248,0.469)
#x$mu_prob
#mean(x$P_t1)










# Binary endpoint: baseline probability 2) from events vs number at risk
############################################

# function
# input: nEvents and nAtRisk
# output: mu_prob (exact expected probability), P_t1
# assumes MCsims sufficient to express the distribution

# test data (from Excel model)
#nEvents = 100
#nAtRisk = 210
#MCsims = 100000

# define function
probEvents <- function(MCsims, nEvents, nAtRisk){
  
  # beta parameter for beta distribution
  nNonEvents <- nAtRisk - nEvents
  mu_prob <- nEvents/(nEvents + nNonEvents)
  
  P_t1 <- rbeta(MCsims,nEvents,nNonEvents )
  outputs <- list(P_t1 = P_t1, mu_prob = mu_prob)
  
  return(outputs)
}

# test function
#probEvents(10, 20, 10000)



















########################################
# Binary endpoint: relative effects
#######################################
# input: P_t1 vector (could be uncertain or single valued but must represent the MCsims in its length)
# output: P_tn

# **** must handle "no uncertainty" from slider range UCI = LCI (vector)
# and from normal slider => single va

# there are 6 methods to input binary relative effects
# and 3 methods to input continuous
# and 2 methods to input survival
# 
# BINARY
# 1) normal dist UCI and LCI for natural odds ratio scale
# 2) normal dist UCI and LCI for natural RR scale
# 3) normal dist UCI and LCI for risk difference
# 4) HalfNormal range odds ratio scale
# 5) HalfNormal range RR scale
# 6) HalfNormal range risk difference
#
# CONTINUOUS
# 7) normal dist mean and standard error for mean difference scale
# 8) normal dist UCI and LCI for mean difference scale
# 9) HalfNormal range for mean difference scale
# 
# SURVIVAL
# 10) normal dist UCI and LCI for natural hazard ratio scale
# 11) HalfNormal range hazard ratio scale


# **** must handle "no uncertainty" from slider range UCI = LCI (vector)
# and from normal slider => single value




# Binary endpoint: relative effects 1) normal dist UCI and LCI for natural odds ratio scale
############################################




# test data
# OR_tn <- c(0.71, 1.18)
# P_t1 <- rbeta(10, 10, 40)

# input: OR_tn (range or single value), P_t1
# output: P_tn
# **** must handle "no uncertainty" from slider range UCI = LCI (vector)
# and from normal slider => single va

# define function
oddsRatioCItoP_tn <- function(OR_tn, P_t1){
  
  if(length(OR_tn) == 1){
  # if single value input: no uncertainty in relative effect
    
    mu <- log(OR_tn)
    LOR_tn_vec <- rep(mu, length(P_t1))
    
    Odds_t1 <- P_t1/(1 - P_t1)
    LO_t1 <- log(Odds_t1)
    LO_tn <- LO_t1 + LOR_tn_vec
    Odds_tn <- exp(LO_tn)
    P_tn <- Odds_tn/(1 + Odds_tn)
    
    return(P_tn)
    
  } else {
  # if range input (handles the case in which there is no difference between LCI and UCI)
    LOR_LCI <- log(OR_tn[1]) 
    LOR_UCI <- log(OR_tn[2])
    sigma <- abs(LOR_UCI - LOR_LCI)/(2*1.96)
    mu <- LOR_LCI + 1.96*sigma # mean on log odds scale
  
    LOR_tn_vec <- rnorm(length(P_t1),mu, sigma)
    
    Odds_t1 <- P_t1/(1 - P_t1)
    LO_t1 <- log(Odds_t1)
    LO_tn <- LO_t1 + LOR_tn_vec
    Odds_tn <- exp(LO_tn)
    P_tn <- Odds_tn/(1 + Odds_tn)
      
    return(P_tn)
  }

}

# test function
# reproduce result from excel: results similar but slightly different, the assumptions here are more appropriate
# x <- BaselineProbCI(10000, 0.248, 0.469)
# P_tn <- oddsRatioCItoP_tn(OR_tn = c(0.71, 1.18), x$P_t1)
# mean(x$P_t1) # 0.379 in excel
# head(x$P_t1)
# mean(P_tn) # 0.364 in excel
# head(P_tn)
# dif <- x$P_t1 - P_tn
# 1 - sum(dif < 0)/10000 # prob of tn being cost effective = 0.7 in excel
# # test single value
# P_t1 <- rbeta(30, 10, 40)
# P_tn <- oddsRatioCItoP_tn(OR_tn = 0.71, P_t1) # appears to work correctly
# dif <- P_t1 - P_tn # correct : should always be lower with no uncertainty
# plot(P_t1, P_tn)


# Binary endpoint: relative effects 2) normal dist UCI and LCI for natural RR scale
############################################
# assumes normal distribution for log(RR) 
# simulate uncertainty in LRR ~ N(mu_LRR, sigma_LRR)
# exp(LRR) = RR
# RR * P_t1 = P_tn

# test data
# RR_tn <- c(0.71, 1.18)
# P_t1 <- rbeta(10, 10, 40)

# input: RR_tn (range or single value), P_t1
# output: P_tn
# **** must handle "no uncertainty" from slider range UCI = LCI (vector)
# and from normal slider => single va

# define function
riskRatioCItoP_tn <- function(RR_tn, P_t1){
  
  if(length(RR_tn) == 1){
    # if single value input: no uncertainty in relative effect
    
    mu_RR <- log(RR_tn)
    LRR_tn_vec <- rep(mu_RR, length(P_t1)) # assume normal distribution of log(RR)
    
    RR_tn_vec <- exp(LRR_tn_vec)
    P_tn <- P_t1*RR_tn_vec
    
    return(P_tn)
    
  } else {
    # if range input (handles the case in which there is no difference between LCI and UCI)
    LRR_LCI <- log(RR_tn[1]) 
    LRR_UCI <- log(RR_tn[2])
    sigma <- abs(LRR_UCI - LRR_LCI)/(2*1.96)
    mu_RR <- LRR_LCI + 1.96*sigma # implied mean on log rr scale
    
    LRR_tn_vec <- rnorm(length(P_t1),mu_RR, sigma)
    
    RR_tn_vec <- exp(LRR_tn_vec)
    P_tn <- P_t1*RR_tn_vec
    
    # replace values greater than 1 or less than zero with legal probabilities
    P_tn[P_tn > 1] <- 1
    P_tn[P_tn < 0] <- 0
    
    return(P_tn)
  }
  
}

# test function
# reproduce result from excel: results similar but slightly different, this uses a rr rather than or
# P_t1 <- BaselineProbCI(10000, 0.248, 0.469)$P_t1
# P_tn <- riskRatioCItoP_tn(c(0.71, 1.18), P_t1)
# mean(P_t1) # 0.379 in excel
# head(P_t1)
# mean(P_tn) # 0.364 in excel
# head(P_tn)
# dif <- P_t1 - P_tn
# 1 - sum(dif < 0)/10000 # prob of tn being cost effective = 0.7 in excel
# # test single value
# P_t1 <- rbeta(30, 10, 40)
# P_tn <- riskRatioCItoP_tn(0.71, P_t1) # appears to work correctly
# dif <- P_t1 - P_tn # correct : should always be lower with no uncertainty
# plot(P_t1, P_tn)






# Binary endpoint: relative effects 3) UCI and LCI for risk difference
############################################
# assume normally distributed

# function
# input: RD_UCI and RD_LCI
# very similar to function above

# input: RD_tn (range or single value), P_t1
# output: P_tn
# **** must handle "no uncertainty" from slider range UCI = LCI (vector)
# and from normal slider => single va
# ** must deal with probabilities > 1 (replace with 1)

# test data
# RD_tn <- c(-0.05, 0.1)
# P_t1 <- rbeta(10, 10, 40)



# define function
riskDifferenceCItoP_tn <- function(RD_tn, P_t1){
  
  if(length(RD_tn) == 1){
    # if single value input: no uncertainty in relative effect
    
    P_tn <- P_t1 + RD_tn
    # remove values greater than 1 or less than zero
    P_tn[P_tn > 1] <- 1
    P_tn[P_tn < 0] <- 0
    
    return(P_tn)
    
  } else {
    # if range input (handles the case in which there is no difference between LCI and UCI)
    RD_LCI <- RD_tn[1]
    RD_UCI <- RD_tn[2]
    sigma <- abs(RD_UCI - RD_LCI)/(2*1.96)
    mu_RD <- RD_LCI + 1.96*sigma # implied mean on rd scale
    
    RD_tn_vec <- rnorm(length(P_t1),mu_RD, sigma)
    
    P_tn <- P_t1 + RD_tn_vec
    
    # replace values greater than 1 or less than zero with legal probabilities
    P_tn[P_tn > 1] <- 1
    P_tn[P_tn < 0] <- 0
    
    return(P_tn)
  }
  
}

# test function - illegal probabilities - pass test
# P_t1 <- BaselineProbCI(10000, 0.98, 0.99)$P_t1
# P_tn <- riskDifferenceCItoP_tn(c(0.3, 0.9), P_t1)
# mean(P_t1) 
# head(P_t1)
# mean(P_tn) 
# head(P_tn)
# # test single value
# P_t1 <- rbeta(30, 10, 40)
# P_tn <- riskDifferenceCItoP_tn(0.1, P_t1) # appears to work correctly
# dif <- P_t1 - P_tn # correct : should always be lower with no uncertainty
# plot(P_t1, P_tn)







# Binary endpoint: relative effects 4) HalfNormal range odds ratio scale
#################################################
# ORHalfNorm_tn <- 2 # upper bound if binaryDist_t2 == alwaysPositive, lower bound if binaryDist_t2 == alwaysNegative,

# input: ORHalfNorm_tn (single value), binaryDist_tn
# output: P_tn


# test data
# ORHalfNorm_tn <- 2
# binaryDist_tn <- "alwaysPositive"
# P_t1 <- rbeta(10, 10, 40)



# define function
oddsRatioHalfNormalToP_tn <- function(ORHalfNorm_tn, binaryDist_tn, P_t1){
    
    # sanity check
    if((binaryDist_tn == "alwaysPositive" & ORHalfNorm_tn < 1) | (binaryDist_tn == "alwaysNegative" & ORHalfNorm_tn > 1)){
      stop("The direction of the half normal and the 95% upper/lower bound are inconsistent")
    }
  
    # if range input (handles the case in which there is no difference between LCI and UCI)
    Odds_t1 <- P_t1/(1 - P_t1)
    LO_t1 <- log(Odds_t1)
    
    # calculate the sd of the zero mean normal dist that corresponds to the halfnormal with parameter theta
    sigma <- abs(log(ORHalfNorm_tn))/(1.96)
    
    if (binaryDist_tn == "alwaysPositive"){
      LOR_tn <- rhalfnorm(length(P_t1), theta =  sd2theta(sigma) ) # simulate normal log odds ratio
      # draws from a positive halfnormal
    } else {
      LOR_tn <- -rhalfnorm(length(P_t1), theta =  sd2theta(sigma) ) # simulate normal log odds ratio
      # draws from a negative halfnormal
    }
    
    LO_tn <- LO_t1 + LOR_tn # combine baseline and relative effect and convert back to probability
    Odds_tn <- exp(LO_tn)
    P_tn <- Odds_tn / (Odds_tn + 1) # output P_tn a vector of probabilities
  
    return(P_tn)
  
}


# test function - face validity - pass test
# P_t1 <- BaselineProbCI(10000, 0.7, 0.8)$P_t1
# P_tn <- oddsRatioHalfNormalToP_tn(0.2, "alwaysPositive", P_t1)
# mean(P_t1)
# head(P_t1)
# mean(P_tn)
# head(P_tn)







# Binary endpoint: relative effects 5) HalfNormal range RR scale
##################################################


# test data
# RRHalfNorm_tn <- 0.1
# binaryDist_tn <- "alwaysPositive"
# P_t1 <- rbeta(10, 10, 40)



# define function
riskRatioHalfNormalToP_tn <- function(RRHalfNorm_tn, binaryDist_tn, P_t1){
  
  # sanity check
  if((binaryDist_tn == "alwaysPositive" & RRHalfNorm_tn < 1) | (binaryDist_tn == "alwaysNegative" & RRHalfNorm_tn > 1)){
    stop("The direction of the half normal and the 95% upper/lower bound are inconsistent")
  }
  
  # if range input (handles the case in which there is no difference between LCI and UCI)
  
  # calculate the sd of the zero mean normal dist that corresponds to the halfnormal with parameter theta
  sigma <- abs(log(RRHalfNorm_tn))/(1.96)
  
  if (binaryDist_tn == "alwaysPositive"){
    LRR_tn <- rhalfnorm(length(P_t1), theta =  sd2theta(sigma) ) # simulate normal log risk ratio
    # draws from a positive halfnormal
  } else {
    LRR_tn <- -rhalfnorm(length(P_t1), theta =  sd2theta(sigma) ) # simulate normal log risk ratio
    # draws from a negative halfnormal
  }
  
  RR_tn <- exp(LRR_tn)
  P_tn <- P_t1*RR_tn
  
  # replace values greater than 1 or less than zero with legal probabilities
  P_tn[P_tn > 1] <- 1
  P_tn[P_tn < 0] <- 0
  
  return(P_tn)
  
}

# test function - face validity - pass test
# P_t1 <- BaselineProbCI(10000, 0.1, 0.8)$P_t1
# P_tn <- riskRatioHalfNormalToP_tn(0.2, "alwaysPositive", P_t1)
# mean(P_t1)
# head(P_t1)
# mean(P_tn)
# head(P_tn)
# plot(density(P_tn))

# Binary endpoint: relative effects 6) HalfNormal range risk difference
########################################################


# test data
# RDHalfNorm_tn <- 0.1
# binaryDist_tn <- "alwaysPositive"
# P_t1 <- rbeta(10, 10, 40)



# define function
riskDifferenceHalfNormalToP_tn <- function(RDHalfNorm_tn, binaryDist_tn, P_t1){
  
  # sanity check
  if((binaryDist_tn == "alwaysPositive" & RDHalfNorm_tn < 0) | (binaryDist_tn == "alwaysNegative" & RDHalfNorm_tn > 0)){
    stop("The direction of the half normal and the 95% upper/lower bound are inconsistent")
  }
  
  # if range input (handles the case in which there is no difference between LCI and UCI)
  
  # calculate the sd of the zero mean normal dist that corresponds to the halfnormal with parameter theta
  sigma <- abs(RDHalfNorm_tn)/(1.96)
  
  if (binaryDist_tn == "alwaysPositive"){
    RD_tn <- rhalfnorm(length(P_t1), theta =  sd2theta(sigma) ) # simulate normal log risk ratio
    # draws from a positive halfnormal
  } else {
    RD_tn <- -rhalfnorm(length(P_t1), theta =  sd2theta(sigma) ) # simulate normal log risk ratio
    # draws from a negative halfnormal
  }
  
  P_tn <- P_t1 + RD_tn
  
  # replace values greater than 1 or less than zero with legal probabilities
  P_tn[P_tn > 1] <- 1
  P_tn[P_tn < 0] <- 0
  
  return(P_tn)
  
}



# test function - face validity - pass test
# P_t1 <- BaselineProbCI(10000, 0.1, 0.8)$P_t1
# P_tn <- riskDifferenceHalfNormalToP_tn(0.2, "alwaysPositive", P_t1)
# mean(P_t1)
# head(P_t1)
# mean(P_tn)
# head(P_tn)
# plot(density(P_tn))






# 7) CONTINUOUS normal dist mean and standard error for mean difference scale
####################################################
# no baseline value


# test data
# continMean_tn <- 1
# continSE_tn <- 0.5


# define function
meanDifferenceMeanSEToDelta_tn <- function(MCsims, continMean_tn, continSE_tn){

    Delta_tn <- rnorm(MCsims, continMean_tn, continSE_tn)
    return(Delta_tn)
}

# test function - face validity
# Delta_tn <- meanDifferenceMeanSEToDelta_tn(111000, 0.2, 1)
# mean(Delta_tn)
# head(Delta_tn)
# plot(density(Delta_tn))



# 8) CONTINUOUS normal dist UCI and LCI for mean difference scale
####################################################

# test data
# MD_tn <-  c(-2, 2)
# MCsims <- 10

# define function
meanDifferenceCIToDelta_tn <- function(MCsims, MD_tn){
    
    # if range input (handles the case in which there is no difference between LCI and UCI)
    MD_LCI <- MD_tn[1]
    MD_UCI <- MD_tn[2]
    sigma <- abs(MD_LCI - MD_UCI)/(2*1.96)
    mu_MD <- MD_LCI + 1.96*sigma # implied mean on rd scale
    
    Delta_tn <- rnorm(MCsims,mu_MD, sigma)
    return(Delta_tn)
}

# test function - 
# Delta_tn <- meanDifferenceCIToDelta_tn(1000, c(-0.2, 2))
# mean(Delta_tn) 
# plot(density(Delta_tn))








# 9) CONTINUOUS HalfNormal range for mean difference scale
####################################################


# define function
meanDifferenceHalfNormalToDelta_tn <- function(MCsims, MDHalfNorm_tn, continDist_tn){
  
  # sanity check
  if((continDist_tn == "alwaysPositive" & MDHalfNorm_tn < 0) | (continDist_tn == "alwaysNegative" & MDHalfNorm_tn > 0)){
    stop("The direction of the half normal and the 95% upper/lower bound are inconsistent")
  }
  
  # if range input (handles the case in which there is no difference between LCI and UCI)
  
  # calculate the sd of the zero mean normal dist that corresponds to the halfnormal with parameter theta
  sigma <- abs(MDHalfNorm_tn)/(1.96)
  
  if (continDist_tn == "alwaysPositive"){
    Delta_tn <- rhalfnorm(MCsims, theta =  sd2theta(sigma) ) # simulate normal log risk ratio
    # draws from a positive halfnormal
  } else {
    Delta_tn <- -rhalfnorm(MCsims, theta =  sd2theta(sigma) ) # simulate normal log risk ratio
    # draws from a negative halfnormal
  }
  
  return(Delta_tn)
  
}



# test function - face validity - pass test
# Delta_tn <- meanDifferenceHalfNormalToDelta_tn(MCsims = 1000, MDHalfNorm_tn = 0.2, continDist_tn = "alwaysPositive")
# plot(density(Delta_tn))




# 10) SURVIVAL normal dist UCI and LCI for natural hazard ratio scale
####################################################
#

# from: http://data.princeton.edu/wws509/notes/c7s1.html
# simulate expected survival duration for t1
# This is the area under the curve of the survival funtion S(t)

# for both models (p101 Briggs green book)
# lnLambda_tn = log(hazard_t1) + log(hazardRatio_tn)
# lnLambda_t1 = log(hazard_t1)
# hazard_t1 = lambda_t1 

# Exponential distribution
# lambda = scale parameter 
# expected survival =  E(T) = 1/lambda ---- from Collett

# Weibull distribution
# from Collett : modelling survival data in medical research p152
# T ~ W(lambda, gamma) 
# lambda = scaleParameter, gamma = shapeParameter
# E(T) = lambda^(-1/gamma)*gamma(1/gamma +1)
# (note the gamma function!)


# test data
# survivalDist_tn <- "alwaysPositive" # "alwaysNegative"
# survivalType <- "exponential" # "weibull"
# lambda_t1 <- 4 #  scaleParameter_t1 <- 4  # used in exponential and weibull
# gamma_t1 <- 1 # shapeParameter_t1 <- 5  # used in weibull
# MCsims <-  1000
# HR_tn <- c(1.9, 2.1)

# define function
hazardRatioCIToExpectedSurvival_tn <- function(survivalType, lambda_t1,gamma_t1,
                                               HR_tn, MCsims){
  
  hazard_t1 <- lambda_t1
  lnHazard_t1 <- log(lambda_t1)
  
  LHR_LCI <- log(HR_tn[1]) 
  LHR_UCI <- log(HR_tn[2])
  sigma <- abs(LHR_UCI - LHR_LCI)/(2*1.96)
  mu <- LHR_LCI + 1.96*sigma # mean on log hazard scale
  lnHazardRatio_tn <- rnorm(MCsims,mu, sigma)
  lnLambda_tn <- lnHazard_t1 + lnHazardRatio_tn
  lambda_tn <- exp(lnLambda_tn)
    
  if(survivalType == "exponential"){
    # for an exponential survial function
    ExpectedSurvival_tn <- 1/lambda_tn
    
    return(ExpectedSurvival_tn)
  }
  
  if(survivalType == "weibull"){
    # for an weibull survial function
    ExpectedSurvival_tn <- lambda_tn^(-1/gamma_t1)*gamma(1/gamma_t1 +1)
      #scaleParameter_tn^(1/shapeParameter_t1)*gamma(1/shapeParameter_t1 + 1) # from Collett
    
    return(ExpectedSurvival_tn)
  }
  
}


# test function - face validity:
# weibull with gamma = 1 = exponential 
# higher hazard ratio => shorter expected survival
# ExpectedSurvival_tn <- hazardRatioCIToExpectedSurvival_tn(survivalType = "exponential",
#                                                           lambda_t1 = 7,
#                                                           gamma_t1 = NA,
#                                                           HR_tn = c(0.9, 1.1), MCsims = 1000)
# mean(ExpectedSurvival_tn) # ExpectedSurvival_t1 = 1/lambda_t1 = 1/7 = 0.143
# plot(density(ExpectedSurvival_tn))
# ExpectedSurvival_tn <- hazardRatioCIToExpectedSurvival_tn(survivalType = "weibull",
#                                                           lambda_t1 = 7,
#                                                           gamma_t1 = 1,
#                                                           HR_tn = c(0.9, 1.1), MCsims = 1000)
# mean(ExpectedSurvival_tn) # for exponential ExpectedSurvival_t1 = 1/lambda_t1 = 0.14
# plot(density(ExpectedSurvival_tn))





# 11) SURVIVAL HALFNormal range hazard ratio scale
####################################################
# same statistics as method above


# test data
# survivalType <- "exponential" # "weibull"
# survivalDist_tn <- "alwaysPositive" # "alwaysNegative"
# lambda_t1 <- 4 #  scaleParameter_t1 <- 4  # used in exponential and weibull
# gamma_t1 <- 1 # shapeParameter_t1 <- 5  # used in weibull
# MCsims <-  1000
# HRHalfNorm_tn <- 1 # single value (bounds depend on whether the half normal is "alwaysPositive" or "alwaysNegative")


# define function
hazardRatioHalfNormalToExpectedSurvival_tn <- function(survivalType, survivalDist_tn, lambda_t1,gamma_t1,
                                               HRHalfNorm_tn, MCsims){
  
  # sanity check
  if((survivalDist_tn == "alwaysPositive" & HRHalfNorm_tn < 1) | (survivalDist_tn == "alwaysNegative" & HRHalfNorm_tn > 1)){
    stop("The direction of the half normal and the 95% upper/lower bound are inconsistent")
  }
  
  hazard_t1 <- lambda_t1
  lnHazard_t1 <- log(lambda_t1)
  
  sigma <- abs(log(HRHalfNorm_tn))/(1.96)
  
  # halfnormal simulation
  if (survivalDist_tn == "alwaysPositive"){
    lnHazardRatio_tn <- rhalfnorm(MCsims, theta =  sd2theta(sigma) ) 
    # draws from a positive halfnormal
  } else {
    lnHazardRatio_tn <- -rhalfnorm(MCsims, theta =  sd2theta(sigma) ) 
    # draws from a negative halfnormal
  }
  
  lnLambda_tn <- lnHazard_t1 + lnHazardRatio_tn
  lambda_tn <- exp(lnLambda_tn)

  if(survivalType == "exponential"){
    # for an exponential survial function
    ExpectedSurvival_tn <- 1/lambda_tn
    
    return(ExpectedSurvival_tn)
  }
  
  if(survivalType == "weibull"){
    # for an weibull survial function
    ExpectedSurvival_tn <- lambda_tn^(-1/gamma_t1)*gamma(1/gamma_t1 +1)
    #scaleParameter_tn^(1/shapeParameter_t1)*gamma(1/shapeParameter_t1 + 1) # from Collett
    
    return(ExpectedSurvival_tn)
  }
  
}


# test function - face validity:
# weibull with gamma = 1 = exponential 
# HRHalfNorm_tn = 1 => exact same result for weibull and exponential
# higher hazard ratio => shorter expected survival
# ExpectedSurvival_tn <- hazardRatioHalfNormalToExpectedSurvival_tn(survivalType = "exponential",
#                                                           survivalDist_tn = "alwaysPositive",
#                                                           lambda_t1 = 7,
#                                                           gamma_t1 = NA,
#                                                           HRHalfNorm_tn = 2, 
#                                                           MCsims = 1000)
# mean(ExpectedSurvival_tn) # ExpectedSurvival_t1 = 1/lambda_t1 = 1/7 = 0.143
# plot(density(ExpectedSurvival_tn))
# ExpectedSurvival_tn <- hazardRatioHalfNormalToExpectedSurvival_tn(survivalType = "weibull",
#                                                           survivalDist_tn = "alwaysPositive",
#                                                           lambda_t1 = 7,
#                                                           gamma_t1 = 1,
#                                                           HRHalfNorm_tn = 1 , 
#                                                           MCsims = 1000)
# mean(ExpectedSurvival_tn) # for exponential ExpectedSurvival_t1 = 1/lambda_t1 = 0.14
# plot(density(ExpectedSurvival_tn))
















