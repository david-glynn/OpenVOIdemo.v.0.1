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
simProbOfOutcomeNormBinary <- function(P_t0, mu, variance){

  Odds_t0 <- P_t0 / (1 - P_t0)
  LO_t0 <- log(Odds_t0)
  LOR_tn <- rnorm(length(P_t0), mu, sqrt(variance)) # simulate normal log odds ratio
  LO_tn <- LO_t0 + LOR_tn # combine baseline and relative effect and convert back to probability
  Odds_tn <- exp(LO_tn)
  Odds_tn / (Odds_tn + 1) # output P_tn a vector of probabilities
}

# test data
# direction <- "alwaysPositive" # takes value "alwaysPositive" or "alwaysNegative" 
  


# function to simulate the probability of the event with a particular treatment
# for a HALF normal distribution on relative effect
simProbOfOutcomeHalfNormBinary <- function(P_t0, direction, variance){
  
  if(is.na(direction)){P_tn <- NA} else {  # check that there is a value for direction
    
  Odds_t0 <- P_t0 / (1 - P_t0)
  LO_t0 <- log(Odds_t0)
  if (direction == "alwaysPositive"){
    LOR_tn <- -rhalfnorm(length(P_t0), theta =  sd2theta(sqrt(variance)) ) # simulate normal log odds ratio
    # draws from a negative halfnormal
  } else {
    LOR_tn <- rhalfnorm(length(P_t0), theta =  sd2theta(sqrt(variance)) ) # simulate normal log odds ratio
    # draws from a positive halfnormal
  }
  LO_tn <- LO_t0 + LOR_tn # combine baseline and relative effect and convert back to probability
  Odds_tn <- exp(LO_tn)
  Odds_tn / (Odds_tn + 1) # output P_tn a vector of probabilities
  
  }

}


# test data
#numberOfTreatments <- 4
#P_t0 <- rep(0.9, 10)
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
 
# master function which uses the above functions to create the P_t0 matrix
# requires simProbOfOutcomeNormBinary and simProbOfOutcomeHalfNormBinary

simProbOfOutcomeMatrixBinary <- function(numberOfTreatments, P_t0,
                                   mu_t1, variance_t1, dist_t1, direction_t1,
                                   mu_t2, variance_t2, dist_t2, direction_t2,
                                   mu_t3, variance_t3, dist_t3, direction_t3
                                   ){
  
  # simulate the probabilities for t1
  P_t1 <- if (dist_t1 == "norm") {
    simProbOfOutcomeNormBinary(P_t0, mu_t1, variance_t1)
  } else {
    simProbOfOutcomeHalfNormBinary(P_t0, direction_t1, variance_t1)
  }
  
  # simulate the probabilities for t1
  P_t2 <- if(numberOfTreatments <= 2 ) { # if there is only 2 treatments then this is given a vector of NAs
    rep(NA, length(P_t0))
  } else {
    
    if (dist_t2 == "norm") {
      simProbOfOutcomeNormBinary(P_t0, mu_t2, variance_t2)
    } else {
      simProbOfOutcomeHalfNormBinary(P_t0, direction_t2, variance_t2)
    }
    
  }
    
  
  P_t3 <- if(numberOfTreatments <= 3 ) { # if there is only 2 treatments then this is given a vector of NAs
    rep(NA, length(P_t0))
  } else {
    
    if (dist_t3 == "norm") {
      simProbOfOutcomeNormBinary(P_t0, mu_t3, variance_t3)
    } else {
      simProbOfOutcomeHalfNormBinary(P_t0, direction_t3, variance_t3)
    }
    
  }
  
  # add all vectors (P_t0 , P_t1..) to the matrix P_t
  # and return this
  P_t <- matrix(c(P_t0, P_t1, P_t2, P_t3), ncol = 4)

  P_t
  
}

# test simulation
#simProbOfOutcomeMatrixBinary (numberOfTreatments = 3, P_t0 = rep(0.1, 10),
#                        mu_t1 = 0, variance_t1 = 0.1, dist_t1 = "norm",  direction_t1 = "alwaysPositive",
#                        mu_t2 = 0.2, variance_t2 = 0.1, dist_t2 = "halfNorm", direction_t2 = "alwaysPositive",
#                        mu_t3 = NA, variance_t3 = NA, dist_t3 = "halfNorm", direction_t3 = NA
#                        )




# basic population function - calculates the population numbers required in the model

verybasicPop <- function(Incidence, D_rate, Time_research, Time_info){
  
  #                                        time end                time start  
  Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
  Pop_during_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
  Pop_after_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))
  
  output <- list(Pop_total = Pop_total,
                 Pop_during_research = Pop_during_research,
                 Pop_after_research = Pop_after_research)
  
  return(output)
}
