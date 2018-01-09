# must load the required functions! SupplementaryFunctions.R

library(shiny)



########################################################################################
# load up required functions

library(fdrtool) # required for halfnormal simulations

#C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles
# absolute paths for use in desktop development
source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/BinaryOutcomeFunction.R", local = TRUE)
source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/BinaryQALYFunction.R", local = TRUE)
source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/SupplementaryFunctions.R", local = TRUE)
source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/ContinuousOutcomeFunction.R", local = TRUE)
source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/ContinuousQALYFunction.R", local = TRUE)
source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/SurvivalOutcomeFunction.R", local = TRUE)
source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/SurvivalQALYFunction.R", local = TRUE)


# relative paths for publishing in shinyapps.io
#source("BinaryOutcomeFunction.R", local = TRUE)
#source("BinaryQALYFunction.R", local = TRUE)
#source("SupplementaryFunctions.R", local = TRUE)
#source("ContinuousOutcomeFunction.R", local = TRUE)
#source("ContinuousQALYFunction.R", local = TRUE)
#source("SurvivalOutcomeFunction.R", local = TRUE)
#source("SurvivalQALYFunction.R", local = TRUE)




# finish loading required functions 
########################################################################################



shinyServer(function(input, output) {
  
  VOIResults <- reactiveValues()
  
  ##########################
  # RCT ACTION BUTTON
  ##########################
  observeEvent(input$runRCT, {
    
    # QALY binary RCT
    ##################
    if(input$typeOfOutcome == "netHealth" & input$typeOfEndpoint == "binary")
       { # start code
      
      resultsHolder <- reactive({
        # VOI function taking user inputs and returning results
        # Function for Binary RCT cost and QALY analysis
        BinaryQALYFunction(numberOfTreatments = input$numberOfTreatments , 
                          MCsims = input$MCsims, P_t1 =input$P_t1, INBBinaryEvent = input$INBBinaryEvent,
                          mu_t2=input$mu_t2, variance_t2=input$variance_t2 ,
                          dist_t2=input$dist_t2 , direction_t2= input$direction_t2,
                          mu_t3=input$mu_t3 , variance_t3=input$variance_t3 ,
                          dist_t3=input$dist_t3 , direction_t3=input$direction_t3 ,
                          mu_t4=input$mu_t4 , variance_t4=input$variance_t4 ,
                          dist_t4=input$dist_t4 , direction_t4=input$direction_t4 ,
                          nameOf_t1=input$nameOf_t1 ,nameOf_t2=input$nameOf_t2 ,
                          nameOf_t3=input$nameOf_t3 , nameOf_t4=input$nameOf_t4 ,
                          tCostsDependOnEvent = input$tCostsDependOnEvent,
                          cost_t1 = input$cost_t1, cost_t2 = input$cost_t2, cost_t3= input$cost_t3, cost_t4= input$cost_t4,
                          costEvent_t1 = input$costEvent_t1,costEvent_t2 = input$costEvent_t2,costEvent_t3 = input$costEvent_t3,costEvent_t4 = input$costEvent_t3,
                          costNotEvent_t1 = input$costNotEvent_t1,costNotEvent_t2= input$costNotEvent_t2,costNotEvent_t3=input$costNotEvent_t3,costNotEvent_t4 = input$costNotEvent_t4,
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
                          utilisation_t4=input$utilisation_t4 ,
                          costHealthSystem = input$costHealthSystem,
                          k = input$k)
        
      })}
    
    # QALY contunuous RCT
    ########################
    if(input$typeOfOutcome == "netHealth" & input$typeOfEndpoint == "continuous")
    { # start code 
      
      resultsHolder <- reactive({
        # Function for continuous RCT QALY outcome
        ContinuousQALYFunction(numberOfTreatments = input$numberOfTreatments , 
                                        MCsims = input$MCsims, INBContinEvent = input$INBContinEvent,
                                        mu_t2=input$mu_t2, variance_t2=input$variance_t2 ,
                                        dist_t2=input$dist_t2 , direction_t2= input$direction_t2,
                                        mu_t3=input$mu_t3 , variance_t3=input$variance_t3 ,
                                        dist_t3=input$dist_t3 , direction_t3=input$direction_t3 ,
                                        mu_t4=input$mu_t4 , variance_t4=input$variance_t4 ,
                                        dist_t4=input$dist_t4 , direction_t4=input$direction_t4 ,
                                        nameOf_t1=input$nameOf_t1 ,nameOf_t2=input$nameOf_t2 ,
                                        nameOf_t3=input$nameOf_t3 , nameOf_t4=input$nameOf_t4 ,
                                        cost_t1 = input$cost_t1, cost_t2 = input$cost_t2, cost_t3 = input$cost_t3, cost_t4 = input$cost_t4,
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
                                        utilisation_t4=input$utilisation_t4 ,
                                        costHealthSystem = input$costHealthSystem, k = input$k)
      })}

    
    # QALY survival RCT
    ########################
    if(input$typeOfOutcome == "netHealth" & input$typeOfEndpoint == "survival")
    { # start code 
      
      resultsHolder <- reactive({
        # Function for continuous RCT QALY outcome
        SurvivalQALYFunction(numberOfTreatments = input$numberOfTreatments , 
                             MCsims = input$MCsims, 
                             survivalDist = input$survivalDist,
                             scaleParameter_t1 = input$scaleParameter_t1,
                             shapeParameter_t1 = input$shapeParameter_t1,
                             INBSurvivalEndpoint = input$INBSurvivalEndpoint,
                             mu_t2=input$mu_t2, variance_t2=input$variance_t2 ,
                             dist_t2=input$dist_t2 , direction_t2= input$direction_t2,
                             mu_t3=input$mu_t3 , variance_t3=input$variance_t3 ,
                             dist_t3=input$dist_t3 , direction_t3=input$direction_t3 ,
                             mu_t4=input$mu_t4 , variance_t4=input$variance_t4 ,
                             dist_t4=input$dist_t4 , direction_t4=input$direction_t4 ,
                             nameOf_t1=input$nameOf_t1 ,nameOf_t2=input$nameOf_t2 ,
                             nameOf_t3=input$nameOf_t3 , nameOf_t4=input$nameOf_t4 ,
                             cost_t1 = input$cost_t1, cost_t2 = input$cost_t2, cost_t3 = input$cost_t3, cost_t4 = input$cost_t4,
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
                             utilisation_t4=input$utilisation_t4 ,
                             costHealthSystem = input$costHealthSystem, k = input$k)
      })}
    
    
    # natural RCT binary
    ####################
    if(input$typeOfOutcome != "netHealth" & input$typeOfEndpoint == "binary")
      { # start code 
      
      resultsHolder <- reactive({
        # Function for Binary RCT natural outcome
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
      })}
    
    # contunuous natural RCT
    ########################
    if(input$typeOfOutcome != "netHealth" & input$typeOfEndpoint == "continuous")
    { # start code 
      
      resultsHolder <- reactive({
        # Function for Binary RCT natural outcome
        ContinuousOutcomeFunction.v.0.1(numberOfTreatments = input$numberOfTreatments , 
                                    MCsims = input$MCsims, 
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
      })}
    
    # survival natural RCT
    ########################
    if(input$typeOfOutcome != "netHealth" & input$typeOfEndpoint == "survival")
    { # start code 
      
      resultsHolder <- reactive({
        # Function for Binary RCT natural outcome
        SurvivalOutcomeFunction(numberOfTreatments = input$numberOfTreatments , 
                                        MCsims = input$MCsims, 
                                        survivalDist = input$survivalDist,
                                        scaleParameter_t1 = input$scaleParameter_t1, 
                                        shapeParameter_t1 = input$shapeParameter_t1,
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
      })}
    
    
    # assign results for all RCT models
    VOIResults$optimalTreatment <- resultsHolder()$optimalTreatment
    VOIResults$probTreatment1isMax <- resultsHolder()$probTreatment1isMax
    VOIResults$probTreatment2isMax <- resultsHolder()$probTreatment2isMax
    VOIResults$probTreatment3isMax <- resultsHolder()$probTreatment3isMax
    VOIResults$probTreatment4isMax <- resultsHolder()$probTreatment4isMax
    VOIResults$popDuringResearch <- resultsHolder()$popDuringResearch
    VOIResults$popAfterResearch <- resultsHolder()$popAfterResearch
    VOIResults$PopTotal <- resultsHolder()$PopTotal
    VOIResults$ListForhistVOIYear <- resultsHolder()$ListForhistVOIYear
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
    
  }) # end RCT observe event expression
  
  
  ##########################
  # FEASIBILITY ACTION BUTTON
  ##########################
  observeEvent(input$runFeas, {
    
    #input$typeOfOutcome == "netHealth"
  
  # for QALY Feasibility models
      
      #QALY feasibiliyt models
      
      #input$typeOfEndpoint 
        
        # QALY feasibility Binary model
        
      # natural outcome feasibility models
      
    
  })
  
  ##########################
  # RECONSIDERATION ACTION BUTTON
  ##########################
  observeEvent(input$runRec, {
    #  similar code for: reconsideration of evidence
  })
  
  
  output$nameOf_t1 <- renderText({
    paste("The name of treatment 1 is", input$nameOf_t1)
  })
  output$nameOf_t2 <- renderText({input$nameOf_t2})
  output$nameOf_t3 <- renderText({input$nameOf_t3})
  output$nameOf_t4 <- renderText({input$nameOf_t4})
  output$nameOfOutcome <- renderText({input$nameOfOutcome})
  
  output$histVOIYear <- renderPlot({plot(VOIResults$ListForhistVOIYear, freq = FALSE,
                                         main = "Consequences of uncertainty (per year)",
                                         xlab = "Primary outcomes",
                                         ylab = "Probability (%)")})
  
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



