#######################
# potential server side bugs: 
# if the app is open on a number of windows on the browser (i.e. if it has not been "stopped" in a while)
# then Binary/Survival Feas NetHealth models crash. Some problem with trying to find popDurationResearch 
# 
#


# must load the required functions! SupplementaryFunctions.R

library(shiny)

########################################################################################
# load up required functions

library(scales) # required to format tables in renderTable
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
source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/SupplementaryFunctionsFeas.R", local = TRUE)
source("C:/Users/David/Desktop/CHE home working/ShinyApps/OpenVOIdemo.v.0.1/ShinyFiles/master.R", local = TRUE)


# relative paths for publishing in shinyapps.io
#source("BinaryOutcomeFunction.R", local = TRUE)
#source("BinaryQALYFunction.R", local = TRUE)
#source("SupplementaryFunctions.R", local = TRUE)
#source("ContinuousOutcomeFunction.R", local = TRUE)
#source("ContinuousQALYFunction.R", local = TRUE)
#source("SurvivalOutcomeFunction.R", local = TRUE)
#source("SurvivalQALYFunction.R", local = TRUE)
#source("SupplementaryFunctionsFeas.R", local = TRUE)
#source("master.R", local = TRUE)




# finish loading required functions 
########################################################################################



shinyServer(function(input, output) {
  
  VOIResults <- reactiveValues()
  
  ##########################
  # ACTION BUTTON 
  ##########################
  observeEvent(input$run, {
    
      # a list which holds the results of the appropriate analysis
      resultsHolder <- reactive({
        # the master function takes all inputs, runs the appropriate model and returns a list of the results
        master(
               # type of analysis 
               typeOfEndpoint = input$typeOfEndpoint,
               typeOfOutcome= input$typeOfOutcome,
               tCostsDependOnEvent= input$tCostsDependOnEvent,
               numberOfTreatments= input$numberOfTreatments,
               typeOfResearch= input$typeOfResearch,
               MCsims= input$MCsims,
               # report writing inputs
               nameOf_t1= input$nameOf_t1,
               nameOf_t2= input$nameOf_t2,
               nameOf_t3= input$nameOf_t3,
               nameOf_t4= input$nameOf_t4,
               nameOfOutcome= input$nameOfOutcome,
               currencySymbol= input$currencySymbol,
               # basic health system info
               incidence= input$incidence,
               timeInformation= input$timeInformation,
               discountRate= input$discountRate,
               utilisation_t1= input$utilisation_t1,
               utilisation_t2= input$utilisation_t2,
               utilisation_t3= input$utilisation_t3,
               utilisation_t4= input$utilisation_t4,
               MCD_t2= input$MCD_t2,
               MCD_t3= input$MCD_t3,
               MCD_t4= input$MCD_t4,
               # epidemiology: binary + generic
               P_t1= input$P_t1,
               dist_t2= input$dist_t2,
               mu_t2= input$mu_t2,
               variance_t2= input$variance_t2,
               direction_t2= input$direction_t2,
               dist_t3= input$dist_t3,
               mu_t3= input$mu_t3,
               variance_t3= input$variance_t3,
               direction_t3= input$direction_t3,
               dist_t4= input$dist_t4,
               mu_t4= input$mu_t4,
               variance_t4= input$variance_t4,
               direction_t4= input$direction_t4,
               # epidemiology: survival
               survivalDist= input$survivalDist,
               scaleParameter_t1= input$scaleParameter_t1,
               shapeParameter_t1= input$shapeParameter_t1,
               # trial info: RCT
               durationOfResearch= input$durationOfResearch,
               costResearchFunder= input$costResearchFunder,
               costHealthSystem= input$costHealthSystem,
               # trial info: feasibility
               probabilityOfDefinitiveResearch= input$probabilityOfDefinitiveResearch,
               durationOfResearchFeas= input$durationOfResearchFeas,
               durationOfResearchDefinitive= input$durationOfResearchDefinitive,
               costResearchFunderFeas = input$costResearchFunderFeas,
               costResearchFunderDefinitive= input$costResearchFunderDefinitive,
               costHealthSystemFeas= input$costHealthSystemFeas,
               costHealthSystemDefinitive= input$costHealthSystemDefinitive,
               # cost and QALY inputs
               k= input$k,
               INBBinaryEvent= input$INBBinaryEvent,
               INBContinEvent= input$INBContinEvent,
               INBSurvivalEndpoint= input$INBSurvivalEndpoint,
               cost_t1= input$cost_t1,
               costEvent_t1= input$costEvent_t1,
               costNotEvent_t1= input$costNotEvent_t1,
               cost_t2= input$cost_t2,
               costEvent_t2= input$costEvent_t2,
               costNotEvent_t2= input$costNotEvent_t2,
               cost_t3= input$cost_t3,
               costEvent_t3= input$costEvent_t3,
               costNotEvent_t3= input$costNotEvent_t3,
               cost_t4= input$cost_t4,
               costEvent_t4= input$costEvent_t4,
               costNotEvent_t4= input$costNotEvent_t4
               
              )
        })
    
    
    
    # assign results for all models
    VOIResults$optimalTreatment <- resultsHolder()$optimalTreatment
    VOIResults$probTreatment1isMax <- resultsHolder()$probTreatment1isMax
    VOIResults$probTreatment2isMax <- resultsHolder()$probTreatment2isMax
    VOIResults$probTreatment3isMax <- resultsHolder()$probTreatment3isMax
    VOIResults$probTreatment4isMax <- resultsHolder()$probTreatment4isMax
    VOIResults$expectedOutcomesPerYearoptimalTreatment <- resultsHolder()$expectedOutcomesPerYearoptimalTreatment
    VOIResults$implementationValueExists <- resultsHolder()$ implementationValueExists            # new output
    VOIResults$uncertaintyInCurrentEvidenceExists <- resultsHolder()$uncertaintyInCurrentEvidenceExists
    VOIResults$popDuringResearch <- resultsHolder()$popDuringResearch
    VOIResults$popAfterResearch <- resultsHolder()$popAfterResearch
    VOIResults$popTotal <- resultsHolder()$popTotal
    VOIResults$listForhistVOIYear <- resultsHolder()$listForhistVOIYear
    VOIResults$valueOfResearchPerYear <- resultsHolder()$valueOfResearchPerYear
    VOIResults$valueOfImplementationPerYear <- resultsHolder()$valueOfImplementationPerYear
    VOIResults$tableEventsPerYearDF <- resultsHolder()$tableEventsPerYearDF                        # new
    VOIResults$tableProbabilityMaxDF <- resultsHolder()$tableProbabilityMaxDF
    VOIResults$tableTreatmentCostsDF <- resultsHolder()$tableTreatmentCostsDF
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
    VOIResults$absoluteExpectedHealthOutcomesFromResearchProject <- resultsHolder()$absoluteExpectedHealthOutcomesFromResearchProject
    # additional feasibility outputs
    VOIResults$popDuringFeasResearch <- resultsHolder()$popDuringFeasResearch               # unique Feasibility output
    VOIResults$popDuringDefinitiveResearch <- resultsHolder()$popDuringDefinitiveResearch    # unique Feasibility output
    VOIResults$popAfterDefinitiveResearch <- resultsHolder()$popAfterDefinitiveResearch     # unique Feasibility output
    VOIResults$expectedCostResearchFunder <- resultsHolder()$expectedCostResearchFunder                 # unique
    VOIResults$valueOfCertainResearchWithPerfectImplementation <- resultsHolder()$valueOfCertainResearchWithPerfectImplementation # unique Feasibility output

    
    
  }) # end observe event expression
  
  
  
  ####################################################
  # render INPUT and OUTPUT objects and pass to output list
  ####################################################
  

  # Create conditional text segments for results section
  ###########################
  
  # Headline results and overview
  ########
  
  output$introduceResearch <- renderText({
    paste("This proposal is for a",input$numberOfTreatments, "arm", 
          ifelse(input$typeOfResearch == "RCT",
                 "randomised controlled trial.",
                 "feasibility study."),
          "The primary endpoint in the trial is", input$nameOfOutcome, "."
          ) # end paste
  })
  
  # ICER in primary natural outcome for RCT and feasibility study
  output$ICERresult <- renderText({
   paste("Considering the uncertainty in the primary endpoint",
         ifelse(input$typeOfResearch == "feasibility",
                paste("and a", input$probabilityOfDefinitiveResearch, "chance of the feasibility study leading to a definitive trial,"),
                ""),
         "the value of the research is calculated to be approximately", VOIResults$ICER_ResearchWithPerfectImplementation,
         "per", input$nameOfOutcome, ifelse(input$typeOfOutcome != "harm",
                                            "gained.",
                                            "avoided."),
         "This means that the research fundeder must spend",VOIResults$ICER_ResearchWithPerfectImplementation,
         "to", ifelse(input$typeOfOutcome != "harm", "gain", "avoid"), "one", input$nameOfOutcome, ".",
         "As the research funder has limited resources, whether this represents good value for money depends on how this compares to other proposals competing for funding.")
  })
  
  
  
  # text for general discussion about current information (common accross all models and endpoints?)
  output$resultsCurrenInformation <- renderText({
    paste("From the table above",VOIResults$optimalTreatment, "is favoured by the evidence with", 
          VOIResults$expectedOutcomesPerYearoptimalTreatment, input$nameOfOutcome, "'s expected per year.",
          ifelse(VOIResults$implementationValueExists == TRUE, 
                 # text if there is implementation value
                 paste(" Because utilisation of",VOIResults$optimalTreatment , " is not 100%, outcomes can be improved by encouraging the use of", VOIResults$optimalTreatment,
                 ". The benefits of switching practice to",VOIResults$optimalTreatment , 
                 " are estimated to be",VOIResults$valueOfImplementationPerYear,input$nameOfOutcome ,"'s per year."),
                 # text if there is NOT implementation value
                 paste(" Because the utilisation of", VOIResults$optimalTreatment , 
                       " is 100%, outcomes can not be improved by encouraging the use of", VOIResults$optimalTreatment ,".")))
  })
  
  # text for discussion about value of research (common accross all models and endpoints?)
  output$resultsValueOfResearch <- renderText({
    ifelse(VOIResults$maxvalueOfResearch > 0,
           # text if there is value in the research
           paste("value in research..."),
           # text if NO value in research
           paste("no value in research..."))
  })
  
  
  # Raw input and output objects
  #########################
  
  # input objects
  output$nameOf_t1 <- renderText({
    paste("The name of treatment 1 is", input$nameOf_t1)
  })
  output$nameOf_t2 <- renderText({input$nameOf_t2})
  output$nameOf_t3 <- renderText({input$nameOf_t3})
  output$nameOf_t4 <- renderText({input$nameOf_t4})
  output$nameOfOutcome <- renderText({input$nameOfOutcome})
  
  # output objects
  output$optimalTreatment <- renderText({VOIResults$optimalTreatment})
  output$expectedOutcomesPerYearoptimalTreatment <- renderText({VOIResults$expectedOutcomesPerYearoptimalTreatment})
  output$implementationValueExists <- renderText({VOIResults$implementationValueExists})            # new output
  output$uncertaintyInCurrentEvidenceExists <- renderText({VOIResults$uncertaintyInCurrentEvidenceExists})
  #output$probTreatment1isMax <- renderText({VOIResults$probTreatment1isMax })
  #output$probTreatment2isMax <- renderText({VOIResults$probTreatment2isMax })
  #output$probTreatment3isMax <- renderText({VOIResults$probTreatment3isMax })
  #output$probTreatment4isMax <- renderText({VOIResults$probTreatment4isMax})
  output$popDuringResearch <- renderText({VOIResults$popDuringResearch})
  output$popAfterResearch <- renderText({VOIResults$popAfterResearch})
  output$popTotal <- renderText({VOIResults$popTotal })
  output$popDuringFeasResearch <- renderText({VOIResults$popDuringFeasResearch})       # feas outputs
  output$popDuringDefinitiveResearch <- renderText({VOIResults$popDuringDefinitiveResearch})       # feas outputs
  output$popAfterDefinitiveResearch <- renderText({VOIResults$popAfterDefinitiveResearch})        # feas outputs
  output$histVOIYear <- renderPlot({plot(VOIResults$listForhistVOIYear, freq = FALSE,
                                         main = "Consequences of uncertainty (per year)",
                                         xlab = "Primary outcomes",
                                         ylab = "Probability (%)")})
  output$valueOfResearchPerYear <- renderText({paste("value of research per year is",  VOIResults$valueOfResearchPerYear)})
  output$valueOfImplementationPerYear <- renderText({paste("value of implementation per year is", VOIResults$valueOfImplementationPerYear)})
  output$tableEventsPerYear <- renderTable({VOIResults$tableEventsPerYearDF}, include.rownames = FALSE)
  output$tableProbabilityMax <- renderTable({VOIResults$tableProbabilityMaxDF}, include.rownames = FALSE)
  output$tableTreatmentCosts <- renderTable({VOIResults$tableTreatmentCostsDF}, include.rownames = FALSE)
  #output$Cell_A <- renderText({VOIResults$Cell_A})
  #output$Cell_C <- renderText({VOIResults$Cell_C})
  #output$Cell_D <- renderText({VOIResults$Cell_D})
  output$maxvalueOfImplementation <- renderText({VOIResults$maxvalueOfImplementation})
  output$maxvalueOfResearch <- renderText({VOIResults$maxvalueOfResearch})
  output$healthOpportunityCostsOfResearch <-   renderText({VOIResults$healthOpportunityCostsOfResearch})
  output$expectedCostResearchFunder <-   renderText({paste("expected costs to research funder" ,VOIResults$expectedCostResearchFunder)})
  output$valueOfResearchWithCurrentImplementation <- renderText({paste("value of research with current implementation ",VOIResults$valueOfResearchWithCurrentImplementation)})
  output$valueOfResearchWithPerfectImplementation <- renderText({paste("Value of research with perfect implementation", VOIResults$valueOfResearchWithPerfectImplementation)})
  output$valueOfCertainResearchWithPerfectImplementation <- renderText({paste("value of research with certain definitive trial", VOIResults$valueOfCertainResearchWithPerfectImplementation)})
  output$ICER_ResearchWithCurrentImplementation <- renderText({paste("ICER with current info is",VOIResults$ICER_ResearchWithCurrentImplementation)})
  output$ICER_ResearchWithPerfectImplementation <- renderText({paste("ICER with perfect info is",VOIResults$ICER_ResearchWithPerfectImplementation)})
  output$valuePer15KResearchSpend <- renderText({paste("value per 15K research spend is",VOIResults$valuePer15KResearchSpend)})
  output$absoluteExpectedHealthOutcomesFromResearchProject <- renderText({paste("absolute expected outcomes from research project",VOIResults$absoluteExpectedHealthOutcomesFromResearchProject)})
  
  output$costResearchFunderFeas <- renderText({paste("cost funder feasibility ", input$costResearchFunderFeas)})
  output$costResearchFunderDefinitive <- renderText({paste("cost funder definitive", input$costResearchFunderDefinitive)})
  output$probabilityOfDefinitiveResearch <- renderText({paste("prob of definitive research ", input$ProbabilityOfDefinitiveResearch)})
  #output$test1 <- renderText({VOIResults$test1})
  #output$test2 <- renderText({VOIResults$test2})
  #output$test3 <- renderText({VOIResults$test3})
  
  
})



