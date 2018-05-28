#######################
# potential server side bugs: 
# if the app is open on a number of windows on the browser (i.e. if it has not been "stopped" in a while)
# then Binary/Survival Feas NetHealth models crash. Some problem with trying to find popDurationResearch 
# 
#
# bug in writing report:
# relevant warning?
# Warning in formatC(ICER_ResearchWithPerfectImplementation, big.mark = ",",  :
# NAs introduced by coercion to integer range


# must load the required functions! SupplementaryFunctions.R

library(shiny)

############################
# load up required functions

library(scales) # required to format tables in renderTable
library(fdrtool) # required for halfnormal simulations
library(MASS) # for use in EpiInputFunctions.R to fit beta distributions to unknown probabilities
library(rmarkdown) # used in generating reports


# WORK new absolute path source files
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/SupplementaryFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/SupplementaryFunctionsFeas.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/master.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/ReconFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/EpiInputFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/PlottingFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/EpiCalcFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/NBCalcFunctions.R", local = TRUE)


#source("W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles/masterExtra.R", local = TRUE)


# HOME new absolute path source files
# source("C:/Users/David/Desktop/Work/R files/Shiny/Tool/ShinyFiles/SupplementaryFunctions.R", local = TRUE)
# source("C:/Users/David/Desktop/Work/R files/Shiny/Tool/ShinyFiles/SupplementaryFunctionsFeas.R", local = TRUE)
# source("C:/Users/David/Desktop/Work/R files/Shiny/Tool/ShinyFiles/master.R", local = TRUE)
# source("C:/Users/David/Desktop/Work/R files/Shiny/Tool/ShinyFiles/ReconFunctions.R", local = TRUE)
# source("C:/Users/David/Desktop/Work/R files/Shiny/Tool/ShinyFiles/EpiInputFunctions.R", local = TRUE)
# source("C:/Users/David/Desktop/Work/R files/Shiny/Tool/ShinyFiles/PlottingFunctions.R", local = TRUE)
# source("C:/Users/David/Desktop/Work/R files/Shiny/Tool/ShinyFiles/EpiCalcFunctions.R", local = TRUE)
# source("C:/Users/David/Desktop/Work/R files/Shiny/Tool/ShinyFiles/NBCalcFunctions.R", local = TRUE)


# zombie old relative path
# source("BinaryOutcomeFunction.R", local = TRUE)
# source("BinaryQALYFunction.R", local = TRUE)
# source("ContinuousOutcomeFunction.R", local = TRUE)
# source("ContinuousQALYFunction.R", local = TRUE)
# source("SurvivalOutcomeFunction.R", local = TRUE)
# source("SurvivalQALYFunction.R", local = TRUE)

# new relative path source
# relative paths for publishing in shinyapps.io
# source("SupplementaryFunctions.R", local = TRUE)
# source("SupplementaryFunctionsFeas.R", local = TRUE)
# source("master.R", local = TRUE)
# source("ReconFunctions.R", local = TRUE)
# source("EpiInputFunctions.R", local = TRUE)
# source("PlottingFunctions.R", local = TRUE)
# source("EpiCalcFunctions.R", local = TRUE)
# source("NBCalcFunctions.R", local = TRUE)



shinyServer(function(input, output,clientData, session) {
  
  #############################
  # fixes to rename variables 
  ###########################
  
  # reproduce the typeOfOutcome input from previous iteration of the app
  # previously took values of "benefit","harm", "netHealth"
  newTypeOfOutcome <- reactive(
    if(input$outcomeExpression == "netHealth"){
      "netHealth"
    } else {
      input$benefitOrHarm # takes values "benefit" or "harm"
    }
  )
  
  # note MCsims directly changed in master() inputs
  
  # not used in new master function - not sure why!
  # just use this in writing results?
  newNameOfOutcome <- reactive(
    if(input$outcomeExpression == "netHealth"){
      "QALY"
    } else {
      input$nameOfOutcome # 
    }
  )
  
  
  
  ##############################
  # Update plots showing user inputs
  ##############################
  # plots show what user has inputted
  
  
  

  ##########################
  # Run VOI analysis with ACTION BUTTON 
  ##########################
  
  # create "managed state variable" - a list which can be repeatedly overwritten by user
  VOIResults <- reactiveValues()
  
  observeEvent(input$run, {
    
      # a list which holds the results of the appropriate analysis
      resultsHolder <- reactive({
        # the master function takes all inputs, runs the appropriate model and returns a list of the results
        master(
          typeOfEndpoint  = input$typeOfEndpoint,
          baselineInput  = input$baselineInput,
          MCsims  = 50000,  # changed
          baselineRange = input$baselineRange,
          nEvents  = input$nEvents,
          nAtRisk  = input$nAtRisk,
          numberOfTreatments  = input$numberOfTreatments,
          binaryRelativeScale_t2  = input$binaryRelativeScale_t2,
          binaryRelativeScale_t3 = input$binaryRelativeScale_t3,
          binaryRelativeScale_t4 = input$binaryRelativeScale_t4,
          binaryDist_t2 = input$binaryDist_t2,
          binaryDist_t3 = input$binaryDist_t3,
          binaryDist_t4 = input$binaryDist_t4,
          OR_t2 = input$OR_t2,
          OR_t3 = input$OR_t3,
          OR_t4 = input$OR_t4,
          RR_t2  = input$RR_t2,
          RR_t3  = input$RR_t3,
          RR_t4 = input$RR_t4,
          RD_t2 = input$RD_t2,
          RD_t3 = input$RD_t3,
          RD_t4 = input$RD_t4,
          ORHalfNorm_t2 = input$ORHalfNorm_t2,
          ORHalfNorm_t3 = input$ORHalfNorm_t3,
          ORHalfNorm_t4 = input$ORHalfNorm_t4,
          RRHalfNorm_t2 = input$RRHalfNorm_t2,
          RRHalfNorm_t3 = input$RRHalfNorm_t3,
          RRHalfNorm_t4 = input$RRHalfNorm_t4,
          RDHalfNorm_t2 = input$RDHalfNorm_t2,
          RDHalfNorm_t3 = input$RDHalfNorm_t3,
          RDHalfNorm_t4 = input$RDHalfNorm_t4,
          continuousInput_t2 = input$continuousInput_t2,
          continuousInput_t3 = input$continuousInput_t3,
          continuousInput_t4 = input$continuousInput_t4,
          continMean_t2 = input$continMean_t2,
          continMean_t3 = input$continMean_t3,
          continMean_t4 = input$continMean_t4,
          continSE_t2 = input$continSE_t2,
          continSE_t3 = input$continSE_t3,
          continSE_t4 = input$continSE_t4,
          continDist_t2 = input$continDist_t2,
          continDist_t3 = input$continDist_t3,
          continDist_t4 = input$continDist_t4,
          MD_t2 = input$MD_t2,
          MD_t3 = input$MD_t3,
          MD_t4 = input$MD_t4,
          MDHalfNorm_t2 = input$MDHalfNorm_t2,
          MDHalfNorm_t3 = input$MDHalfNorm_t3,
          MDHalfNorm_t4 = input$MDHalfNorm_t4,
          survivalType  = input$survivalType,
          survivalDist_t2 = input$survivalDist_t2,
          survivalDist_t3 = input$survivalDist_t3,
          survivalDist_t4 = input$survivalDist_t4,
          lambda_t1 = input$lambda_t1,
          gamma_t1 = input$gamma_t1,
          HR_t2 = input$HR_t2,
          HR_t3 = input$HR_t3,
          HR_t4 = input$HR_t4,
          HRHalfNorm_t2 = input$HRHalfNorm_t2,
          HRHalfNorm_t3 = input$HRHalfNorm_t3,
          HRHalfNorm_t4 = input$HRHalfNorm_t4,
          typeOfOutcome = newTypeOfOutcome(), # changed
          tCostsDependOnEvent = input$tCostsDependOnEvent,
          MCD_t2 = input$MCD_t2,
          MCD_t3 = input$MCD_t3 ,
          MCD_t4 = input$MCD_t4,
          cost_t1 = input$cost_t1,
          cost_t2 = input$cost_t2 ,
          cost_t3 = input$cost_t3 ,
          cost_t4 = input$cost_t4 ,
          k = input$k,
          currencySymbol = input$currencySymbol ,
          incidence = input$incidence ,
          discountRate = input$discountRate ,
          timeInformation = input$timeInformation,
          nameOf_t1 = input$nameOf_t1,
          nameOf_t2 = input$nameOf_t2 ,
          nameOf_t3 = input$nameOf_t3 ,
          nameOf_t4 = input$nameOf_t4,
          costEvent_t1 = input$costEvent_t1,
          costEvent_t2 = input$costEvent_t2,
          costEvent_t3 = input$costEvent_t3,
          costEvent_t4 = input$costEvent_t4,
          costNotEvent_t1 = input$costNotEvent_t1,
          costNotEvent_t2 = input$costNotEvent_t2,
          costNotEvent_t3 = input$costNotEvent_t3,
          costNotEvent_t4 = input$costNotEvent_t4,
          
          numberS0States = input$numberS0States,
          numberS1States = input$numberS1States ,
          utility_s01 = input$utility_s01,
          utility_s02 = input$utility_s02,
          utility_s03 = input$utility_s03,
          utility_s04 = input$utility_s04,
          utility_s11 = input$utility_s11,
          utility_s12 = input$utility_s12,
          utility_s13 = input$utility_s13,
          utility_s14 = input$utility_s14,
          lifeDuration_s01 = input$lifeDuration_s01,
          lifeDuration_s02 = input$lifeDuration_s02,
          lifeDuration_s03 = input$lifeDuration_s03,
          lifeDuration_s04 = input$lifeDuration_s04,
          lifeDuration_s11 = input$lifeDuration_s11,
          lifeDuration_s12 = input$lifeDuration_s12,
          lifeDuration_s13 = input$lifeDuration_s13,
          lifeDuration_s14 = input$lifeDuration_s14,
          cost_s01 = input$cost_s01,
          cost_s02 = input$cost_s02,
          cost_s03 = input$cost_s03,
          cost_s04 = input$cost_s04,
          cost_s11 = input$cost_s11,
          cost_s12 = input$cost_s12,
          cost_s13 = input$cost_s13,
          cost_s14 = input$cost_s14,
          probability_s01 = input$probability_s01,
          probability_s02 = input$probability_s02,
          probability_s03 = input$probability_s03,
          probability_s04 = input$probability_s04,
          probability_s11 = input$probability_s11,
          probability_s12 = input$probability_s12,
          probability_s13 = input$probability_s13,
          probability_s14 = input$probability_s14,
          # continuous NB
          deltaUnitUtilityDirection = input$deltaUnitUtilityDirection, # "increase" # Q Is an increase in the primary outcome expected to be associated with an increase or decrease in health state utility?
          deltaUnitUtilitySize = input$deltaUnitUtilitySize, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease the health state utility?
          treatmentDurationMonths = input$treatmentDurationMonths, # Q how long is the treatment effect expected to last
          deltaUnitCostsDirection = input$deltaUnitCostsDirection, # "increase" # Q Is an increase in the primary outcome expected to be associated with disease related cost increases or decreases?
          deltaUnitCostsSize = input$deltaUnitCostsSize, # Q By how much is a one unit increase in the primary outcome expected to increase/decrease disease related costs?
          treatmentCostsMonthly_t1 = input$treatmentCostsMonthly_t1, # used in survival too
          treatmentCostsMonthly_t2 = input$treatmentCostsMonthly_t2,# used in survival too
          treatmentCostsMonthly_t3 = input$treatmentCostsMonthly_t3,# used in survival too
          treatmentCostsMonthly_t4 = input$treatmentCostsMonthly_t4,# used in survival too
          # survival NB
          utilityPreTransition = input$utilityPreTransition, # Q What is the health utility associated with the pre-progression health state?
          monthlyCostPreTransition = input$monthlyCostPreTransition, # Q What are the expected monthly disease related costs associated with the pre-transition health state?
          treatUntilProgression_t1 = input$treatUntilProgression_t1, # "No"  Q Are individuals always treated until progression under the baseline treatment?
          maxDurationOfTreatmentMonths_t1 = input$maxDurationOfTreatmentMonths_t1, # Q what is the maximum number of months that the baseline treatment will be given?
          treatUntilProgression_t2 = input$treatUntilProgression_t2, # "No" "Yes"
          maxDurationOfTreatmentMonths_t2 = input$maxDurationOfTreatmentMonths_t2,
          treatUntilProgression_t3 = input$treatUntilProgression_t3, # "No" "Yes"
          maxDurationOfTreatmentMonths_t3 = input$maxDurationOfTreatmentMonths_t3,
          treatUntilProgression_t4 = input$treatUntilProgression_t4, # "No" "Yes"
          maxDurationOfTreatmentMonths_t4 = input$maxDurationOfTreatmentMonths_t4,
          
          typeOfResearch = input$typeOfResearch,
          durationOfResearch = input$durationOfResearch,
          costResearchFunder = input$costResearchFunder,
          utilisation_t1 = input$utilisation_t1,
          utilisation_t2 = input$utilisation_t2,
          utilisation_t3 = input$utilisation_t3,
          utilisation_t4 = input$utilisation_t4,
          costHealthSystem = input$costHealthSystem,
          durationOfResearchDefinitive = input$durationOfResearchDefinitive,
          durationOfResearchFeas = input$durationOfResearchFeas,
          costResearchFunderFeas = input$costResearchFunderFeas,
          costResearchFunderDefinitive = input$costResearchFunderDefinitive,
          probabilityOfDefinitiveResearch = input$probabilityOfDefinitiveResearch,
          costHealthSystemFeas = input$costHealthSystemFeas,
          costHealthSystemDefinitive = input$costHealthSystemDefinitive 
          
          
               
              )
        })
    
    
    
    # assign results for all models
    VOIResults$optimalTreatment <- resultsHolder()$optimalTreatment
    VOIResults$probTreatment1isMax <- resultsHolder()$probTreatment1isMax
    VOIResults$probTreatment2isMax <- resultsHolder()$probTreatment2isMax
    VOIResults$probTreatment3isMax <- resultsHolder()$probTreatment3isMax
    VOIResults$probTreatment4isMax <- resultsHolder()$probTreatment4isMax
    VOIResults$probOptimalTisMax <- resultsHolder()$probOptimalTisMax
    VOIResults$probOptimalTisNotMax <- resultsHolder()$probOptimalTisNotMax
    VOIResults$expectedOutcomesPerYearoptimalTreatment <- resultsHolder()$expectedOutcomesPerYearoptimalTreatment
    VOIResults$implementationValueExists <- resultsHolder()$implementationValueExists            # new output
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
    VOIResults$maxvalueOfResearchDesign <- resultsHolder()$maxvalueOfResearchDesign
    VOIResults$maxvalueOfResearchAfterDefinitiveTrial <- resultsHolder()$maxvalueOfResearchAfterDefinitiveTrial
    VOIResults$healthOpportunityCostsOfResearch <- resultsHolder()$healthOpportunityCostsOfResearch
    VOIResults$valueOfResearchWithCurrentImplementation <- resultsHolder()$valueOfResearchWithCurrentImplementation
    VOIResults$valueOfResearchWithPerfectImplementation <- resultsHolder()$valueOfResearchWithPerfectImplementation
    VOIResults$ICER_ResearchWithCurrentImplementation <- resultsHolder()$ICER_ResearchWithCurrentImplementation
    VOIResults$ICER_ResearchWithPerfectImplementation <- resultsHolder()$ICER_ResearchWithPerfectImplementation
    VOIResults$valuePer15KResearchSpend <- resultsHolder()$valuePer15KResearchSpend
    VOIResults$valuePerOpCostResearchSpend <- resultsHolder()$valuePerOpCostResearchSpend
    VOIResults$absoluteExpectedHealthOutcomesFromResearchProject <- resultsHolder()$absoluteExpectedHealthOutcomesFromResearchProject
    # additional feasibility outputs
    VOIResults$popDuringFeasResearch <- resultsHolder()$popDuringFeasResearch               # unique Feasibility output
    VOIResults$popDuringDefinitiveResearch <- resultsHolder()$popDuringDefinitiveResearch    # unique Feasibility output
    VOIResults$popAfterDefinitiveResearch <- resultsHolder()$popAfterDefinitiveResearch     # unique Feasibility output
    VOIResults$expectedCostResearchFunder <- resultsHolder()$expectedCostResearchFunder                 # unique
    VOIResults$expectedCostHealthSystem <- resultsHolder()$expectedCostHealthSystem                 # unique
    VOIResults$valueOfCertainResearchWithPerfectImplementation <- resultsHolder()$valueOfCertainResearchWithPerfectImplementation # unique Feasibility output

    
    
  }) # end observe event expression
  
  
  
  ##########################
  # Run EXTRA ANALYSIS 
  ##########################
  
  # # create "managed state variable" - a list which can be repeatedly overwritten by user
  # VOIExtraAnalysis <- reactiveValues()
  # 
  # observeEvent(input$run, {
  #   
  #   # a list which holds the results of the appropriate analysis
  #   resultsHolder2 <- reactive({
  #     # the master function takes all inputs, runs the appropriate model and returns a list of the results
  #     # master()
  #     list(one = mean(1), two = mean(2))
  #     
  #   })
  #   
  #   # assign results for all models
  #   VOIExtraAnalysis$one <- resultsHolder2()$one
  #   
  # }) # end EXTRA ANALYSIS observe event expression
  
  
  ###########################################################
  # Results 
  ####################################################
  # render INPUT and OUTPUT objects and pass to output list
  ####################################################
  # have to update typeOfOutcome to newTypeOfOutcome
  
  # format ouputs for use in reusults (and report??)
  # MUST CALL THESE AS FUNCTIONS!! e.g. FormatValueOfResearchWithPerfectImplementation()
  
  
  
  
  # value of research / implementation
  FormatValueOfResearchPerYear <- reactive( 
    formatC(round(VOIResults$valueOfResearchPerYear, 0), big.mark = ',', format = 'd'))
  
  FormatValueOfImplementationPerYear <- reactive(
    formatC(round(VOIResults$valueOfImplementationPerYear,0), big.mark = ',',format = 'd'))
  
  FormatMaxvalueOfImplementation <- reactive(
    formatC(round(VOIResults$maxvalueOfImplementation,0), big.mark = ',',format = 'd'))

  
  FormatMaxvalueOfResearch <- reactive( 
    formatC(round(VOIResults$maxvalueOfResearch, 0), big.mark = ',', format = 'd'))
  
  FormatMaxvalueOfResearchDesign <- reactive( 
    formatC(round(VOIResults$maxvalueOfResearchDesign, 0), big.mark = ',', format = 'd'))
  
  
  FormatMaxvalueOfResearchAfterDefinitiveTrial <- reactive( 
    formatC(round(VOIResults$maxvalueOfResearchAfterDefinitiveTrial, 0), big.mark = ',', format = 'd'))

  
  FormatValueOfResearchWithPerfectImplementation <- reactive( 
    formatC(round(VOIResults$valueOfResearchWithPerfectImplementation, 0), big.mark = ',', format = 'd'))
  
  FormatValueOfCertainResearchWithPerfectImplementation  <- reactive( 
    formatC(round(VOIResults$valueOfCertainResearchWithPerfectImplementation, 0), big.mark = ',', format = 'd'))
  
  FormatMaxValueOfUncertainResearchWithPerfectImplementation  <- reactive( 
    formatC(round(VOIResults$maxvalueOfResearchAfterDefinitiveTrial* input$probabilityOfDefinitiveResearch, 0), big.mark = ',', format = 'd'))
  
  
  
  # probabilities
  FormatProbOptimalTisNotMax <- reactive(
    paste0(round(VOIResults$probOptimalTisNotMax*100,0), "%"))
  
  FormatProbOptimalTisMax <- reactive(
    paste0(round(VOIResults$probOptimalTisMax*100,0), "%"))
  
  FormatProbabilityOfDefinitiveResearch <- reactive(
    paste0(input$probabilityOfDefinitiveResearch*100, "%"))
  
  FormatProbabilityOfNoDefinitiveResearch <- reactive(
    paste0(100 - input$probabilityOfDefinitiveResearch*100, "%"))
  
  
  # ICERs

  FormatICER_ResearchWithPerfectImplementation <- reactive(
    paste0(input$currencySymbol, formatC(round(VOIResults$ICER_ResearchWithPerfectImplementation,0), big.mark = ',',format = 'd')))
  




  # costs
  
  FormatK <- reactive(
    paste0(input$currencySymbol, formatC(input$k, big.mark = ',',format = 'd')))
  
  FormatHealthOpportunityCostsOfResearch <- reactive( 
    formatC(round(VOIResults$healthOpportunityCostsOfResearch, 0), big.mark = ',', format = 'd'))
  

  # costs: RCT
  FormatCostResearchFunder <- reactive(
    paste0(input$currencySymbol, formatC(input$costResearchFunder, big.mark = ',',format = 'd')))
  
  FormatCostHealthSystem <- reactive(
    paste0(input$currencySymbol, formatC(input$costHealthSystem, big.mark = ',',format = 'd')))
  
  # costs: feasibiltiy
  FormatCostResearchFunderFeas <- reactive(
    paste0(input$currencySymbol, formatC(input$costResearchFunderFeas, big.mark = ',',format = 'd')))
  
  FormatCostResearchFunderDefinitive <- reactive(
    paste0(input$currencySymbol, formatC(input$costResearchFunderDefinitive, big.mark = ',',format = 'd')))
  
  FormatCostHealthSystemFeas <- reactive(
    paste0(input$currencySymbol, formatC(input$costHealthSystemFeas, big.mark = ',',format = 'd')))
  
  FormatCostHealthSystemDefinitive <- reactive(
    paste0(input$currencySymbol, formatC(input$costHealthSystemDefinitive, big.mark = ',',format = 'd')))

  
  FormatExpectedCostResearchFunder <- reactive(
    paste0(input$currencySymbol, formatC(round(VOIResults$expectedCostResearchFunder,0), big.mark = ',',format = 'd')))
  
  FormatExpectedCostHealthSystem <- reactive(
    paste0(input$currencySymbol, formatC(round(VOIResults$expectedCostHealthSystem,0), big.mark = ',',format = 'd')))
  
  
  
  
  # Headline
  ##############
  
  
  # headline: best treatment with current evidence
  output$headlineBestTreatment <- renderText({
    paste0("Given what we currently know about the treatments, 
    the option with the highest expected health benefit is ", 
    VOIResults$optimalTreatment,".")
    })
  
  # test
  headlineBestTreatment <- reactive(
    paste0("Given what we currently know about the treatments, 
    the option with the highest expected health benefit is ", 
           VOIResults$optimalTreatment,"."
  ))
  
  # TEST implementation value condition == 'TRUE' if imp value exists, 'FALSE' if not
  output$implementationValueExists <- renderText({
    VOIResults$implementationValueExists
  })
  
  # if there is implementation value
  output$headlineImpOutcomesExist <- renderText({
    paste0("Not all individuals currently receive ",
           VOIResults$optimalTreatment,
           " and so outcomes can be improved by encouraging its use in the health system.
           The benefits of switching practice are expected to be ",
           FormatValueOfImplementationPerYear(),
           " ", paste0(newNameOfOutcome(),"s") ,
           ifelse(newTypeOfOutcome() != "harm"," gained"," avoided"),
           " per year.")
  })
  # if there is not implementation value
  output$headlineImpOutcomesNotExist <- renderText({
    paste0("As ", VOIResults$optimalTreatment,
           " is current practice in the health system,
           outcomes cannot be improved by changing practice.")
  })
  
  # == TRUE if some value of information,  FASE otherwise
  output$PositiveValueOfInformation <- renderText({
    VOIResults$maxvalueOfResearch > 0
  })
  # if there is no value of informaiton
  output$headlineNoVOI <- renderText({
    paste("The evidence suggests that there is no uncertainty about the treatment which provides the highest expected health benefit.
             As there is no uncertainty, there is no value in further research.")
  })
  # == TRUE if the specific research is worthwhile
  output$specificResearchWorthwhile <- renderText({
    VOIResults$valueOfResearchWithPerfectImplementation > 0
  })
  # if VOI > 0 but this research is not worthwhile
  output$headlineSomeVOIButBadResearch <- renderText({
    paste0("The evidence suggests that there is some uncertainty about whether ", 
           VOIResults$optimalTreatment,
           " provides the highest expected health benefit but the proposed research is inadequate to address this uncertainty.")
  })
  # if there is value in the proposed research
  output$headlineSomeVOIAndGoodResearch <- renderText({
    paste0("The upper bound for the health benefit of the proposed research is estimated to be ",
         FormatValueOfResearchWithPerfectImplementation(),
         " ", paste0(newNameOfOutcome(),"s") ,
         ifelse(newTypeOfOutcome() != "harm"," gained"," avoided"),
         " over the full time horizon.")
    })
  
  # headline: is RCT and there is value in research
  output$headlineValueOfResearchRCT <- renderText({
      paste0("The proposed research is expected to cost the research funder ",
             FormatCostResearchFunder(),
             " meaning the maximum value of the proposed research is estimated to be (",
             FormatCostResearchFunder(), "/", FormatValueOfResearchWithPerfectImplementation(),
             " =) ", FormatICER_ResearchWithPerfectImplementation(), 
             " per ", newNameOfOutcome(), ifelse(newTypeOfOutcome() != "harm", " gained.", " avoided."))
  })
  
  # headline: Feasibility and there is value in research
  # bullet 1
  output$headlineValueOfResearchFeas1 <- renderText({
      paste0("The total expected cost to the research funder of both the feasibility study and the follow up research is (",
             FormatCostResearchFunderFeas(), " + ", FormatCostResearchFunderDefinitive(), " x ", 
             FormatProbabilityOfDefinitiveResearch(), " =) ", FormatExpectedCostResearchFunder())
  })
  # bullet 2
  output$headlineValueOfResearchFeas2 <- renderText({
      paste0("Therefore, the expected upper bound on the value of funding the feasibility trial is (",
             FormatExpectedCostResearchFunder(), "/", FormatValueOfResearchWithPerfectImplementation(),
             " =) ", FormatICER_ResearchWithPerfectImplementation(), 
             " per ", newNameOfOutcome(), ifelse(newTypeOfOutcome() != "harm", " gained.", " avoided."))
  })
  
  

  
  # What is the value of changing practice?
  ###########################################
  
  # reuse text from headline
  # change practice: best treatment with current evidence
  output$changePracticeBestTreatment <- renderText({
    paste0("Given what we currently know about the treatments, 
    the option with the highest expected health benefit is ", 
           VOIResults$optimalTreatment,".")
  })
  
  # if there is implementation value
  output$changePracticeImpOutcomesExist <- renderText({
    paste0("Not all individuals currently receive ",
           VOIResults$optimalTreatment,
           " and so outcomes can be improved by encouraging its use in the health system.
           The benefits of switching practice are expected to be ",
           FormatValueOfImplementationPerYear(),
           " ", paste0(newNameOfOutcome(),"s") ,
           ifelse(newTypeOfOutcome() != "harm"," gained"," avoided"),
           " per year and ",
           FormatMaxvalueOfImplementation(), 
           " over the ", input$timeInformation, " year time horizon."
           )
  })
  # if there is not implementation value
  output$changePracticeImpOutcomesNotExist <- renderText({
    paste0("As ", VOIResults$optimalTreatment,
           " is current practice in the health system,
           outcomes cannot be improved by changing practice.")
  })
  
  # consequences of remaining uncertainty 
  #######################################
  
  # uncertainty: if no uncertainty - bullet 1
  output$uncertaintyNone1 <- renderText({
      # if there is zero VOI
      paste("The evidence suggests that there is no uncertainty about the treatment which provides the highest expected health benefit.
            As there is no uncertainty, there is no value in further research.")
    })
  # uncertainty: if no uncertainty - bullet 2
  output$uncertaintyNone2 <- renderText({
      # if there is zero VOI
      paste("As there is no uncertainty, there can be no value in further research.
            To improve health outcomes, research funding should address other uncertainties in the health system.")
    })
  
  
  # uncertainty: IS uncertainty - bullet 1
  output$uncertaintySome1 <- renderText({
      paste0("Due to uncertainty in the evidence there is a ", 
             FormatProbOptimalTisNotMax(),
             " chance that ", 
             VOIResults$optimalTreatment
             ," does not provide the largest health benefit.")
  })
  # uncertainty: IS uncertainty - bullet 2
  output$uncertaintySome2 <- renderText({
      paste0("This uncertainty translates into consequences for patient outcomes, i.e. health consequences due to uncertainty about the best treatment.")
  })
  # uncertainty: IS uncertainty - bullet 3
  output$uncertaintySome3 <- renderText({
      paste0("The health consequences this uncertainty are estimated to be ",
             FormatValueOfResearchPerYear(),
             ifelse(newTypeOfOutcome() != "harm"," "," additional "),
             paste0(newNameOfOutcome(),"s") ,
             ifelse(newTypeOfOutcome() != "harm"," lost",""),
             " per year. How this figure is arrived at is illustrated in the graph below:")
  })
  # uncertainty: IS uncertainty - explain hist - bullet 1
  output$discussHistVOIYear1 <- renderText({
           paste0("The left hand bar in the diagram shows that there is a ",
                  FormatProbOptimalTisMax(), " chance that ",
                  VOIResults$optimalTreatment, 
                  " provides the largest health benefit. If this is the case then there are zero health consequences of uncertainty.")
  })
  # uncertainty: IS uncertainty - explain hist - bullet 2
  output$discussHistVOIYear2 <- renderText({
    paste0("However, there is a ",
           FormatProbOptimalTisNotMax(),
           " chance that ", VOIResults$optimalTreatment,
           " does not provide the largest health benefit. These health consequences are not uniform. 
           There is a greater chance of more limited consequences compared to a smaller chance of greater adverse consequences.")
  })

  
  # Proposed research - requires some uncertainty
  ##########################
  
  # Proposed research: RCT
  ###
  
  # max value over 15 years
  output$proposedResearchMaxValueOfResearchRCT <- renderText({
    paste0("Extending the yearly consequences of uncertainty over the ",
           input$timeInformation, 
           " year time horizon, the maximum value of research is estimated to be ",
           FormatMaxvalueOfResearch(),
           " ", paste0(newNameOfOutcome(),"s") ,
           ifelse(newTypeOfOutcome() != "harm"," gained"," avoided"),
           " over the full time horizon.")
  })
  
  # max value of full trial 
  output$fullTrialMaxValueOfResearchRCT <- renderText({
    paste0("It is expected that it will take ", 
           input$durationOfResearch
           ," years for the research to report and so the upper bound on the value of this research is expected to fall to ",
           FormatMaxvalueOfResearchDesign() ,
           " ", paste0(newNameOfOutcome(),"s"),ifelse(newTypeOfOutcome() != "harm", " gained.", " avoided."))
  })
  
  # test 
  # == TRUE if max value of the proposed research is positive,  FALSE otherwise
  output$PositiveValueOfResearchDesignRCT <- renderText({
    VOIResults$maxvalueOfResearchDesign > 0
  })
  
  output$researchTakesTooLong <- renderText({
    paste0("As the value of this research is zero this suggests that the proposed research takes too long to report. By the time the research reports the population who can potentially benefit is expected to fall to zero.")
  })
  
  output$expectedRCTNHSOpportunityCost <- renderText({
    paste0("Funding this research imposes an expected cost of ",
           FormatCostHealthSystem(),
           " on additional health system support and treatment budgets. The estimated opportunity cost of these resources is (",
           FormatCostHealthSystem(), "/", FormatK(), " =) ",
           FormatHealthOpportunityCostsOfResearch(), 
           " QALYs. After these costs have been subtracted the maximum value of this research falls from  ",
           FormatMaxvalueOfResearchDesign(), " to ",
           FormatValueOfResearchWithPerfectImplementation(), "."
           )
  })
  
  output$ValueOfResearchRCT <- renderText({
    paste0("The proposed research is expected to cost the research funder ",
           FormatCostResearchFunder(), 
           ", meaning the maximum value of the proposed research is estimated to be (",
           FormatCostResearchFunder(), "/", FormatValueOfResearchWithPerfectImplementation(),
           " =) ", FormatICER_ResearchWithPerfectImplementation(), 
           " per ", newNameOfOutcome(), ifelse(newTypeOfOutcome() != "harm", " gained.", "avoided."))
  })
  
  
  # Proposed research: feasibiltiy
  ###
  
  # max value over 15 years
  output$proposedResearchMaxValueOfResearchFeas <- renderText({
    paste0("Extending the yearly consequences of uncertainty over the ",
           input$timeInformation, 
           " year time horizon, the maximum value of research is estimated to be ",
           FormatMaxvalueOfResearch(),
           " ", paste0(newNameOfOutcome(),"s") ,
           ifelse(newTypeOfOutcome() != "harm"," gained"," avoided"),
           " over the full time horizon.")
  })
  
  # max value of full trial (feasibility research)
  output$fullTrialMaxValueOfResearchFeas <- renderText({
    paste0("It is expected that it will take ", 
           input$durationOfResearchFeas
           ," years for the feasibility study to report and a further ",
           input$durationOfResearchDefinitive,
           " years for potential follow up research to report. After (",
           input$durationOfResearchFeas ," + ",input$durationOfResearchDefinitive ," =) ",input$durationOfResearchFeas + input$durationOfResearchDefinitive ,
           " years, the upper bound on the value of this research is expected to be ",
           FormatMaxvalueOfResearchAfterDefinitiveTrial() ,
           " ", paste0(newNameOfOutcome(),"s"),ifelse(newTypeOfOutcome() != "harm", " gained.", " avoided."))
  })
  
  # test 
  # == TRUE if value of full trial in feasibility studies,  FASE otherwise
  output$PositiveValueOfFullTrialFeas <- renderText({
    VOIResults$maxvalueOfResearchAfterDefinitiveTrial > 0
  })
  
  # reduce max value by prob of feasibilty research
  output$fullTrialNotCertainFeas <- renderText({
    paste0("As there is a ", 
           FormatProbabilityOfNoDefinitiveResearch(),
           " chance that the full trial is not possible, the upper bound on the value of this project falls to ",
           FormatMaxValueOfUncertainResearchWithPerfectImplementation(),
           " ", paste0(newNameOfOutcome(),"s"),ifelse(newTypeOfOutcome() != "harm", " gained", " avoided"),
           " over the full time horizon.")
  })
  
  
  output$expectedFullTrialFunderCost <- renderText({
    paste0("The feasibility trial is expected to cost the research funder ",
           FormatCostResearchFunderFeas(),
           " and the definitive trial is expected to cost ", 
           FormatCostResearchFunderDefinitive(),
           ". As the feasibility study costs will always be incurred and there is a ",
           FormatProbabilityOfDefinitiveResearch(),
           " chance that the follow-up research will occur, the total expected cost to the research funder is",
           FormatCostResearchFunderFeas(), " + ", FormatCostResearchFunderDefinitive(), " x ", 
           FormatProbabilityOfDefinitiveResearch(), " =) ", FormatExpectedCostResearchFunder(), ".")
  })
  
  output$valueFeasNatural <- renderText({
    paste0("Therefore, the expected upper bound on the value of funding the feasibility trial is (",
           FormatExpectedCostResearchFunder(), "/", FormatValueOfResearchWithPerfectImplementation(),
           " =) ", FormatICER_ResearchWithPerfectImplementation(), 
           " per ", newNameOfOutcome(), ifelse(newTypeOfOutcome() != "harm", " gained.", " avoided."))
    })
  
  
  output$expectedFullTrialNHSCost <- renderText({
    paste0("The feasibility trial is expected to cost the health system ",
           FormatCostHealthSystemFeas(),
           " and the definitive trial is expected to cost ",
           FormatCostHealthSystemDefinitive(),
           ". In the same manner as above, the expected cost to the health system is (", 
           FormatCostHealthSystemFeas(), " + ", FormatCostHealthSystemDefinitive(), " x ", 
           FormatProbabilityOfDefinitiveResearch(), " =) ", FormatExpectedCostHealthSystem(), ".")
    })
  
  output$expectedFullTrialNHSOpportunityCost <- renderText({
    paste0("The opportunity costs associated with the above health system resources are estimated to be (",
           FormatExpectedCostHealthSystem(), "/", FormatK(), " =) ",
           FormatHealthOpportunityCostsOfResearch(), 
           " QALYs. After these costs have been subtracted the maximum value of this research falls from  ",
           FormatMaxValueOfUncertainResearchWithPerfectImplementation(),
           " to ", FormatValueOfResearchWithPerfectImplementation(), ".")
  })
  
  # test 
  # == TRUE if value in feasibilty trial after subtracting op costs
  output$PositiveValueOfFeas <- renderText({
    VOIResults$valueOfResearchWithPerfectImplementation > 0
  })
  
  
  output$ValueOfResearchFeasQALY <- renderText({
    paste0("Therefore, the expected upper bound on the value of funding the feasibility trial is (",
           FormatExpectedCostResearchFunder(), "/", FormatValueOfResearchWithPerfectImplementation(),
           " =) ", FormatICER_ResearchWithPerfectImplementation(), 
           " per ", newNameOfOutcome(), ifelse(newTypeOfOutcome() != "harm", " gained.", "avoided."))
  })
  
  
  
  # output plot
  #############
  ##########
  output$histVOIYear <- renderPlot({
    barplot(VOIResults$listForhistVOIYear$prob_bin,
            width = 0.5,
            names.arg = VOIResults$listForhistVOIYear$bin_value,
            ylim = c(0,1),
            main = "Consequences of uncertainty (per year)",
            xlab = "Primary outcomes",
            ylab = "Probability")
  })
  
  # output tables
  ###############
  ###########
  
  # expected outcomes per year table
  output$tableEventsPerYear <- renderTable({VOIResults$tableEventsPerYearDF}, include.rownames = FALSE)
  
  
  
  # treatment cost table
  output$tableTreatmentCosts <- renderTable({VOIResults$tableTreatmentCostsDF}, include.rownames = FALSE)

  
  
  
  
  
  
  # zombie code
  ######### old stuff
  
  # paste0("The total expected cost to the research funder of both the feasibility study and the follow up research is (",
  #        FormatCostResearchFunderFeas(), " + ", FormatCostResearchFunderDefinitive(), " x ", 
  #        FormatProbabilityOfDefinitiveResearch(), " =) ", FormatExpectedCostResearchFunder())
  # 
  # paste0("Therefore, the expected upper bound on the value of funding the feasibility trial is (",
  #        FormatExpectedCostResearchFunder(), "/", FormatValueOfResearchWithPerfectImplementation(),
  #        " =) ", FormatICER_ResearchWithPerfectImplementation(), 
  #        " per ", newNameOfOutcome(), ifelse(newTypeOfOutcome() != "harm", " gained.", "avoided."))
  # 
  
  
  # output$FeasVOIresults <- renderText({
  #   ifelse(VOIResults$maxvalueOfResearch > 0,
  #          paste("From the above analysis, the maximum that can be gained from the follow-up research is",VOIResults$maxvalueOfResearch ,paste0(newNameOfOutcome(),"s."), 
  #                "However, the feasibility trial takes",input$durationOfResearchFeas ,"years to report and has a",paste0(input$probabilityOfDefinitiveResearch*100, "%") ,
  #                "chance of leading to the follow-up trial, which would then take an additional",input$durationOfResearchDefinitive ,"years to report. 
  #                Naturally, the value of the additional evidence will decline the longer it takes the follow-up trial to report. 
  #                If the follow-up trial was certain to report, it would take (",input$durationOfResearchFeas ,"+",input$durationOfResearchDefinitive ,"=)",input$durationOfResearchFeas + input$durationOfResearchDefinitive ,
  #                "years in total and the expected value of the additional evidence would be",VOIResults$valueOfCertainResearchWithPerfectImplementation ,paste0(newNameOfOutcome(),"s"),ifelse(newTypeOfOutcome() != "harm", "gained", "avoided"),"over the",input$timeInformation ,"year period. 
  #                As there is a",paste0(input$probabilityOfDefinitiveResearch*100, "%") ,"chance that the follow-up trial is possible, the value of the project falls to",VOIResults$valueOfResearchWithPerfectImplementation ,paste0(newNameOfOutcome(),"s"),ifelse(newTypeOfOutcome() != "harm", "gained.", "avoided."),
  #                "The feasibility trial is expected to cost the research funder",paste0(input$currencySymbol, formatC(input$costResearchFunderFeas, big.mark = ',',format = 'd')) ,"and the definitive trial is expected to cost",paste0(input$currencySymbol, formatC(input$costResearchFunderDefinitive, big.mark = ',',format = 'd')),
  #                ". As the feasibility study costs will always be incurred and there is a",paste0(input$probabilityOfDefinitiveResearch*100, "%") ,"chance that the follow-up research will not occur, 
  #                the total expected cost to the research funder is (",paste0(input$currencySymbol, formatC(input$costResearchFunderFeas, big.mark = ',',format = 'd')) ,"+",paste0(input$currencySymbol, formatC(input$costResearchFunderDefinitive, big.mark = ',',format = 'd')) ,"x",paste0(input$probabilityOfDefinitiveResearch*100, "%") ,"=)",VOIResults$expectedCostResearchFunder,
  #                ". Therefore, the expected value of funding the feasibility trial is (",VOIResults$expectedCostResearchFunder,"/",VOIResults$valueOfResearchWithPerfectImplementation  ,"=)",VOIResults$ICER_ResearchWithPerfectImplementation,"per",newNameOfOutcome(),ifelse(newTypeOfOutcome() != "harm", "gained.", "avoided."),
  #                ifelse(newTypeOfOutcome() == "netHealth",
  #                       # final text if QALY analysis
  #                       paste("Funding this research, imposes an expected cost of",paste0(input$currencySymbol, formatC(input$costHealthSystemFeas, big.mark = ',',format = 'd')) ,"+",paste0(input$currencySymbol, formatC(input$costHealthSystemDefinitive, big.mark = ',',format = 'd')) ,"x",paste0(input$probabilityOfDefinitiveResearch*100, "%") ,"=)",VOIResults$expectedCostHealthSystem,
  #                             "on the health system. These resources could have been used in direct patient care.
  #                             In order to reflect the health opportunity costs associated with these costs we use the value of",paste0(input$currencySymbol, formatC(input$k, big.mark = ',',format = 'd')) ,"per QALY.
  #                             This means that for every",paste0(input$currencySymbol, formatC(input$k, big.mark = ',',format = 'd')) ,"of health system resources displaced the system can expect to lose one QALY.
  #                             The opportunity costs associated with the health system research costs is estimated to be (",VOIResults$expectedCostHealthSystem,"/",paste0(input$currencySymbol, formatC(input$k, big.mark = ',',format = 'd')),"=)",VOIResults$healthOpportunityCostsOfResearch ,"QALYs.
  #                             These opportunity costs have already been subtracted from the trial benefits in calculating the value of the trial.
  #                             As the value of the trial is expressed in a generic measure of health outcome it can be compared to other candidates competing for funding.
  #                             As stated previously, for every",paste0(input$currencySymbol, formatC(input$k, big.mark = ',',format = 'd'))  ,"spent the health system can expect to produce one QALY. 
  #                             By funding this proposal the funding agency has to spend",VOIResults$ICER_ResearchWithPerfectImplementation ,"to produce one QALY. 
  #                             This means that the proposal offers",ifelse(VOIResults$valuePerOpCostResearchSpend > 1, "better", "worse") ,"value for money to the health system compared to general service provision.
  #                             Every",paste0(input$currencySymbol, formatC(input$k, big.mark = ',',format = 'd')),"spent on this research project is expected to produce",VOIResults$valuePerOpCostResearchSpend," QALYs.
  #                             This can be compared to 1 QALY produced in the general health system from",paste0(input$currencySymbol, formatC(input$k, big.mark = ',',format = 'd')),"of spending.
  #                             However, given a fixed budget for research funding, whether the proposed trial represents good value for research spending depends on how it compares to other proposals competing for funding."),
  #                       # final text if not QALY analysis
  #                       paste("The value of the proposed research can now be compared to the other proposals competing for funding. 
  #                             Whether this research represents good value to the health system depends on the value of the other potential uses of these resources.")
  #                       )
  #                
  #                       ),
  #          # if there is no value in research just leave blank
  #          ""
  #          )
  # })  
  
  
  # "From the tall left hand bar in the diagram, it can be seen that there is over a ",VOIResults$probOptimalTisMax ,
  # "chance that there will be zero or very small consequences if the optimal treatment with current evidence (",                                                                                                          VOIResults$optimalTreatment, ") is used. 
  # However, there is a small chance of larger consequnces, which are illustrated by the reamaining bars in the graph.
  # The average over this distribution provides an estimate of the expected consequences of uncertainty, 
  # which is",VOIResults$valueOfResearchPerYear ,paste0(newNameOfOutcome(),"s"), 
  # "per year due to uncertainty.
  # These expected consequences can be interpreted as an estimate of the health benefits that could potentially be gained each year 
  # if the uncertainty in the decision could be resolved, i.e., 
  # it indicates an expected upper bound on the health benefits of further research which would confirm which treatment is optimal.
  # These expected benefits will increase with the size of the patient population whose treatment choice can be informed by additional evidence and the time over which the evidence is expected to be useful.
  
  
  
  
  
  ######################################## old results stuff #############################################
  
  
  
  #   # with",
  #   # round(VOIResults$expectedOutcomesPerYearoptimalTreatment,0),
  #   # paste0(newNameOfOutcome(), "s"),
  #   # "per year.")
  # #formatC(input$incidence, big.mark = ',', format = 'd')
  # 
  # output$bullet2 <- renderText({
  #   paste(
  #     "The health consequences the remaining uncertainty are estimated to be 42 functional recovery’s per year."
  #   )
  # })
  # 
  # # bullet 3
  # 
  # # conditional (RCT)
  # output$bullet4 <- renderText({
  #   paste("The proposed research is expected to cost the research funder", 
  #         "£2,854,000, meaning the maximum value of the proposed research is 
  #          (£2,854,000/299 =) £9,533 
  #         per Functional recovery gained.[or QALY gained if RCT QALY]")
  # })
  # 
  # output$bullet5 <- renderText({
  #   paste("output if feasibiltiy")
  # })
  # 
  # # Create conditional text segments for results section
  # ###########################
  # 
  # output$testText <- renderText({"test"})  # test
  # 
  # # Headline results and overview
  # ########
  # 
  # output$introduceResearch <- renderText({
  #   paste("This proposal is for a",input$numberOfTreatments, "arm", 
  #         ifelse(input$typeOfResearch == "RCT",
  #                "randomised controlled trial.",
  #                "feasibility study."),
  #         "The primary endpoint in the trial is", newNameOfOutcome(), "."
  #         ) # end paste
  # })
  # 
  # #****
  # # adjust for when research is not valuable!!!
  # # ICER in primary natural outcome for RCT and feasibility study
  # output$ICERresult <- renderText({
  #  paste("Considering the uncertainty in the primary endpoint",
  #        # extra text for feasibility studies
  #        ifelse(input$typeOfResearch == "feasibility",
  #               paste("and a", paste0(input$probabilityOfDefinitiveResearch*100, "%"), "chance of the feasibility study leading to a follow-up trial,"),
  #               ""),
  #        # text if there is/isnt value in the research
  #        ifelse(VOIResults$maxvalueOfResearch > 0,
  #               # text if there is value in the research
  #               paste("the value of the research is calculated to be approximately", VOIResults$ICER_ResearchWithPerfectImplementation,
  #                     "per", newNameOfOutcome(), ifelse(newTypeOfOutcome() != "harm","gained.","avoided."),
  #                     "This means that the research funder must spend",VOIResults$ICER_ResearchWithPerfectImplementation,
  #                     "to", ifelse(newTypeOfOutcome() != "harm", "gain", "avoid"), "one", newNameOfOutcome(), ".",
  #                     "As the research funder has limited resources, whether this represents good value for money depends on how this compares to other proposals competing for funding."),
  #               # text if NO value in research
  #               paste(VOIResults$optimalTreatment, "is certainty the optimal treatment and therefore there is no value in any further research.
  #                     ", ifelse(VOIResults$implementationValueExists == TRUE, 
  #                               paste("Because utilisation of",VOIResults$optimalTreatment , " is not 100%, outcomes can be improved by encouraging it's use in the health system." ),
  #                               paste("Because the current utilisation of", VOIResults$optimalTreatment , " is already 100%, outcomes can not be improved by encouraging it's use in the health system."))
  #               )
  #  )
  #  )
  # })
  # 
  # 
  # # treatment cost table
  # output$tableTreatmentCosts <- renderTable({VOIResults$tableTreatmentCostsDF}, include.rownames = FALSE)
  # 
  # # text for discussion about treatment costs
  # output$discussTableTreatmentCosts <- renderText({
  #   paste("The table above shows the estimated costs associated with each of the treatments.
  #         The yearly costs are calculated by multiplying the cost of treating one individual with the incidence per year (", formatC(input$incidence, big.mark = ',', format = 'd'), ").", 
  #         "The additional costs are calculated by subtracting the yearly costs of the baseline treatment (treatment 1) from the costs of the other treatments.
  #         Total costs are obtained by multiplying the cost of treating one individual with the total number of individuals who will face this decision (", VOIResults$popTotal, ").", 
  #         "The total number who face the decision will depend on the incidence, the time over which the evidence is expected to be valuable and the discount rate.")
  # 
  # })
  # 
  # 
  # 
  # # expected outcomes per year table
  # output$tableEventsPerYear <- renderTable({VOIResults$tableEventsPerYearDF}, include.rownames = FALSE)
  # 
  # # text for general discussion about current information (common text for RCT and Feas)
  # output$resultsCurrenInformation <- renderText({
  #   paste("From the table above",VOIResults$optimalTreatment, "is favoured by the evidence with", 
  #         VOIResults$expectedOutcomesPerYearoptimalTreatment, paste0(newNameOfOutcome(), "s"), "expected per year.",
  #         ifelse(VOIResults$implementationValueExists == TRUE, 
  #                # text if there is implementation value
  #                paste(" Because utilisation of",VOIResults$optimalTreatment , "is not 100%, outcomes can be improved by encouraging the use of", VOIResults$optimalTreatment,
  #                ". The benefits of switching practice to",VOIResults$optimalTreatment , 
  #                " are estimated to be",VOIResults$valueOfImplementationPerYear,paste0(newNameOfOutcome(),"s") ,"per year and",VOIResults$maxvalueOfImplementation ,paste0(newNameOfOutcome(),"s"), "over the", input$timeInformation, "year time horion."),
  #                # text if there is NOT implementation value
  #                paste(" Because the current utilisation of", VOIResults$optimalTreatment , 
  #                      " is already 100%, outcomes can not be improved by encouraging the use of", VOIResults$optimalTreatment ,".")))
  # })
  # 
  # # first block of text for discussion of the potential value of research
  # # (common text for RCT and Feas)
  # #     - when there is value in research 
  # #     - when there is NOT value in research
  # # note: conditional panel JavaScript has a problem when the condition is an object created by server operations
  # # need to render text and diagrams and so cannot do everything in one output$ object.
  # output$resultsValueOfResearch <- renderText({
  #   ifelse(VOIResults$maxvalueOfResearch > 0,
  #          # text if there is value in the research
  #          paste("There is uncertainty about which treatment is optimal in this decision.",
  #                "From the previous section",  VOIResults$optimalTreatment, "appears to be the optimal treatment with current evidence as it has the best expected outcomes.
  #                However uncertainty about this means that for every individual treated there is a",VOIResults$probOptimalTisMax ,"chance that this is the incorrect choice of treatment.
  #                This uncertainty translates into consequences for patient outcomes, i.e. health consequences due to uncertainty about the best treatment.
  #                This is calculated by combining the uncertain relative effect with", 
  #                   ifelse(input$typeOfEndpoint != "continuous", 
  #                               paste("an estimate of the baseline risk of the outcome and multiplying by the number of individuals affected by the decision each year."),
  #                                               paste("the number of individuals affected by the decision each year.")),
  #                "This results in a distribution of health consequences in number of",paste0(newNameOfOutcome(), "s"), "per year.
  #                The distribution of these consequences is illustrated in the diagram below:"
  #                ),
  #          # text if there is NO value of research
  #          paste("The evidence suggests that there is no uncertainty and that",VOIResults$optimalTreatment , "is definitely the optimal treatment.
  #                This lack of uncertainty means there is no value in", 
  #                ifelse(input$typeOfResearch == "feasibility",
  #                       "carrying out research. Even if the follow up research was possible, it would not provide useful information. Therefore neither the feasibility study nor the follow up study are expected to improve health outcomes.",
  #                       "further research."),
  #                "To improve health outcomes, resources should be focused on implementation.")
  #   )
  # })
  # 
  # 
  # # bug
  # # problem in ui.R conditional planel
  # # cannot make javaScript condition depend on results of VOI calcluation
  # # must display this even if there is value in the research
  # # (common to both RCT and Feas)
  # 
  # 
  # 
  # 
  # # discussion of results : common to both RCT and Feas
  # output$VOIresultsPart1 <- renderText({
  #   ifelse(VOIResults$maxvalueOfResearch > 0,
  #          paste("It is expected that the information will be valuable for about", input$timeInformation ,"years. 
  #                This means that the consequences of uncertainty surrounding the decision become magnified by the fact that 
  #                (in the absence of better evidence) we might not be making the optimal decision every year for", input$timeInformation, "years. 
  #                Taking this time horizon into account means that the expected consequences of uncertainty are",
  #                VOIResults$maxvalueOfResearch ,paste0(newNameOfOutcome(),"s"), "over a", input$timeInformation, "year period (after discounting appropriately)."
  #          ),
  #          # if there is no value in research just leave blank
  #          ""
  #   )
  # })
  # 
  # # ** the op costs of research assume that carrying out research cannot save money in the health system
  # # this could be a problem for p3 AND P6**
  # # that weird A formatting problem with paste0(input$currencySymbol, formatC(input$costResearchFunder, big.mark = ',',format = 'd'))
  # # discussion of results Text only for RCT analysis
  # output$RCTVOIresults <- renderText({
  #   ifelse(VOIResults$maxvalueOfResearch > 0,
  #          paste("However, the proposed trial will not report immediately and the value of additional evidence will decline the longer it takes to report.
  #          As the trial is expected to take",input$durationOfResearch ,"years to report, 
  #          the expected value of the additional evidence is",VOIResults$valueOfResearchWithPerfectImplementation ,paste0(newNameOfOutcome(), "s"),ifelse(newTypeOfOutcome() != "harm", "gained", "avoided") ,"over the",input$durationOfResearch, "year period. 
  #          The trial is expected to cost the research funder",paste0(input$currencySymbol, formatC(input$costResearchFunder, big.mark = ',',format = 'd')) ,". 
  #          Therefore, the maximum value of the trial is (",paste0(input$currencySymbol, formatC(input$costResearchFunder, big.mark = ',',format = 'd'), "/",VOIResults$valueOfResearchWithPerfectImplementation), "=)",
  #          VOIResults$ICER_ResearchWithPerfectImplementation,"per", newNameOfOutcome(), ifelse(newTypeOfOutcome() != "harm", "gained.", "avoided."),
  #          ifelse(newTypeOfOutcome() == "netHealth",
  #                 # final text if QALY analysis
  #                 paste("Funding this research, imposes a cost of",paste0(input$currencySymbol, formatC(input$costHealthSystem, big.mark = ',',format = 'd')) ,"on the health system.
  #                 These resources could have been used in direct patient care.
  #                 In order to reflect the health opportunity costs associated with these costs we use the value of",paste0(input$currencySymbol, formatC(input$k, big.mark = ',',format = 'd')) ,"per QALY.
  #                 This means that for every",paste0(input$currencySymbol, formatC(input$k, big.mark = ',',format = 'd')) ,"of health system resources displaced the system can expect to lose one QALY.
  #                 The opportunity costs associated with the health system research costs is estimated to be (",paste0(input$currencySymbol, formatC(input$costHealthSystem, big.mark = ',',format = 'd')),"/",paste0(input$currencySymbol, formatC(input$k, big.mark = ',',format = 'd')),"=)",VOIResults$healthOpportunityCostsOfResearch ,"QALYs.
  #                 These opportunity costs have already been subtracted from the trial benefits in calculating the value of the trial.
  #                 As the value of the trial is expressed in a generic measure of health outcome it can be compared to other candidates competing for funding.
  #                 As stated previously, for every",paste0(input$currencySymbol, formatC(input$k, big.mark = ',',format = 'd'))  ,"spent the health system can expect to produce one QALY. 
  #                 By funding this proposal the funding agency has to spend",VOIResults$ICER_ResearchWithPerfectImplementation ,"to produce one QALY. 
  #                 This means that the proposal offers",ifelse(VOIResults$valuePerOpCostResearchSpend > 1, "better", "worse") ,"value for money to the health system compared to general service provision.
  #                 Every",paste0(input$currencySymbol, formatC(input$k, big.mark = ',',format = 'd')),"spent on this research project is expected to produce",VOIResults$valuePerOpCostResearchSpend," QALYs.
  #                 This can be compared to 1 QALY produced in the general health system from",paste0(input$currencySymbol, formatC(input$k, big.mark = ',',format = 'd')),"of spending.
  #                 However, given a fixed budget for research funding, whether the proposed trial represents good value for research spending depends on how it compares to other proposals competing for funding."),
  #                 # final text if not QALY analysis
  #                 paste("The value of the proposed research can now be compared to the other proposals competing for funding. 
  #                 Whether this research represents good value to the health system depends on the value of the other potential uses of these resources.")
  #                 )
  #       
  #          ),
  #          # if there is no value in research just leave blank
  #          ""
  #          )
  # })
  # 
  # # 
  # 

  
  
  
  
  ########################################################
  # Results and report writing
  #########################################################
  ############################################################
  
  ##########################
  # Report writing
  ##############################
  # approach based on : https://shiny.rstudio.com/articles/generating-reports.html
  
  # NB: when adding objects to params list - must set default value in report.Rmd equal to NA
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.doc",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list( 
        # need to add inputs to construct table
        
        # outputs
        timeInformation = input$timeInformation,
        newNameOfOutcome = newNameOfOutcome(),
        headlineBestTreatment = headlineBestTreatment(),
        optimalTreatment = VOIResults$optimalTreatment,
        probTreatment1isMax = VOIResults$probTreatment1isMax,
        probTreatment2isMax = VOIResults$probTreatment2isMax,
        probTreatment3isMax = VOIResults$probTreatment3isMax ,
        probTreatment4isMax = VOIResults$probTreatment4isMax ,
        probOptimalTisMax = VOIResults$probOptimalTisMax ,
        probOptimalTisNotMax = VOIResults$probOptimalTisNotMax, 
        expectedOutcomesPerYearoptimalTreatment = VOIResults$expectedOutcomesPerYearoptimalTreatment,
        implementationValueExists = VOIResults$implementationValueExists ,
        uncertaintyInCurrentEvidenceExists = VOIResults$uncertaintyInCurrentEvidenceExists ,
        popDuringResearch = VOIResults$popDuringResearch ,
        popAfterResearch = VOIResults$popAfterResearch ,
        popTotal = VOIResults$popTotal ,
        listForhistVOIYear = VOIResults$listForhistVOIYear ,
        valueOfResearchPerYear = VOIResults$valueOfResearchPerYear ,
        valueOfImplementationPerYear = VOIResults$valueOfImplementationPerYear ,
        tableEventsPerYearDF = VOIResults$tableEventsPerYearDF    ,              
        tableProbabilityMaxDF = VOIResults$tableProbabilityMaxDF ,
        tableTreatmentCostsDF = VOIResults$tableTreatmentCostsDF,
        Cell_A = VOIResults$Cell_A ,
        Cell_C = VOIResults$Cell_C ,
        Cell_D = VOIResults$Cell_D ,
        maxvalueOfImplementation = VOIResults$maxvalueOfImplementation ,
        maxvalueOfResearch = VOIResults$maxvalueOfResearch, 
        maxvalueOfResearchDesign = VOIResults$maxvalueOfResearchDesign,
        maxvalueOfResearchAfterDefinitiveTrial = VOIResults$maxvalueOfResearchAfterDefinitiveTrial,
        healthOpportunityCostsOfResearch = VOIResults$healthOpportunityCostsOfResearch,
        valueOfResearchWithCurrentImplementation = VOIResults$valueOfResearchWithCurrentImplementation ,
        valueOfResearchWithPerfectImplementation = VOIResults$valueOfResearchWithPerfectImplementation ,
        ICER_ResearchWithCurrentImplementation = VOIResults$ICER_ResearchWithCurrentImplementation ,
        ICER_ResearchWithPerfectImplementation = VOIResults$ICER_ResearchWithPerfectImplementation ,
        valuePer15KResearchSpend = VOIResults$valuePer15KResearchSpend ,
        valuePerOpCostResearchSpend = VOIResults$valuePerOpCostResearchSpend ,
        absoluteExpectedHealthOutcomesFromResearchProject = VOIResults$absoluteExpectedHealthOutcomesFromResearchProject ,
        # additional feasibility outputs
        popDuringFeasResearch = VOIResults$popDuringFeasResearch    ,          
        popDuringDefinitiveResearch = VOIResults$popDuringDefinitiveResearch ,
        popAfterDefinitiveResearch = VOIResults$popAfterDefinitiveResearch   ,
        expectedCostResearchFunder = VOIResults$expectedCostResearchFunder   ,          
        expectedCostHealthSystem = VOIResults$expectedCostHealthSystem    ,          
        valueOfCertainResearchWithPerfectImplementation = VOIResults$valueOfCertainResearchWithPerfectImplementation
        
        # formatted values - cause a bug i think!
        # FormatValueOfResearchWithPerfectImplementation = FormatValueOfResearchWithPerfectImplementation(),
        # FormatCostResearchFunder  = FormatCostResearchFunder(),
        # FormatICER_ResearchWithPerfectImplementation  =FormatICER_ResearchWithPerfectImplementation(),
        # FormatCostResearchFunderFeas = FormatCostResearchFunderFeas(),
        # FormatCostResearchFunderDefinitive =  FormatCostResearchFunderDefinitive(),
        # FormatProbabilityOfDefinitiveResearch =  FormatProbabilityOfDefinitiveResearch(),
        #FormatExpectedCostResearchFunder = FormatExpectedCostResearchFunder()
        
        
        
        
      ) # end of list of objects saved to params 
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
 
  
}) # end server function












































#######################################################################################
# ZOMBIE CODE 
#######################################################################################



# Raw input and output objects
#########################

# input objects
#output$nameOf_t1 <- renderText({
#  paste("The name of treatment 1 is", input$nameOf_t1)
#})
#output$nameOf_t2 <- renderText({input$nameOf_t2})
#output$nameOf_t3 <- renderText({input$nameOf_t3})
#output$nameOf_t4 <- renderText({input$nameOf_t4})
#output$nameOfOutcome <- renderText({newNameOfOutcome()})

# output objects
# output$optimalTreatment <- renderText({VOIResults$optimalTreatment})
# output$expectedOutcomesPerYearoptimalTreatment <- renderText({VOIResults$expectedOutcomesPerYearoptimalTreatment})
# output$implementationValueExists <- renderText({VOIResults$implementationValueExists})            # new output
# output$uncertaintyInCurrentEvidenceExists <- renderText({VOIResults$uncertaintyInCurrentEvidenceExists})
#output$probTreatment1isMax <- renderText({VOIResults$probTreatment1isMax })
#output$probTreatment2isMax <- renderText({VOIResults$probTreatment2isMax })
#output$probTreatment3isMax <- renderText({VOIResults$probTreatment3isMax })
#output$probTreatment4isMax <- renderText({VOIResults$probTreatment4isMax})
# output$popDuringResearch <- renderText({VOIResults$popDuringResearch})
# output$popAfterResearch <- renderText({VOIResults$popAfterResearch})
# output$popTotal <- renderText({VOIResults$popTotal })
# output$popDuringFeasResearch <- renderText({VOIResults$popDuringFeasResearch})       # feas outputs
# output$popDuringDefinitiveResearch <- renderText({VOIResults$popDuringDefinitiveResearch})       # feas outputs
# output$popAfterDefinitiveResearch <- renderText({VOIResults$popAfterDefinitiveResearch})        # feas outputs
# 
#output$valueOfResearchPerYear <- renderText({paste("value of research per year is",  VOIResults$valueOfResearchPerYear)})
# output$valueOfImplementationPerYear <- renderText({paste("value of implementation per year is", VOIResults$valueOfImplementationPerYear)})
# 
#output$tableProbabilityMax <- renderTable({VOIResults$tableProbabilityMaxDF}, include.rownames = FALSE)

#output$Cell_A <- renderText({VOIResults$Cell_A})
#output$Cell_C <- renderText({VOIResults$Cell_C})
#output$Cell_D <- renderText({VOIResults$Cell_D})
#output$maxvalueOfImplementation <- renderText({VOIResults$maxvalueOfImplementation})
#output$maxvalueOfResearch <- renderText({VOIResults$maxvalueOfResearch})
#output$healthOpportunityCostsOfResearch <-   renderText({VOIResults$healthOpportunityCostsOfResearch})
#output$expectedCostResearchFunder <-   renderText({paste("expected costs to research funder" ,VOIResults$expectedCostResearchFunder)})
#output$valueOfResearchWithCurrentImplementation <- renderText({paste("value of research with current implementation ",VOIResults$valueOfResearchWithCurrentImplementation)})
#output$valueOfResearchWithPerfectImplementation <- renderText({paste("Value of research with perfect implementation", VOIResults$valueOfResearchWithPerfectImplementation)})
#output$valueOfCertainResearchWithPerfectImplementation <- renderText({paste("value of research with certain definitive trial", VOIResults$valueOfCertainResearchWithPerfectImplementation)})
#output$ICER_ResearchWithCurrentImplementation <- renderText({paste("ICER with current info is",VOIResults$ICER_ResearchWithCurrentImplementation)})
#output$ICER_ResearchWithPerfectImplementation <- renderText({paste("ICER with perfect info is",VOIResults$ICER_ResearchWithPerfectImplementation)})
#output$valuePer15KResearchSpend <- renderText({paste("value per 15K research spend is",VOIResults$valuePer15KResearchSpend)})
#output$absoluteExpectedHealthOutcomesFromResearchProject <- renderText({paste("absolute expected outcomes from research project",VOIResults$absoluteExpectedHealthOutcomesFromResearchProject)})

#output$costResearchFunderFeas <- renderText({paste("cost funder feasibility ", input$costResearchFunderFeas)})
#output$costResearchFunderDefinitive <- renderText({paste("cost funder definitive", input$costResearchFunderDefinitive)})
#output$probabilityOfDefinitiveResearch <- renderText({paste("prob of definitive research ", input$ProbabilityOfDefinitiveResearch)})
#output$test1 <- renderText({VOIResults$test1})
#output$test2 <- renderText({VOIResults$test2})
#output$test3 <- renderText({VOIResults$test3})





