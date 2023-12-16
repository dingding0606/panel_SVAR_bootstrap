# ---------------------------------------------------------------------------- #
#                       Code for ECON 371 Final Project                        #
#                        Authors: Matthew Wu, Yufeng Wu                        #
#                             Date: Dec.16th, 2023                             #
#                                                                              #
# Acknowledgement: The simulation technique is inspired by Prof. Pedroni's     #
# article "Structural Panel VARs"                                              #
# Econometrics 2013, 2, 180-206; doi:10.3390/econometrics1020180               #
# ---------------------------------------------------------------------------- #

 
library("MASS")
library("dplyr")
library("Matrix")
library("openxlsx")

testData <- read.csv("~/Desktop/IR_to_composite_shock_R_60.csv") 
##print(testData[, 1])
##colOfCountryIndex = testData[, 1]
##length(colOfCountryIndex)
##colOfCountryIndex[1]
  
##rowForEachCountry <- testData[testData[, 1] == "112", ] 
   
 
############################################################################################

seperateImpulseResponseOfEachCountry = function(trueMean, impulseResponseTotalPeriod, totalNumberOfImpulseResponseCoefficients, totalNumberOfShocks) { ## ATTENTION: need to implement desired lag period for A 
     ##dimensionOfPanelData 
      columnOfCountryIndex = trueMean[, 1] ##extract country index/names, which is assumed to present in the first column
      bigListOfImpulseResponse <- list() 
  
     for (index in 1:length(columnOfCountryIndex)) {  ## assinging the row of each country to a list
       country  <- columnOfCountryIndex[index]
       rowForEachCountry <- trueMean[testData[, 1] == country, ]
       listOfImpulseResponseForEachCountry <- list()
       listOfImpulseResponseDividedByTimePeriod <- list() 
        
       for (multiple in 0:(totalNumberOfImpulseResponseCoefficients -1)) { 
         startingIndex = (multiple * impulseResponseTotalPeriod) + 2  ## THIS INDEX IS SO CONFUSING... 
         endingIndex = ((multiple + 1) * impulseResponseTotalPeriod) + 1 
         ##print( paste("DEBUGGING FOR STARTING INDEX", startingIndex, "DEBUGGING FOR ENDING INDEX ", endingIndex))
         impulseResponseCoefficientsFromZeroToEnd <- rowForEachCountry[startingIndex:endingIndex] 
         ##print(impulseResponseCoefficientsFromZeroToEnd)
         listOfImpulseResponseForEachCountry <- c(listOfImpulseResponseForEachCountry, impulseResponseCoefficientsFromZeroToEnd) 
         ##print(listOfImpulseResponseForEachCountry)
       ##listOfImpulseResponseForEachCountry[[as.character(countryIndex)]] <- rowForEachCountry
         }
       bigListOfImpulseResponse <- c(bigListOfImpulseResponse, listOfImpulseResponseForEachCountry)
     }
     return (bigListOfImpulseResponse) ##output a list of impulse responses grouped by countries
} 
##SAMPLE OUTPUT:
## { {ImpuseResponse_11 for country 1 from 1 to end of time period } , {ImpuseResponse_12 for country 1 from 1 to end of time period }                    
## {ImpuseResponse_21 for country 1 from 1 to end of time period }, ....., {ImpuseResponse_11 for country N from 1 to end of time period }        
## } 
 
##biglist <- seperateImpulseResponseOfEachCountry(testData, 20, 4, 2) 
 
##storingList <- list()
##for (time in 1:2){ 
 ## smallList <- list()
##  for (i in 1:4){ 
##    print(biglist[[i]][time])
##    smallList <-c(smallList, list(biglist[[i]][time]))
##    print("###########################")
 ## }
 ## storingList <- c(storingList, list(smallList))
##}
##storingList[[2]][[1]]

##testList <- list()
##for (index in 1:4){ 
##  testList <- c(testList, list(biglist[[index]])) 
##  }
########################################################################################################## 

divideImpulseResponsesIntoCountries = function(listOfImpulseResponseForAllCountries, totalNumberOfImpulseResponseCoefficients, numberOfCountries, impulseResponseTotalPeriod) { 
  ##numberOfCountries = length(listOfImpulseResponseForAllCountries) / totalNumberOfImpulseResponseCoefficients 
  numberOfCountries
  listStoringShcoksOfAllCountries <- list()
  startingIndex = 1
  endingIndex = impulseResponseTotalPeriod
  for (countryIndex in 1:numberOfCountries) {
    listOfShocksForASingleCountry <- list()
    for (ImpulseResponseCoefficientsAtIndex_ij in 1:totalNumberOfImpulseResponseCoefficients) { 
      listOfShocksForASingleCountry <- c(listOfShocksForASingleCountry, list(listOfImpulseResponseForAllCountries[startingIndex:endingIndex]))
      startingIndex = startingIndex + impulseResponseTotalPeriod
      endingIndex = endingIndex + impulseResponseTotalPeriod  
      }
    listStoringShcoksOfAllCountries <- c(listStoringShcoksOfAllCountries, list(listOfShocksForASingleCountry))
   }
   return (listStoringShcoksOfAllCountries)  ## group impulse respones by countries and put them into sublists 
}
##SAMPLE OUTPUT:
## { { {ImpuseResponse_11 for country 1 from 1 to end of time period } , {ImpuseResponse_12 for country 1 from 1 to end of time period }                    
## {ImpuseResponse_21 for country 1 from 1 to end of time period }, {ImpuseResponse_22 for country 1 from 1 to end of time period } }, ....., 
##   { {ImpuseResponse_11 for country N from 1 to end of time period }, .... {ImpuseResponse_22 for country N from 1 to end of time period } }
## } 
## THus, if we have N countries in the panel, we will output a list with N sublists, where each sublist contains all impulse response coefficients
## from time period 1 to end of the time period
##listOrganizedByCountries <- divideImpulseResponsesIntoCountries(biglist, 4, numberOfCountries = 20, impulseResponseTotalPeriod = 20) 

########################################################################################################## 

groupingAllCoefficientsOfImpulseResponseTogetherForSingleCountry = function(listOfImpulseResponseForASingleCountry, impulseResponseTotalPeriod) { 
  storingListOfCoefficientsOfIRForSingleCountry <- list()
    for (time in 1:impulseResponseTotalPeriod) {
        listOfImpulseResponseForSingleCountryAtEachTimePeriod <- list() ##A_t, for given time period t 
      for (impulseResponse in listOfImpulseResponseForASingleCountry) { 
        oneCoefficientOfACountryAtEachTimePeriod <- impulseResponse[[time]]
        listOfImpulseResponseForSingleCountryAtEachTimePeriod <- c(listOfImpulseResponseForSingleCountryAtEachTimePeriod, list(oneCoefficientOfACountryAtEachTimePeriod))
      }
   storingListOfCoefficientsOfIRForSingleCountry <- c(storingListOfCoefficientsOfIRForSingleCountry, list(listOfImpulseResponseForSingleCountryAtEachTimePeriod))
  }
  return (storingListOfCoefficientsOfIRForSingleCountry)
}
## expected input is an array with all impulse responses for a single country
## SAMPLE INPUT: 
##
## { {ImpuseResponse_11 for country 1 from 1 to end of time period } , {ImpuseResponse_12 for country 1 from 1 to end of time period },                     
##   {ImpuseResponse_21 for country 1 from 1 to end of time period }, {ImpuseResponse_22 for country 1 from 1 to end of time period } }
##
## SAMPLE OUTPUT:  (T denotes the impulseResponseTotalPeriod)
## { {ImpulseResponse_11_{t = 1}, ImpulseResponse_12_{t = 1}, ImpulseResponse_21_{t = 1}, ImpulseResponse_22_{t = 1} },                
##   {ImpulseResponse_11_{t = 2}, ImpulseResponse_12_{t = 2}, ImpulseResponse_21_{t = 2}, ImpulseResponse_22_{t = 2} }, 
##      ....,  
##    {ImpulseResponse_11_{t = T}, ImpulseResponse_12_{t = T}, ImpulseResponse_21_{t = T}, ImpulseResponse_22_{t = T}}  } 
 

##smaplelist <- groupingAllCoefficientsOfImpulseResponseTogetherForSingleCountry(listOrganizedByCountries[[1]], 20)
###########################################################################################################
 
groupingAllCoefficientsOfImpulseResponseTogetherForAllCountries = function(listOfImpulseResponsesOrganizedByCountries, impulseResponseTotalPeriod) {
  listStroingImpulseResponsesForAllCountriesGroupedByTimeIndex <- list()
  for (index in 1:length(listOfImpulseResponsesOrganizedByCountries)) {
    listToBePassedIn <- listOfImpulseResponsesOrganizedByCountries[[1]]
    listOfImpulseOfSingleCountryGroupedByTimeIndex <- groupingAllCoefficientsOfImpulseResponseTogetherForSingleCountry(listToBePassedIn, impulseResponseTotalPeriod)
    listStroingImpulseResponsesForAllCountriesGroupedByTimeIndex <- c(listStroingImpulseResponsesForAllCountriesGroupedByTimeIndex, list(listOfImpulseOfSingleCountryGroupedByTimeIndex))
  }
  return (listStroingImpulseResponsesForAllCountriesGroupedByTimeIndex)
}
##testOutput <- groupingAllCoefficientsOfImpulseResponseTogetherForAllCountries(listOrganizedByCountries, 20)

## function expects the input of list of impulse responses organized by countries (can simply plug in output from divideImpulseResponsesIntoCountries)
## SAMPLE INPUT: 
## { { {ImpuseResponse_11 for country 1 from 1 to end of time period } , {ImpuseResponse_12 for country 1 from 1 to end of time period },                     
##     {ImpuseResponse_21 for country 1 from 1 to end of time period },  {ImpuseResponse_22 for country 1 from 1 to end of time period } }, ....., 
##   { {ImpuseResponse_11 for country N from 1 to end of time period }, .... , {ImpuseResponse_22 for country N from 1 to end of time period } }
## } 
##
## SAMPLE OUTPUT: 
## First Level List: { C_1, ...., C_i,..., C_N} where i denotes country index 
## Second Level List: For each C_i, we have {A_1, A_2, ..., A_t, ..., A_T}  
## where t denotes the time index, and A_t takes list data structure and A is list containing all impulse responses for given time period
## Third level List: if we have the following matrix, 
##  {A_11, A_12 //
##   A_21, A_22}
##  A_t stores it into a list like {A_11, A_12, A_21, A_22}
##

########################################################################################################### 
##asssuming dimension of the response vector for each country 
## is equal to the dimension of shocks

organizeDataIntoMatricies = function(listOfImpulseResponsesForAllCountriesGroupedByTimeIndex, numberOfShocks) { 
  listStoringCoefficientMatrixAForAllCountries <- list()
  for (impulseResponseOfEachCountry in listOfImpulseResponsesForAllCountriesGroupedByTimeIndex) { ## accessing first level list 
    listOfImpulseResponsesGroupedByCountry <- list()
      for (impulseResponseOfEachCountryAtTime_t in impulseResponseOfEachCountry) { 
        ##convertedListToVector <- unlist(impulseResponseOfEachCountryAtTime_t)
        coefficentMatrix <- matrix(unlist(impulseResponseOfEachCountryAtTime_t), nrow = numberOfShocks, ncol = numberOfShocks, byrow = TRUE)
        listOfImpulseResponsesGroupedByCountry <- c(listOfImpulseResponsesGroupedByCountry, list(coefficentMatrix)) 
      } 
    listStoringCoefficientMatrixAForAllCountries <- c(listStoringCoefficientMatrixAForAllCountries, list(listOfImpulseResponsesGroupedByCountry))
    }
  return(listStoringCoefficientMatrixAForAllCountries)
}
##matrixListOutput <- organizeDataIntoMatricies(testOutput, 2)


################################################################################################################### 
##scalingMatrixC = matrix(NA, nrow = 3, ncol = 3, byrow = TRUE)
##print(scalingMatrixC) 

createScalingMatrixCForAllCountries = function(dimensionM, numberOfCountries) { 
  listStoringTheScalingMatrixC <- list()
  for (iterationForEachCountry in 1:numberOfCountries) { 
    scalingMatrixC = matrix(NA, nrow = dimensionM, ncol = dimensionM, byrow = TRUE)
    for (rowIndex in 1:dimensionM) { 
      for (columnIndex in 1:dimensionM) {
        scalingMatrixC[rowIndex, columnIndex] <- runif(1, min = -0.15, max = 0.15) 
          }
    }
    ##print(scalingMatrixC)
    listStoringTheScalingMatrixC <- c(listStoringTheScalingMatrixC, list(scalingMatrixC)) 
  }
  return (listStoringTheScalingMatrixC)
}

##testList <- createScalingMatrixCForAllCountries(2, 20)
##mat <- testList[[1]]
##mat[1, 1]

################################################################################################################### 

calculateTheSampleAverageOfScalingMatrixCForAllCountries = function(listOfScalingMatrixC, dimensionM, numberOfCountries) { 
  meanAverageMatrix <- matrix(NA, nrow = dimensionM, ncol = dimensionM, byrow = TRUE)
  for (rowIndex in 1:dimensionM) { 
    for (columnIndex in 1:dimensionM) { 
      sumOfAllEntriesAtGivenRowIndexAndColumnIndex = 0 
      for(indexForMatrixC in 1:length(listOfScalingMatrixC)) { 
         currentScalingMatrixC = listOfScalingMatrixC[[indexForMatrixC]]
         sumOfAllEntriesAtGivenRowIndexAndColumnIndex =sumOfAllEntriesAtGivenRowIndexAndColumnIndex + currentScalingMatrixC[rowIndex, columnIndex]
      }
      sampleMean = sumOfAllEntriesAtGivenRowIndexAndColumnIndex / numberOfCountries
      meanAverageMatrix[rowIndex, columnIndex] <- sampleMean
       }
  }
  return (meanAverageMatrix)
}

##print("DEBUGGING FOR SAMPLE AVERAGE")
##sampleAverageC <- calculateTheSampleAverageOfScalingMatrixCForAllCountries(testList, 2, 20) 

########################################################################## 

returnTheRecenteredScalingMatrixC = function(originalListOfScalingMatrixCBeforeRecentering, sampleAverageMatrix) { 
   listOfScalingMatrixCAfterRecentering = list()
   for (index in 1:length(originalListOfScalingMatrixCBeforeRecentering)) { 
      recenteredMatrix = originalListOfScalingMatrixCBeforeRecentering[[index]] - sampleAverageMatrix
      listOfScalingMatrixCAfterRecentering = c(listOfScalingMatrixCAfterRecentering, list(recenteredMatrix))
     }
  return (listOfScalingMatrixCAfterRecentering) 
}
## recenteredMatrixC <- returnTheRecenteredScalingMatrixC(testList, sampleAverageC)


##########################################################################
## 
## i believe the number of true mean estimated for each country should be equal to indexS... 
##  check if numberOfStepsOfImpulseResponseToBeEstimated is needed or not 
##

simulateTheStructuralShockCoefficientA = function(trueMean, listOfRecenteredScalingMatrix, numberOfStepsOfImpulseResponseToBeEstimated, numberOfCountries, delta, gamma) { 
  listOfSimulatedStructuralCoefficientAForAllCountries <- list()
  for (countryIndex in 1:numberOfCountries) { 
    listOfSimulatedStructuralCoefficientAForASingleCountry <- list() 
    trueMeanForOneSingleCountry <- trueMean[[countryIndex]]
    for (indexS in 1:numberOfStepsOfImpulseResponseToBeEstimated) {
      scalar <- exp(- (indexS - delta)^2 / gamma) 
      simulatedStructuralCoefficientA <- trueMeanForOneSingleCountry[[indexS]] + listOfRecenteredScalingMatrix[[countryIndex]] * scalar
      listOfSimulatedStructuralCoefficientAForASingleCountry <- c(listOfSimulatedStructuralCoefficientAForASingleCountry, list(simulatedStructuralCoefficientA))
        }
    listOfSimulatedStructuralCoefficientAForAllCountries <- c(listOfSimulatedStructuralCoefficientAForAllCountries, list(listOfSimulatedStructuralCoefficientAForASingleCountry))
    }
  return (listOfSimulatedStructuralCoefficientAForAllCountries)
}

## TODO: test cases needed here
##
##

##simulatedCoefficientA <- simulateTheStructuralShockCoefficientA(trueMeanFromCSV, recenteredMatrixC, 20, 20, delta = 8, gamma = 35)
###########################################################################  

drawingEpsilonBar_it = function(numberOfStepsOfImpulseResponseToBeEstimated, numberOfCountries, dimensionM) {
   listSotringEpsilonBarForAllCountries <- list()
   meanForMultivariteNormal = rep(0, dimensionM)
   varianceForMultivariteNormal = diag(dimensionM) ## check if this paremter should be covariance or not 
   for (countryIndex in 1:numberOfCountries) { 
      listStoringEpsilonBarForEachCountry <- list()
       for (numOfSteps in 1:numberOfStepsOfImpulseResponseToBeEstimated) {
         sampleAllEpsilonBarForOneCountry <- mvrnorm(1, meanForMultivariteNormal, varianceForMultivariteNormal)
        epsilonBar_it <- matrix(sampleAllEpsilonBarForOneCountry)
        listStoringEpsilonBarForEachCountry <- c(listStoringEpsilonBarForEachCountry, list(epsilonBar_it))
      }
      listSotringEpsilonBarForAllCountries <- c(listSotringEpsilonBarForAllCountries, list(listStoringEpsilonBarForEachCountry))
      }
  return (listSotringEpsilonBarForAllCountries)
}

## TODO: test cases needed
## 
##epsilonBar_it <- drawingEpsilonBar_it(20, 20,2)
###########################################################################

## TODO: need implement idioscryatic shocks simulator, but need the loadings 
## input diagonal matrix of user's choice, note that each entry in model Lambda
## should be in the bounded interval [0, 1]
##
creatingLambdaForAllCountries = function(numberOfCountries, dimensionM, userInputedDiagonalMatrixLambda, listOfLMax) { 
  listOfLambdaForAllCountries <- list()
    for (countryIndex in 1:numberOfCountries) { 
    lambdaMatrixForOneSingleCountry <- diag(dimensionM)
    for (matrixDiagonalIndex in 1:dimensionM){ 
      while ( !(0 < lambdaMatrixForOneSingleCountry[matrixDiagonalIndex, matrixDiagonalIndex] & lambdaMatrixForOneSingleCountry[matrixDiagonalIndex, matrixDiagonalIndex] < 1) ){  
       Lmax <- listOfLMax[[matrixDiagonalIndex]]
       lambdaMatrixForOneSingleCountry[matrixDiagonalIndex, matrixDiagonalIndex] <- runif(1, min = userInputedDiagonalMatrixLambda[matrixDiagonalIndex, matrixDiagonalIndex] - Lmax, max = userInputedDiagonalMatrixLambda[matrixDiagonalIndex, matrixDiagonalIndex] + Lmax)
        }
       }
    listOfLambdaForAllCountries <- c(listOfLambdaForAllCountries, list(lambdaMatrixForOneSingleCountry))
    }
   return (listOfLambdaForAllCountries)
}

## test case: 
##set.seed(123) 
## diagonal_matrix <- Diagonal(2, c(0.4, 0.3)) 
 ##print(diagonal_matrix)
## listOfLMax <- list()
## max1 <- 0.25
#### max2 <- 0.35
## listOfLMax <- c(listOfLMax,list(max1, max2))
##testLambaMatrix <- creatingLambdaForAllCountries(20, 2, diagonal_matrix, listOfLMax)

################################################################################

calculateTheSampleAverageOfLambdaMatrixForAllCountries = function(listOfLambdaForAllCountries, dimensionM) { 
  meanAverageMatrix <- matrix(0, nrow = dimensionM, ncol = dimensionM)
   for (diagonalIndex in 1:dimensionM) { 
      sumOfAllEntriesAtGivenRowIndexAndColumnIndex = 0
      for(countryIndex in 1:length(listOfLambdaForAllCountries)) { 
        currentLambdaMatrixForOneSingleCountry = listOfLambdaForAllCountries[[countryIndex]]
        sumOfAllEntriesAtGivenRowIndexAndColumnIndex =sumOfAllEntriesAtGivenRowIndexAndColumnIndex + currentLambdaMatrixForOneSingleCountry[diagonalIndex, diagonalIndex]
      }
      sampleMean = sumOfAllEntriesAtGivenRowIndexAndColumnIndex / length(listOfLambdaForAllCountries)
      meanAverageMatrix[diagonalIndex, diagonalIndex] <- sampleMean
  }
  return (meanAverageMatrix)
}
##testcase:
## testSampleAverageForLmabda <- calculateTheSampleAverageOfLambdaMatrixForAllCountries(testLambaMatrix, 2)

###################################################################################

recenterLambdaMatrix = function(listOfLambdaForAllCountries, dimensionM, meanAverageMatrix){ 
  listOfLambdaForAllCountriesAfterRecentring <- list() 
  for (countryIndex in 1:length(listOfLambdaForAllCountries)) { 
    currentLambda <- listOfLambdaForAllCountries[[countryIndex]]
    for (diagonalIndex in 1:dimensionM){ 
      currentLambda[diagonalIndex, diagonalIndex] <- currentLambda[diagonalIndex, diagonalIndex] - meanAverageMatrix[diagonalIndex, diagonalIndex]
    }
    listOfLambdaForAllCountriesAfterRecentring <- c(listOfLambdaForAllCountriesAfterRecentring, list(currentLambda))
  }
  return (listOfLambdaForAllCountriesAfterRecentring) 
}

## test case: 
##recenteringLambdaMatrix <- recenterLambdaMatrix(testLambaMatrix, 2, testSampleAverageForLmabda) ### DOES NEGATIVE CORRELATION MAKE SENSE

#####################################################################################

creatingEpsilonTilde_it = function(listOfRecenteredLambdaForAllCountries, numberOfStepsOfImpulseResponseToBeEstimated, dimensionM) {
  listOfEpsilonTilde_itForAllCountries <- list()
  meanForMultivariteNormal = rep(0, dimensionM) 
  for (countryIndex in 1:length(listOfRecenteredLambdaForAllCountries)) { 
    listOfEpsilonTilde_itForOneIndividualCountry <- list()
    loadingMatrixLambda <- listOfRecenteredLambdaForAllCountries[[countryIndex]]
    varianceCovarianceMatrixForOneSingleCountry <- diag(1, dimensionM) - loadingMatrixLambda %*% t(loadingMatrixLambda)
     for (numberOfStep in 1:numberOfStepsOfImpulseResponseToBeEstimated){ 
       sampleAllEpsilonTilde <- mvrnorm(1, meanForMultivariteNormal, varianceCovarianceMatrixForOneSingleCountry)
      epsilonBar_it_atNumberOfStep <- matrix(sampleAllEpsilonTilde)
      listOfEpsilonTilde_itForOneIndividualCountry <- c(listOfEpsilonTilde_itForOneIndividualCountry, list(epsilonBar_it_atNumberOfStep))
    }
    listOfEpsilonTilde_itForAllCountries <- c(listOfEpsilonTilde_itForAllCountries, list(listOfEpsilonTilde_itForOneIndividualCountry))
    }
   return (listOfEpsilonTilde_itForAllCountries) 
}


##############################################################################################

calculatingSimulatedValuesOfResponse = function (epsilonBar_itForALLCountries, recenteredLoadingMatrix, epsilonTilde_itForAllCountries, totalStepsOfImpulseResponse, structuralCoefficientA){ 
   numberOfCountries <- length(epsilonBar_itForALLCountries) 
   compositeShockForAllCountries <- list()
   
   for (countryIndex in 1:numberOfCountries) { 
     epsilonBar_itForOneCountry <- epsilonBar_itForALLCountries[[countryIndex]]  
     epsilonTilde_itForOneCountry <- epsilonTilde_itForAllCountries[[countryIndex]]
     listOfCompositeShockForOneCountry <- list()
     for (timeIndex in 1:totalStepsOfImpulseResponse){ 
       compositeShockForOneCountry = recenteredLoadingMatrix[[countryIndex]] %*% epsilonBar_itForOneCountry[[timeIndex]] + epsilonTilde_itForOneCountry[[timeIndex]]
       listOfCompositeShockForOneCountry <- c(listOfCompositeShockForOneCountry, list(compositeShockForOneCountry))
     }
     compositeShockForAllCountries <- c(compositeShockForAllCountries, list(listOfCompositeShockForOneCountry))
   } 
  
   listOfEveryEpsilonMultipliedByAForAllCountries <- list()
   for (countryIndex in 1:numberOfCountries) { 
     compositeShockForSingleCountry <- compositeShockForAllCountries[[countryIndex]]
     simluatedMatrixAForEachCountry <- structuralCoefficientA[[countryIndex]]
     listOfShockMultipliedByAForEachCountry <- list()
     
     for (timeIndexForA in 1:totalStepsOfImpulseResponse){ 
       listOfShockMultipliedByA <- list()
       for (timeIndex in 1:totalStepsOfImpulseResponse) { 
         eachEpsilonMultipliedByAllA  <- simluatedMatrixAForEachCountry[[timeIndexForA]] %*% compositeShockForSingleCountry[[timeIndex]]
         listOfShockMultipliedByA <- c(listOfShockMultipliedByA, list(eachEpsilonMultipliedByAllA))
       }
       listOfShockMultipliedByAForEachCountry <- c(listOfShockMultipliedByAForEachCountry, list(listOfShockMultipliedByA))
     } 
     listOfEveryEpsilonMultipliedByAForAllCountries <- c(listOfEveryEpsilonMultipliedByAForAllCountries, list(listOfShockMultipliedByAForEachCountry))
    }
   return (list(coefficientsMultipliedByAllComposite = listOfEveryEpsilonMultipliedByAForAllCountries, compositeShocks = compositeShockForAllCountries))
  }
##finalOutputOfSimulatedResponses <- calculatingSimulatedValuesOfResponse(epsilonBar_it,recenteringLambdaMatrix,listOfEpsilonTilde_itForALLCountries, 20)

calculatingSimulatedValuesOfResponse_helper = function(allEpsilonAndCoefficientA, orderOfLag, listOfCompositeShocks, totalImpulseResponse, numberOfCountries) { 
  listOfResponseForAllCountries <- list()
  for (countryIndex in 1:numberOfCountries) { 
      listOfResponseForSingleCountry <- list() ##create list for each country 
      epsilonAndCoefficentAForOneCountry <- allEpsilonAndCoefficientA[[countryIndex]]
      compositeShockForOneCountry <- listOfCompositeShocks[[countryIndex]]
      sumAt = 0
      for (t in 1:totalImpulseResponse) {
        sumAt  =  sumAt + compositeShockForOneCountry[[t]]
       for (j in 0:orderOfLag) {
         if (t - j > 0) {
           sumAt <- sumAt + epsilonAndCoefficentAForOneCountry[[j + 1]][[t - j]]
           }
       }
        listOfResponseForSingleCountry <- c(listOfResponseForSingleCountry, list(sumAt))
     }
     listOfResponseForAllCountries <- c(listOfResponseForAllCountries, list(listOfResponseForSingleCountry))  
  }
  return (listOfResponseForAllCountries)
}
 ############################################# USER INPUT FUNCTION ########################################################
set.seed(123) 
gettingTrueMeanAsMatrixFromData = function(trueMean, impulseResponseTotalPeriod, totalNumberOfImpulseResponseCoefficients, numberOfShocksIdentified, totalNumberOfCountries) { 
  listOfImpulseResponseForAllCountries <- seperateImpulseResponseOfEachCountry(trueMean, impulseResponseTotalPeriod, totalNumberOfImpulseResponseCoefficients, numberOfShocksIdentified)
  listOrganizedByCountries <- divideImpulseResponsesIntoCountries(listOfImpulseResponseForAllCountries, totalNumberOfImpulseResponseCoefficients, totalNumberOfCountries, impulseResponseTotalPeriod)  
  listOfGroupedCoefficientsByCountries <- groupingAllCoefficientsOfImpulseResponseTogetherForAllCountries(listOrganizedByCountries, impulseResponseTotalPeriod)
  result <- organizeDataIntoMatricies(listOfGroupedCoefficientsByCountries, numberOfShocksIdentified)  
  return (result)
}
trueMeanFromCSV <- gettingTrueMeanAsMatrixFromData(trueMean = testData, impulseResponseTotalPeriod = 60, totalNumberOfImpulseResponseCoefficients = 4, numberOfShocksIdentified = 2, totalNumberOfCountries= 20)


simluateDataFromTrueMean = function(trueMean, dimensionM, impulseResponseTotalPeriod, numberOfCountries, delta, gamma){ 
  originalListOfScalingMatrixC <- createScalingMatrixCForAllCountries(dimensionM, numberOfCountries)
  sampleAverageMatrix <- calculateTheSampleAverageOfScalingMatrixCForAllCountries(originalListOfScalingMatrixC, dimensionM, numberOfCountries) 
  recenteredScalingMatrix  <- returnTheRecenteredScalingMatrixC(originalListOfScalingMatrixC, sampleAverageMatrix)  
  diagonal_matrix <- Diagonal(2, c(0.3, 0.2))  ## true diagonal matrix 
  listOfLMax <- list()
  max1 <- 0.3
  max2 <- 0.25
  listOfLMax <- c(listOfLMax,list(max1, max2))
  originalListOfLambdaMatrix <- creatingLambdaForAllCountries(numberOfCountries, dimensionM, diagonal_matrix, listOfLMax) 
  sampleAverageMatrixLambda <- calculateTheSampleAverageOfLambdaMatrixForAllCountries(originalListOfLambdaMatrix, dimensionM)
  recenterLambda <- recenterLambdaMatrix(originalListOfLambdaMatrix, dimensionM, sampleAverageMatrixLambda) 
  
  structuralCoefficientA <- simulateTheStructuralShockCoefficientA(trueMean, recenteredScalingMatrix, impulseResponseTotalPeriod, numberOfCountries, delta, gamma)  
  epsilonBar_it <- drawingEpsilonBar_it(impulseResponseTotalPeriod, numberOfCountries, dimensionM) 
  epsilonTilde_it <- creatingEpsilonTilde_it(recenterLambda, impulseResponseTotalPeriod, dimensionM) 
  simulated <- calculatingSimulatedValuesOfResponse(epsilonBar_it, recenterLambda, epsilonTilde_it, impulseResponseTotalPeriod, structuralCoefficientA) 
  result <- calculatingSimulatedValuesOfResponse_helper(simulated$coefficientsMultipliedByAllComposite, 2, simulated$compositeShocks, impulseResponseTotalPeriod, numberOfCountries)
  return (result)
}

simulatedData <- simluateDataFromTrueMean(trueMean = trueMeanFromCSV, dimensionM = 2, numberOfCountries = 20, impulseResponseTotalPeriod = 60, delta = 15, gamma = 30) 

writeDataIntoCSV = function(data, numberOfCountries, dimensionM, impulseResponseTotalPeriod) {
  allData <- NULL
  
  for (countryIndex in 1:numberOfCountries) { 
    dataForIndividualCountry <- data[[countryIndex]]
    
    for (timeIndex in 1:impulseResponseTotalPeriod){ 
      dataAtGivenTimePeriod <- dataForIndividualCountry[[timeIndex]] 
        # Convert the vector to a single-row data frame
        dataForAppending <- as.data.frame(t(dataAtGivenTimePeriod))
        colnames(dataForAppending) <- paste("Var", seq_along(dataAtGivenTimePeriod), sep = "_")
        
        # Add Country, TimePeriod, and ResponseIndex
        dataForAppending$Country <- countryIndex
        dataForAppending$TimePeriod <- timeIndex
        # Append to allData
        allData <- rbind(allData, dataForAppending)
  
    }
  }
  
  write.csv(allData, "_countries_data.csv", row.names = FALSE)
}
writeDataIntoCSV(simulatedData, numberOfCountries = 20, dimensionM = 2,impulseResponseTotalPeriod= 60)
