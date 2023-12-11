## COMMENT: 
## epsilonBar_it : this gives member-specific idiosyncratic white noise structural shocks
##
## epsilonTilde_it: this gives common structural shock
##
## coefficentA: this gives the individual coeeficent for given time period t
##
## trueMean: this gives the true impuluse-response function, which allows bootstrapping 
## evaluation. 
## 
## scalingMatrixC: this scales downward or upward, each entry c_i ~ MultivariateUniform(-c, c) 
##
## dimensionM: this gives the dimension of vector containing the  
## response variables, Delta y_m, it. 
##
##
##
##
##
##


library("MASS")
library("dplyr")

testData <- read.csv("~/Desktop/R_code_demo_psvar_pedroni2013/IR_to_common_shock_R.csv") 
print(testData[, 1])
colOfCountryIndex = testData[, 1]
length(colOfCountryIndex)
colOfCountryIndex[1]
  
for (countryIndex in colOfCountryIndex) { 
    rowForEachCountry <- testData[testData[, 1] == countryIndex, ] 
    print(paste("DEBUGGING FOR COUNTRY INDEX ", countryIndex, "\n")) 
    print(rowForEachCountry)           
    }
  
  
seperateImpulseResponseOfEachCountry = function(trueMean, impulseResponseTotalPeriod, totalNumberOfImpulseResponseCoefficients, totalNumberOfShocks) { ## ATTENTION: need to implement desired lag period for A 
     ##dimensionOfPanelData 
      columnOfCountryIndex = trueMean[, 1] ##extract country index/names, which is assumed to present in the first column
      bigListOfImpulseResponse <- list() 
  
     for (index in 1:length(columnOfCountryIndex)) {  ## assinging the row of each country to a list
       countryIndex <- columnOfCountryIndex[index]
       rowForEachCountry <- testData[testData[, 1] == countryIndex, ]
       listOfImpulseResponseForEachCountry <- list()
       listOfImpulseResponseDividedByTimePeriod <- list() 
        
       for (multiple in 0 :(totalNumberOfImpulseResponseCoefficients -1)) { 
         startingIndex = (multiple * impulseResponseTotalPeriod) + 2  ## THIS INDEX IS SO CONFUSING... 
         endingIndex = (multiple + 1) * impulseResponseTotalPeriod + 1 
         ## print( paste("DEBUGGING FOR STARTING INDEX", startingIndex, "DEBUGGING FOR ENDING INDEX ", endingIndex))
         impulseResponseCoefficientsFromZeroToEnd <- list(rowForEachCountry[startingIndex:endingIndex])
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
 
biglist <- seperateImpulseResponseOfEachCountry(testData, 36, 4, 2) 

 
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
divideImpulseResponsesIntoCountries = function(listOfImpulseResponseForAllCountries, totalNumberOfImpulseResponseCoefficients) { 
  numberOfCountries = length(listOfImpulseResponseForAllCountries) / totalNumberOfImpulseResponseCoefficients 
  listStoringShcoksOfAllCountries <- list()
  
  startingIndex = 1
  endingIndex = totalNumberOfImpulseResponseCoefficients
  for (countryIndex in 1:numberOfCountries) {
    listOfShocksForASingleCountry <- list()
    listOfShocksForASingleCountry <- c(listOfShocksForASingleCountry, list(listOfImpulseResponseForAllCountries[startingIndex:endingIndex]))
    listStoringShcoksOfAllCountries <- c(listStoringShcoksOfAllCountries, listOfShocksForASingleCountry)
    startingIndex = startingIndex + totalNumberOfImpulseResponseCoefficients
    endingIndex = totalNumberOfImpulseResponseCoefficients + totalNumberOfImpulseResponseCoefficients
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
listOrganizedByCountries <- divideImpulseResponsesIntoCountries(biglist, 4) 

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
 

## smaplelist <- groupingAllCoefficientsOfImpulseResponseTogetherForSingleCountry(listOrganizedByCountries[[1]], 36)
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
testOutput <- groupingAllCoefficientsOfImpulseResponseTogetherForAllCountries(listOrganizedByCountries, 36)

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
    print(length(listOfImpulseResponsesForAllCountriesGroupedByTimeIndex))
      for (impulseResponseOfEachCountryAtTime_t in impulseResponseOfEachCountry) { 
        convertedListToVector <- unlist(impulseResponseOfEachCountryAtTime_t)
        coefficentMatrix <- matrix(convertedListToVector, nrow = numberOfShocks, ncol = numberOfShocks, byrow = TRUE)
        listOfImpulseResponsesGroupedByCountry <- c(listOfImpulseResponsesGroupedByCountry, list(coefficentMatrix)) 
      } 
    listStoringCoefficientMatrixAForAllCountries <- c(listStoringCoefficientMatrixAForAllCountries, list(listOfImpulseResponsesGroupedByCountry))
    }
  return(listStoringCoefficientMatrixAForAllCountries)
}
matrixListOutput <- organizeDataIntoMatricies(testOutput, 2)


################################################################################################################### 
scalingMatrixC = matrix(NA, nrow = 3, ncol = 3, byrow = TRUE)
print(scalingMatrixC) 

createScalingMatrixCForAllCountries = function(dimensionM, numberOfCountries) { 
  listStoringTheScalingMatrixC <- list()
  for (iterationForEachCountry in 1:numberOfCountries) { 
    scalingMatrixC = matrix(NA, nrow = dimensionM, ncol = dimensionM, byrow = TRUE)
    for (rowIndex in 1:dimensionM) { 
      for (columnIndex in 1:dimensionM) {
        scalingMatrixC[rowIndex, columnIndex] <- runif(1, min = -10, max = 10) 
          }
    }
    print(scalingMatrixC)
    listStoringTheScalingMatrixC <- c(listStoringTheScalingMatrixC, list(scalingMatrixC)) 
  }
  return (listStoringTheScalingMatrixC)
}

testList <- createScalingMatrixCForAllCountries(3, 2)
mat <- testList[[1]]
mat[1, 1]
################################################################################################################### 

calculateTheSampleAverageOfScalingMatrixCForAllCountries = function(listOfScalingMatrixC, dimensionM, numberOfCountries) { 
  meanAverageMatrix <- matrix(NA, nrow = dimensionM, ncol = dimensionM, byrow = TRUE)
  for (rowIndex in 1:dimensionM) { 
    for (columnIndex in 1:dimensionM) { 
      sumOfAllEntriesAtGivenRowIndexAndColumnIndex = 0 
      for(iterationForEachScalingMatrixC in 1:length(listOfScalingMatrixC)) { 
         currentScalingMatrixC = listOfScalingMatrixC[[1]]
         sumOfAllEntriesAtGivenRowIndexAndColumnIndex =sumOfAllEntriesAtGivenRowIndexAndColumnIndex + currentScalingMatrixC[rowIndex, columnIndex]
      }
      sampleMean = sumOfAllEntriesAtGivenRowIndexAndColumnIndex / numberOfCountries
      meanAverageMatrix[rowIndex, columnIndex] <- sampleMean
       }
  }
  return (meanAverageMatrix)
}

print("DEBUGGING FOR SAMPLE AVERAGE")
calculateTheSampleAverageOfScalingMatrixCForAllCountries(testList, 3, 2) 

########################################################################## 

returnTheRecenteredScalingMatrixC = function(originalListOfScalingMatrixCBeforeRecentering, sampleAverageMatrix) { 
   listOfScalingMatrixCAfterRecentering = list()
   for (index in 1:length(originalListOfScalingMatrixCBeforeRecentering)) { 
      recenteredMatrix = originalListOfScalingMatrixCBeforeRecentering[index] - sampleAverageMatrix
      listOfScalingMatrixCAfterRecentering[index] = recenteredMatrix
     }
  return (listOfScalingMatrixCAfterRecentering) 
}

##########################################################################
## 
## i believe the number of true mean estimated for each country should be equal to indexS... 
##  check if numberOfStepsOfImpulseResponseToBeEstimated is needed or not 
##

simulateTheStructuralShockCoefficientA = function(trueMean, listOfRecenteredScalingMatrix, numberOfStepsOfImpulseResponseToBeEstimated, numberOfCountries, delta, gamma) { 
  listOfSimulatedStructuralCoefficientAForAllCountries <- list()
  for (countryIndex in 1:numberOfCountries) { 
    listOfSimulatedStructuralCoefficientAForASingleCountry <- list() 
    for (indexS in 1:numberOfStepsOfImpulseResponseToBeEstimated) {
      simulatedStructuralCoefficientA <- trueMean[indexS] + listOfRecenteredScalingMatrix[] * exp(- (indexS - delta)^2 / gamma) 
      listOfSimulatedStructuralCoefficientAForASingleCountry <- c(listOfSimulatedStructuralCoefficientAForASingleCountry, list(simulatedStructuralCoefficientA))
        }
    listOfSimulatedStructuralCoefficientAForASingleCountry <- c(listOfSimulatedStructuralCoefficientAForASingleCountry, list(listOfSimulatedStructuralCoefficientAForASingleCountry))
    }
  return (listOfSimulatedStructuralCoefficientAForAllCountries)
}

## TODO: test cases needed here
##
##

########################################################################### 

drawingEpsilonBar_it = function(numberOfStepsOfImpulseResponseToBeEstimated, numberOfCountries, dimensionM) {
   listSotringEpsilonBarForAllCountries <- list()
   meanForMultivariteNormal = rep(0, dimensionM)
   varianceForMultivariteNormal = diag(dimensionM)
   for (countryIndex in 1:numberOfCountries) { 
      listStoringEpsilonBarForEachCountry <- list()
      for (stepSforImpulseResponseOfGivenCountry in 1:numberOfStepsOfImpulseResponseToBeEstimated) { 
        epsilonBar_it <- rep(NA, dimensionM)
        epsilonBar_it <- mvrnorm(M, meanForMultivariteNormal, varianceForMultivariteNormal)
        listStoringEpsilonBarForEachCountry <- c(listStoringEpsilonBarForEachCountry, list(epsilonBar_it))
      }
      listSotringEpsilonBarForAllCountries <- c(listSotringEpsilonBarForAllCountries, list(listStoringEpsilonBarForEachCountry))
      }
  return (listSotringEpsilonBarForAllCountries)
}

## TODO: test cases needed
## 

###########################################################################

## TODO: need implement idioscryatic shocks simulator, but need the loadings 




  ## coefficentA_simulated <- matrix(0, nrow = 3, ncol = 4) ## TO BE REVISED 
  ## row is the number of countries in the panel  
  ## column is the number of shocks studied
  
  ##Simulate artifical impulse response coefficients for each country  
  #### for (indexForEachCountry in 1:numberOfCountries){ 
  ####   coefficentA_simulated[indexForEachCountry] = trueMean + e^(-1/2) * scalingMatrixC[indexForEachCountry]
  #### }
  ##End of simulation

