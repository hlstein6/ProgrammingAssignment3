## function best returns the name of the best hospital in the requested state
## for the requested

best <- function(state, outcome) {
	## read outcome data
  outcomeStats <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## subset outcome data frame for the columns needed
  subOutcomeStats <- outcomeStats[ , c(2, 7, 11, 17, 23)]
  colnames(subOutcomeStats) <- c("HospitalName", 
                                 "State", 
                                 "HospitalMortalityRateHeartAttack", 
                                 "HospitalMortalityRateHeartFailure", 
                                 "HospitalMortalityRatePneumonia")
  
	## Check that state and outcome are valid
  validOutcomeType <- c("heart attack", "heart failure", "pneumonia")
  validStateList <- unique(subOutcomeStats$State)
  if (is.element(state, validStateList) == FALSE) {
    print ("Invalid State")
    return()
  }
  if (is.element(outcome, validOutcomeType) == FALSE) {
    print ("Invalid Outcome")
    return()
  }
  
	## Subset the statistics by the requested State
  stateStats <- with(subOutcomeStats, 
                    subset(subOutcomeStats, 
                           select = c(HospitalName, 
                                      HospitalMortalityRateHeartAttack,
                                      HospitalMortalityRateHeartFailure,
                                      HospitalMortalityRatePneumonia), 
                           State == state))
  
  
  ## Return the hospital name with the lowest mortality rate for the requested outcome
  if (outcome == "heart attack") {
    stateStats <- subset(stateStats, select = c(HospitalName, HospitalMortalityRateHeartAttack), is.na(stateStats$HospitalMortalityRateHeartAttack) == FALSE)
    stateStats$HospitalMortalityRateHeartAttack <- as.numeric(stateStats$HospitalMortalityRateHeartAttack)
    ordered <- stateStats[order(stateStats$HospitalMortalityRateHeartAttack), ]
    ordered[1, 1]
  # return()
  }
  else if (outcome == "heart failure") {
    stateStats$HospitalMortalityRateHeartFailure <- as.numeric(stateStats$HospitalMortalityRateHeartFailure)
    ordered <- stateStats[order(stateStats$HospitalMortalityRateHeartFailure), ]
    ordered[1, 1]
  #  return()
  }
  else if (outcome == "pneumonia") {
    stateStats$HospitalMortalityRatePneumonia <- as.numeric(stateStats$HospitalMortalityRatePneumonia)
    ordered <- stateStats[order(stateStats$HospitalMortalityRatePneumonia), ]
    ordered[1, 1]
  #  return()
  }

}

rankhospital <- function(state, outcome, num) {
  ## read outcome data
  outcomeStats <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## subset outcome data frame for the columns needed
  subOutcomeStats <- outcomeStats[ , c(2, 7, 11, 17, 23)]
  colnames(subOutcomeStats) <- c("HospitalName", 
                                 "State", 
                                 "HospitalMortalityRateHeartAttack", 
                                 "HospitalMortalityRateHeartFailure", 
                                 "HospitalMortalityRatePneumonia")
  
  ## Check that state and outcome are valid
  validOutcomeType <- c("heart attack", "heart failure", "pneumonia")
  validStateList <- unique(subOutcomeStats$State)
  if (is.element(state, validStateList) == FALSE) {
    print ("Invalid State")
    return()
  }
  if (is.element(outcome, validOutcomeType) == FALSE) {
    print ("Invalid Outcome")
    return()
  }
  
  ## if num is "best" call best() function
  if (num == "best") 
    best(state, outcome)
  else if (num == "worst") {
    ## Subset the statistics by the requested State
    stateStats <- with(subOutcomeStats, 
                       subset(subOutcomeStats, 
                              select = c(HospitalName, 
                                         HospitalMortalityRateHeartAttack,
                                         HospitalMortalityRateHeartFailure,
                                         HospitalMortalityRatePneumonia), 
                              State == state))
    
    ## Return the hospital name with the lowest mortality rate for the requested outcome
    if (outcome == "heart attack") {
      stateStats <- subset(stateStats, select = c(HospitalName, HospitalMortalityRateHeartAttack), is.na(stateStats$HospitalMortalityRateHeartAttack) == FALSE)
      stateStats$HospitalMortalityRateHeartAttack <- as.numeric(stateStats$HospitalMortalityRateHeartAttack)
      ordered <- stateStats[order(stateStats$HospitalMortalityRateHeartAttack, decreasing = TRUE), ]
      ordered[1, 1]
    }
      # return() 
     else if (outcome == "heart failure") {
        stateStats <- subset(stateStats, select = c(HospitalName, HospitalMortalityRateHeartFailure), is.na(stateStats$HospitalMortalityRateHeartFailure) == FALSE)
        stateStats$HospitalMortalityRateHeartFailure <- as.numeric(stateStats$HospitalMortalityRateHeartFailure)
        ordered <- stateStats[order(stateStats$HospitalMortalityRateHeartFailure, decreasing = TRUE), ]
        ordered[1, 1]
        # return() 
     }
      else  if (outcome == "pneumonia") {
          stateStats <- subset(stateStats, select = c(HospitalName, HospitalMortalityRatePneumonia), is.na(stateStats$HospitalMortalityRatePneumonia) == FALSE)
          stateStats$HospitalMortalityRatePneumonia <- as.numeric(stateStats$HospitalMortalityRatePneumonia)
          ordered <- stateStats[order(stateStats$HospitalMortalityRatePneumonia, decreasing = TRUE), ]
          ordered[1, 1]
          # return() 
      }
    
  }
  else {
    #want the one at the rank number
    ## Subset the statistics by the requested State
    stateStats <- with(subOutcomeStats, 
                       subset(subOutcomeStats, 
                              select = c(HospitalName, 
                                         HospitalMortalityRateHeartAttack,
                                         HospitalMortalityRateHeartFailure,
                                         HospitalMortalityRatePneumonia), 
                              State == state))
    idx <- as.numeric(num)
    ## Return the hospital name with the lowest mortality rate for the requested outcome
    if (outcome == "heart attack") {
      stateStats <- subset(stateStats, select = c(HospitalName, HospitalMortalityRateHeartAttack), is.na(stateStats$HospitalMortalityRateHeartAttack) == FALSE)
      stateStats$HospitalMortalityRateHeartAttack <- as.numeric(stateStats$HospitalMortalityRateHeartAttack)
      ordered <- stateStats[order(stateStats$HospitalMortalityRateHeartAttack, stateStats$HospitalName), ]
      len <- length(ordered$HospitalMortalityRateHeartAttack)
      print(c("num=", num))
      print(c("len=", len))
      print(c("idx=", idx))
      if (idx > len) print("num is too high")
      else ordered[idx, 1]
    }
    else if (outcome == "heart failure") {
      stateStats <- subset(stateStats, select = c(HospitalName, HospitalMortalityRateHeartFailure), is.na(stateStats$HospitalMortalityRateHeartFailure) == FALSE)
      stateStats$HospitalMortalityRateHeartFailure <- as.numeric(stateStats$HospitalMortalityRateHeartFailure)
      ordered <- stateStats[order(stateStats$HospitalMortalityRateHeartFailure, stateStats$HospitalName), ]
      len <- length(ordered$HospitalMortalityRateHeartFailure)
      print(c("num=", num))
      print(c("len=", len))
      print(c("idx=", idx))
      if (idx > len) print("num is too high")
      else ordered[idx, 1]
    }
    else if (outcome == "pneumonia") {
      stateStats <- subset(stateStats, select = c(HospitalName, HospitalMortalityRatePneumonia), is.na(stateStats$HospitalMortalityRatePneumonia) == FALSE)
      stateStats$HospitalMortalityRatePneumonia <- as.numeric(stateStats$HospitalMortalityRatePneumonia)
      ordered <- stateStats[order(stateStats$HospitalMortalityRatePneumonia, stateStats$HospitalName), ]
      len <- length(ordered$HospitalMortalityRatePneumonia)
      print(c("num=", num))
      print(c("len=", len))
      print(c("idx=", idx))
      if (idx > len) print("num is too high")
      else ordered[idx, 1]
    }
  }
 } 