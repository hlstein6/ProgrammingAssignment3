## ranks hospitals by state at a particual level, best, worst or a ranking

rankall <- function(outcome, num) {
  
  #check that outcome is valid value
  validOutcomeType <- c("heart attack", "heart failure", "pneumonia")
  if (is.element(outcome, validOutcomeType) == FALSE) {
    print ("Invalid Outcome")
    return()
  }
  
	## read outcome data
  outcomeStats <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## subset outcome data frame for the columns needed
  if (outcome == "heart attack") {
    subOutcomeStats <- outcomeStats[ , c(2, 7, 11)]
    colnames(subOutcomeStats) <- c("HospitalName", "State", "OutcomeMortalityRate")
  }
  else if (outcome == "heart failure") {
    subOutcomeStats <- outcomeStats[ , c(2, 7, 17)]
    colnames(subOutcomeStats) <- c("HospitalName", "State", "OutcomeMortalityRate")
  }
  else if (outcome == "pneumonia") {
    subOutcomeStats <- outcomeStats[ , c(2, 7, 23)]
    colnames(subOutcomeStats) <- c("HospitalName", "State", "OutcomeMortalityRate")
  }
  subOutcomeStats$OutcomeMortalityRate <- as.numeric(subOutcomeStats$OutcomeMortalityRate)

  
  #create list of valid states from data frame, subOutcomeStats
  validStateList <- unique(subOutcomeStats$State)
  numValidStates <- length(validStateList)
  
  # Initialize 
  
  stVector <- vector()
  hspVector <- vector()
  rateVector <- vector()

  ## Return the hospital name with the lowest mortality rate for the requested outcome
  s <- split(subOutcomeStats, subOutcomeStats$State)
  if (num == "best") {
    for (i in 1:numValidStates){
      byst <- s[[i]]
      ordered <- byst[order(byst$OutcomeMortalityRate, na.last = TRUE), ]
      stVector[i] <- ordered[1, "State"]
      hspVector[i] <- ordered[1, "HospitalName"]
      rateVector[i] <- ordered[1, "OutcomeMortalityRate"]
    }
    ranked <- cbind.data.frame(stVector, hspVector, rateVector)
    print (c("Hospitals and Rates by State Ranked Best for OUtcome", outcome))
    names(ranked) <- c("State", "HospitalName", "Best Outcome Mortality Rate")
    return(ranked)
  }
  else if (num == "worst") {
    for (i in 1:numValidStates){
      byst <- s[[i]]
      ordered <- byst[order(byst$OutcomeMortalityRate, decreasing = TRUE, na.last = TRUE), ]
      stVector[i] <- ordered[1, "State"]
      hspVector[i] <- ordered[1, "HospitalName"]
      rateVector[i] <- ordered[1, "OutcomeMortalityRate"]
    }
    ranked <- cbind.data.frame(stVector, hspVector, rateVector)
    print (c("Hospitals and Rates by State Ranked Worst for OUtcome", outcome))
    names(ranked) <- c("State", "HospitalName", "Worst Outcome Mortality Rate")
    return(ranked)
  }
  else {
    idx <- as.numeric(num)
    for (i in 1:numValidStates){
      byst <- s[[i]] 
      ordered <- byst[order(byst$OutcomeMortalityRate, na.last = TRUE), ]
      if (idx > length(byst$HospitalName)) {
        stVector[i] <- ordered[1, "State"]
        hspVector[i] <- "NA"
        rateVector[i] <- "NA"
      }
      else {
        stVector[i] <- ordered[idx, "State"]
        hspVector[i] <- ordered[idx, "HospitalName"]
        rateVector[i] <- ordered[idx, "OutcomeMortalityRate"]
      }
      
    }
    ranked <- cbind.data.frame(stVector, hspVector, rateVector)
    print (c("Hospitals and Rates by State Ranked", num, "for Outcome", outcome))
    names(ranked) <- c("State", "HospitalName", "Nth Outcome Mortality Rate")
    return(ranked)
  }

}
