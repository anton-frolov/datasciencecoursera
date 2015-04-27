best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!is.element(state, outcomeData$State)){
    stop("invalid state")
  }

  outcomeSub <- c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome, outcomeSub)){
    stop("invalid outcome")
  }
  
  
  ## Return hospital name in that state with lowest 30-day death
  outcomeState <- outcomeData[outcomeData$State == state,] 
  colNames <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  ##cast values as numeric (or set NA)
  outcomeState[, colNames] <- sapply(outcomeState[, colNames], as.numeric)
  ## sort by hospital name
  outcomeState <- outcomeState[order(outcomeState[, "Hospital.Name"]), ]
  
  ## rate
  if (outcome == "heart attack") {
    best <- outcomeState[which.min(outcomeState[, colNames[1]]), "Hospital.Name"]
  }
  else if (outcome == "heart failure") {
    best <- outcomeState[which.min(outcomeState[, colNames[2]]), "Hospital.Name"]
  }
  else {
    best <- outcomeState[which.min(outcomeState[, colNames[3]]), "Hospital.Name"]
  }
  
  best

}
