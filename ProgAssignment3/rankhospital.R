rankhospital <- function(state, outcome, num = "best") {
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  outcomeState <- outcomeData[outcomeData$State == state,] 
  colNames <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  rankNames <- c("best", "worst")
  if (!is.element(num, rankNames) && !is.numeric(num)){
    stop("invalid num")
  }
  rankNum <- NULL
  if (is.numeric(num)){
    rankNum <- as.numeric(num)
  }
  
  if (outcome == "heart attack") {
    outcomeState <- outcomeState[order(outcomeState[, colNames[1]], outcomeState[, "Hospital.Name"]), ]
    outcomeState <- outcomeState[!is.na(outcomeState[, colNames[1]]), ]
    
  }
  else if (outcome == "heart failure") {
    outcomeState <- outcomeState[order(outcomeState[, colNames[2]], outcomeState[, "Hospital.Name"]), ]
    outcomeState <- outcomeState[!is.na(outcomeState[, colNames[2]]), ]
  }
  else {
    outcomeState <- outcomeState[order(outcomeState[, colNames[3]], outcomeState[, "Hospital.Name"]), ]
    outcomeState <- outcomeState[!is.na(outcomeState[, colNames[3]]), ]
  }
  
  if (num == "best") {
    nRow <- 1L
  } else if (num == "worst") {
    nRow <- nrow(outcomeState)
  } else {
    nRow <- rankNum
  }
  
  outcomeState[nRow, "Hospital.Name"]
  
  
}