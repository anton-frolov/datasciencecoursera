rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  outcomeSub <- c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome, outcomeSub)){
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  states <- sort(unique(outcomeData$State))
  
  ranks <- data.frame(hospital=NA, state=NA)
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  for (i in 1:length(states)) {
    ranks[i, ] <- c(rankhospital(states[i], outcome, num), states[i])
  }
  
  ranks
  
}
