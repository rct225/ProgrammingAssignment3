rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", 
                   colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- unique(data[,7])
  if(is.na(match(state, states))) {
    stop("invalid state")
  }
  
  validOutcomes <- c("heart attack" = 11, 
                     "heart failure" = 17, 
                     "pneumonia" = 23)
  
  if (is.na(match(outcome, names(validOutcomes)))) {
    stop("invalid outcome")
  }
  
  outcomeColumn <- validOutcomes[[outcome]]
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
}