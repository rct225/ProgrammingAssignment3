rankhospital <- function(state, outcome, num = "best") {
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

	     ## Return hospital name in that state with the given rank
	     ## 30-day death rate

	     stateHospitalData <- data[data$State == state, ]
     	     hospitalOutcomeData <- stateHospitalData[c(2,outcomeColumn)]
     	     colnames(hospitalOutcomeData) <- c("Name", "Outcome")
     	     clean <- hospitalOutcomeData[hospitalOutcomeData$Outcome != "Not Available",]
     	     clean$Outcome <- as.numeric(as.character(clean$Outcome))

	     if (num > nrow(stateHospitalData) && is.numeric(num)) {
	     	retval <- NA
	     } else if (num == "best") {
	     	results <- clean[order(clean$Outcome,clean$Name),]
	     	retval <- results[1,1]
	     } else if (num == "worst") {
	     	results <- clean[order(-clean$Outcome,clean$Name),]
             	retval <- results[1,1]
	     } else {
	        results <- clean[order(clean$Outcome,clean$Name),]
		retval <- results[num,1]
	     }
	     
	     retval
}