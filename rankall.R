rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv",
                     colClasses = "character")

    ## Check that state and outcome are valid
    states <- sort(unique(data[,7]))

    validOutcomes <- c("heart attack" = 11,
                       "heart failure" = 17,
                       "pneumonia" = 23)

    if (is.na(match(outcome, names(validOutcomes)))) {
        stop("invalid outcome")
    }

    outcomeColumn <- validOutcomes[[outcome]]

    hospitalOutcomeData <- data[c(2,7,outcomeColumn)]
    colnames(hospitalOutcomeData) <- c("Name", "State", "Outcome")
    clean <- hospitalOutcomeData[hospitalOutcomeData$Outcome != "Not Available",]
    clean$Outcome <- as.numeric(as.character(clean$Outcome))

    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name

    hospitals.hospital <- character()
    hospitals.state <- character()

    i <- 1
    for (state in states) {
        stateHospData <- clean[clean$State == state, ]
        if (num > nrow(stateHospData) && is.numeric(num)) {
            hospitals.state[i] <- state
            hospitals.hospital[i] <- NA
        } else if (num == "best") {
            results <- stateHospData[order(stateHospData$Outcome,
                                           stateHospData$Name),]
            hospitals.state[i] <- state
            hospitals.hospital[i] <- results[1,1]
        } else if (num == "worst") {
            results <- stateHospData[order(-stateHospData$Outcome,
                                           stateHospData$Name),]
            hospitals.state[i] <- state
            hospitals.hospital[i] <- results[1,1]
        } else {
            results <- stateHospData[order(stateHospData$Outcome,
                                           stateHospData$Name),]
            hospitals.state[i] <- state
            hospitals.hospital[i] <- results[num,1]
        }
        i <- i + 1

    }

    hospitals <- data.frame(hospitals.hospital, hospitals.state)
    names(hospitals) <- c("hospital", "state")
    hospitals
}
