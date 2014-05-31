best <- function(state, outcome) {
        
        ## Read outcome data
        data <- read.csv ("outcome-of-care-measures.csv", colClasses ="character")
        diseases <- c("heart attack", "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        if (! state %in% data$State){
                stop("invalid state")
        }
        else if (! outcome %in% diseases){
                stop("invalid outcome")
        }
        ## Return hospital name in that state with lowest 30-day death rate
        else {
                column <- 23 # pneumonia
                if(outcome == "heart attack"){
                        column <-11 
                }
                else if(outcome == "heart failure"){
                        column <- 17
                }
                GoalState <- data[data$State == state, ]
                StateOutcome <- as.numeric(GoalState[, column])
                min <- min(StateOutcome, na.rm = T)
                indices <- which(StateOutcome == min)
                BestHospitals <- GoalState[indices, 2]
                rank <- order(BestHospitals)
                HospName <- BestHospitals[rank[1]]
                HospName
        }
}
