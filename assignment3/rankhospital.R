rankhospital <- function(state, outcome, num = "best") {
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
        ## Return hospital name in that state with lowest 30-day death
        else {
                column <- 23 # pneumonia
                if(outcome == "heart attack")
                        column <-11 
                else if(outcome == "heart failure")
                        column <- 17
                
                ## Return hospital name in that state with the given rank
                ## 30-day death 
                GoalState <- data[data$State == state,]
                GoalState <- GoalState[GoalState[,column] != "Not Available",]
                StateOutcome <- as.numeric(GoalState[, column])
                AllHospitals <- GoalState[,2]
                rank <- order(StateOutcome, AllHospitals)
                
                if (num == "best"){
                        num <-1
                }
                else if (num == "worst"){
                        num <- length(StateOutcome)
                }
                if (!is.numeric(num) ){
                        stop("invalid num")
                }
                else if (num < 1) {
                        NA
                }
                else if (num > length(StateOutcome)) {
                        NA
                }
                else {      
                        HospName <- GoalState[rank[num],2]
                        HospName
                }
        }        
}

