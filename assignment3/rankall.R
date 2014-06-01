rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv ("outcome-of-care-measures.csv", colClasses ="character")
        diseases <- c("heart attack", "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        if  (! outcome %in% diseases){
                stop("invalid outcome")
        }
        ## For each state, find the hospital of the given rank
        else {
                column <- 23 # pneumonia
                if(outcome == "heart attack")
                        column <-11 
                else if(outcome == "heart failure")
                        column <- 17
                
                ## Return hospital name in that state with the given rank 30-day death
                state <- levels(factor(data$State))
                hospital <- as.character(c(seq(length(state)))) 
                
                for (i in 1:length(state)) {
                        CurrentNum <- num
                        GoalState <- data[data$State == state[i],]
                        GoalState <- GoalState[GoalState[,column] != "Not Available",]
                        StateOutcome <- as.numeric(GoalState[, column])
                        AllHospitals <- GoalState[,2]
                        rank <- order(StateOutcome, AllHospitals)
                        
                        if (CurrentNum == "best"){
                                CurrentNum <-1
                        }
                        else if (CurrentNum == "worst"){
                                CurrentNum <- length(StateOutcome)
                        }
                        if (!is.numeric(CurrentNum) ){
                                stop("invalid num")
                        }
                        else if (CurrentNum < 1) {
                                hospital[i] <- NA
                        }
                        else if (CurrentNum > length(StateOutcome)) {
                                hospital[i] <- NA
                        }
                        else {      
                                hospital[i] <- GoalState[rank[CurrentNum],2]
                        }
                }
        }        
        ## Return a data frame with the hospital names and the (abbreviated) state names
        df <- data.frame(cbind(hospital,state),row.names = state,stringsAsFactors=FALSE)
        df
}