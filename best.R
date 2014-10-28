##  Function to return the best ranked hospital
##  Based on a given outcome, against the 30 day death rate

best <- function(state, outcome)
{
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")      
    
    ## Check that state and outcome are valid
    if(!(tolower(state) %in% tolower(outcome_data$State)))
    {
        stop("invalid state")        
    }
    
    if(!(tolower(outcome) %in% c("heart attack", "heart failure", "pneumonia")))
    {
        stop("invalid outcome")
    }
    
    
    ## Return hospital name in that state with th lowest 30-day death rate
    
    if(identical(tolower(outcome),"heart attack"))
        col <- 11
    else if(identical(tolower(outcome),"heart failure"))
        col <- 17
    else if(identical(tolower(outcome),"pneumonia"))
        col <- 23
    else
        col <- 0

    # get the rows for this state
    s <- outcome_data[outcome_data$State == state & outcome_data[,col] != "Not Available", ]
    
    #print(subset(s,select=c(2, col)))
    
    # get the hospital name (col 2) for min of the column inspected
    hospitalName <- s[,2][as.numeric(s[,col]) == min(as.numeric(s[,col]))]    

    hospitalName <- sort(hospitalName)[1]    
    
    # and return that
    hospitalName
}
