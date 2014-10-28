##  function to return the hospital at rank "num" for a given outcome (30 day death 
##    rate) and given state

rankhospital <- function(state, outcome, num = "best")
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
    
    ## Return hospital name in the state with the given rank
    ##  30-day death rate
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
    
    s_ordered <- s[order(as.numeric(s[,col]), s[,2]), ]
    
    #print(subset(s_ordered,select=c(2, col)))    
    #print(length(s_ordered))
    
    if(identical(num,"best"))
        sRet <- head(s_ordered[,2],1) #use head (n=1) to get the first row, subset to hospital name column
    else if(identical(num,"worst"))
        sRet <- tail(s_ordered[,2],1) #use tail (n=1) to get the last row, subset to hospital name column
    else
        sRet <-s_ordered[num,2]
    
    sRet
}
