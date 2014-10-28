rankall <- function(outcome, num = "best") 
{
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")      
    
    ## Check that outcome is valid    
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
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    s <- outcome_data[outcome_data[,col] != "Not Available", ]
    
    s <- split(s, s$State)     
    
    #order each state split d.f. by outcome, then hospital name
    s_splitAndOrdered <- lapply(s, function(x) x[order(as.numeric(x[,col]),x[,2]),])    
    
    #loop over each ordered split d.f. and get the hospital name at row 'n'      
    if(identical(num,"best"))
        hospital <- lapply(s_splitAndOrdered, function(x) head(x[,2],1)) 
    else if(identical(num,"worst"))
        hospital <- lapply(s_splitAndOrdered, function(x) tail(x[,2],1)) 
    else
        hospital <- lapply(s_splitAndOrdered, function(x) x[num,2])     
    
    #get the state name
    state <- lapply(s_splitAndOrdered, function(x) x[1,7])
    
    #bind the vectors as columns of a data frame, return
    data.frame(cbind(hospital, state))
    
}
