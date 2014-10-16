pollutantmean <- function(directory, pollutant, id = 1:332) 
{
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    wd <- getwd() #store the current working directory
    setwd(directory)
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".    
    
    vPollutantVals <- c(0)
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    for (i in seq_along(id))
    {    
        df <- read.csv(paste(sprintf("%03d", id[i]), ".csv", sep =""))
        vPollutantVals <- 
            c(vPollutantVals,df[[pollutant]][!is.na(df[[pollutant]])])     
    } 
    
    #reset the working dir
    setwd(wd)
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    mean(vPollutantVals)
}


