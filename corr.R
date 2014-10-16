corr <- function(directory, threshold = 0) 
{
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  wd <- getwd() #store the current working directory
  setwd(directory)
  
  vCorrs <- c()  
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  for (i in 1:332)
  {
    df <- read.csv(paste(sprintf("%03d", i), ".csv", sep =""))
    
    nobs <- sum(complete.cases(df))
    
    if (nobs > threshold)
    {
      vCorrs <- c(vCorrs, cor(df$nitrate, df$sulfate, use = "complete"))
    }
  }  
  
  #reset the working dir
  setwd(wd)
  
  if (length(vCorrs)==0)
  {
    vCorrs <- numeric(0)
  }
  
  ## Return a numeric vector of correlations
  vCorrs
}
