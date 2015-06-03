corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ## Get all the files in the specified directory
  myfiles <- list.files(path = directory, pattern="*.csv") 
  corrs <-as.numeric(c())
  for ( i in myfiles) {
    ## Build out our file path for reading 
    myfile <- paste (directory,"/",i, sep="")
    if (file.exists(myfile)) {df <-read.csv(myfile)}
    # look for files above our threshold
    if (nrow(df[complete.cases(df),]) > threshold ) {
      colnames(df) <- c("Date","sulfate","nitrate","ID")
      # calcutate add append the correlations
      corrs <- c(corrs,cor (x=df$nitrate, y=df$sulfate,use="complete"))  
    }
  }
  
  ## Example of cor
  ## cor (x=samplecor$nitrate, y=samplecor$sulfate,use="complete")
  corrs
  
}
