complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  ## Create our empty data frame
  ## df <- data.frame(id=as.integer(),nobs=as.integer())
 
  for ( i in id) {
    ## Build out our file path for reading 
    myfile <- paste (directory,"/",zeropad(i),".csv", sep="")
    if (file.exists(myfile)) {temp <-read.csv(myfile)}
    mycases <- nrow(temp[complete.cases(temp),])
    df <- rbind (df, c(i,mycases))
  }
  colnames(df) <- c("id", "nobs")
  df
}

## Helper fucntion to zero pad numbers
zeropad <- function(x, num=3) {s<-paste ("%0",num,"d",sep="");sprintf(s,x)}