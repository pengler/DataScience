pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  for ( i in id) {
    ## Build out our file path for reading 
    myfile <- paste (directory,"/",zeropad(i),".csv", sep="")
    ## print (myfile)
    ## Check if I've already read a file in or not
    ## if the pollutant data frame exists we append
    ## otherwise create a new data frame
    if (exists("mydata") ) {
      if (file.exists(myfile)) {temp <-read.csv(myfile)}
      mydata <- rbind(mydata,temp)
    }
    else { 
      if (file.exists(myfile)){mydata <- read.csv(myfile)}
    }
  }
  mean(mydata[,pollutant], na.rm=TRUE)
}

zeropad <- function(x, num=3) {s<-paste ("%0",num,"d",sep="");sprintf(s,x)}