rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomedata <-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  # Simplify our ourcome data to allow for easier manipulation
  soutcome <- outcomedata[(c(2,7,11,17,23))]
  my_states<-unique(soutcome$State)
  my_states <- sort(my_states)
  ranking <- data.frame(hospital=character(),
                       state=character(), 
                       stringsAsFactors=FALSE) 
  ##                     ha=numeric(),
  ##                     hf=numeric(),
  ##                     p=numeric(),

  rm(outcomedata)
  
  ## Convert the chars to numerics for each column
    soutcome[3:5] <- suppressWarnings(sapply(soutcome[3:5],as.numeric))   
  
  ## Check that state and outcome are valid
  
  ## The simplified soutcome data frame should look like
  ## Hostpital name = 
  ## [1] (chr) "Hospital.Name"
  ## State = 
  ## [2] (chr)"State"
  ## Heart Attack =
  ## [3] (num)"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  ## Heart Failure = 
  ## [4] (num)"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  ## Pneumonia
  ## [5] (num) "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
  
  if ( outcome == "heart attack" ) { outcomecol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"}
  else if ( outcome == "heart failure" ) { outcomecol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"}
  else if ( outcome == "pneumonia" ) { outcomecol = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"}
  else {stop ("invalid outcome") }
  
  if (num =="best") {num = 1}
  
  for ( i in my_states) {
    tempranked <- subset(soutcome, State == i)
    tempranked <- tempranked[ order(tempranked[,outcomecol],tempranked[,1]), ]
    ##tempranked <- complete.cases(tempranked)
    
    if (num =="best") {
      ranking <-rbind(ranking,cbind(tempranked[1,1],i)) }
    else if (num == "worst") {
      ranking <-rbind(ranking,cbind(tempranked[(which.max(tempranked[[outcomecol]])),1],i)) }
    else { 
      ranking <-rbind(ranking,cbind(tempranked[num,1],i)) }
  }

  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  colnames(ranking) <- c("hospital", "state")
  ranking
}