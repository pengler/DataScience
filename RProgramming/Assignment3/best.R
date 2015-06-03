best <-function(state,outcome) {
  
  ## Read outcome data
  outcomedata <-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid                           
  
  ## Hostpital name = 
  ## [2] "Hospital.Name"
  ## Heart Attack =
  ## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  ## Heart Failure = 
  ## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  ## Pneumonia
  ## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    
  if ( outcome == "heart attack" ) { outcomecol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"}
  else if ( outcome == "heart failure" ) { outcomecol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"}
  else if ( outcome == "pneumonia" ) { outcomecol = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"}
  else {stop ("invalid outcome") }

  my_states<-unique(outcomedata$State)
  if (!is.element(state, my_states)) {stop ("invalid state")}
  
  bystate <- subset(outcomedata, State==state)
  bystate[[outcomecol]] <- suppressWarnings(as.numeric (bystate[[outcomecol]]))
  hospital <- bystate[which(bystate[[outcomecol]] == min(bystate[[outcomecol]], na.rm = TRUE)),] 
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  hospital[,2]

}
