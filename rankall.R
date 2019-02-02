rankall <- function(outcome, num = "best") {
  ## Read outcome data
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  valid_outcomes<-c("heart attack", "heart failure","pneumonia")
  
  estados<-unique(dat$State)
  TStates<-length(unique(dat$State))
  totalstates<-c()
  totalHospital<-c()
  
  ## For each state, find the hospital of the given rank
  for (i in 1:TStates) {
    state_sub<-subset(dat,dat$State==estados[i])
    totalHospital<-c(totalHospital,rankhospital(estados[i],outcome,num))
    totalstates<-c(totalstates,estados[i])
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name  
  respuesta<-data.frame(Hospital=totalHospital,State=totalstates)
  return(respuesta)
  
}