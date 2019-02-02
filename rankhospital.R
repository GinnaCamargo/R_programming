source(best.R)

worst<-function(ListStates,outcome){
  if(outcome=="heart attack"){
    ListStates<-ListStates[order(ListStates[,11],ListStates[,2],na.last = NA),]
    hospital<-ListStates[nrow(ListStates),2]
  }
  else if(outcome=="heart failure"){
    ListStates<-ListStates[order(ListStates[,17],ListStates[,2],na.last = NA),]
    hospital<-ListStates[nrow(ListStates),2]
  }
  else {
    ListStates<-ListStates[order(ListStates[,23],ListStates[,2],na.last = NA),]
    hospital<-ListStates[nrow(ListStates),2]
  }
}
  



numfunction <- function(ListStates, outcome,num){
  if(outcome=="heart attack"){
    ListStates<-ListStates[order(ListStates[,11],ListStates[,2],na.last = NA),]
    hospital<-ListStates[num,2]
  }
  else if(outcome=="heart failure"){
    ListStates<-ListStates[order(ListStates[,17],ListStates[,2],na.last = NA),]
    hospital<-ListStates[num,2]
  }
  else {
    ListStates<-ListStates[order(ListStates[,23],ListStates[,2],na.last = NA),]
    hospital<-ListStates[num,2]
  }
}

rankhospital <- function(state, outcome, num = "best"){
  ## Read outcome data
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
 
  ## Check that state and outcome are valid
  valid_dat<-c("heart attack", "heart failure","pneumonia")
  
  # change data type from character to numeric
  dat[, 11] <- as.numeric(dat[, 11]) # heart attack
  dat[, 17] <- as.numeric(dat[, 17]) # heart failure
  dat[, 23] <- as.numeric(dat[, 23]) # pneumonia
  
  if(!state %in% dat$State){
    stop("invalid state")
  }
  else if(!outcome %in% valid_dat){
    stop("invalid state")
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  else {
    ListStates<-subset(dat,dat$State==state)
    if(num=="best"){
      hospital<-best(ListStates,outcome)
    }
    else if(num=="worst"){
      hospital<-worst(ListStates,outcome)
    }
    else if(num>nrow(ListStates)){
      hospital<-NA
    }
    else {
      hospital<-numfunction(ListStates,outcome,num)
    }
  }
  return(hospital)
}


