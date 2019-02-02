
best <- function(state, outcome) {
  ## Read outcome data
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  valid_outcomes<-c("heart attack", "heart failure","pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in%   dat$State){
    stop("invalid state")
  }
  else if(!outcome %in% valid_outcomes )  {
    stop("invalid state")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
    else{
    if(outcome=="heart attack"){
      ListStates<-subset(dat,dat$State==state)
      min_inx<-which.min(ListStates[,11])
      hospital<-ListStates[min_inx,2]
    }
    else if(outcome=="heart failure"){
      ListStates<-subset(dat,dat$State==state)
      min_inx<-which.min(ListStates[,17])
      hospital<-ListStates[min_inx,2]
    }
    else if(outcome=="pneumonia"){
      ListStates<-subset(dat,dat$State==state)
      min_inx<-which.min(ListStates[,23])
      hospital<-ListStates[min_inx,2]
    }
    hospital
  }
}


  





