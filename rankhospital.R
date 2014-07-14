find.hospital<-function(datas,state,outcome) {
  if (outcome == "heart attack"){
    datas.sub<-split(datas[,c(2,11)], datas[,7])
  }
  else if (outcome == "heart failure"){
    datas.sub<-split(datas[,c(2,17)], datas[,7])
  }
  else if (outcome == "pneumonia"){
    datas.sub<-split(datas[,c(2,23)], datas[,7])
  }
  states.by.sub<-datas.sub[[state]]
  states.by.sub<-states.by.sub[order(states.by.sub[2], states.by.sub[1]),]
  y<-complete.cases(states.by.sub)
  return(states.by.sub[y,])
}

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  setwd("/home/bbqrpg/Coursera/Rprogramming/assign.week4/")
  datas<-read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  datas[,11]<-as.numeric(datas[,11])
  datas[,17]<-as.numeric(datas[,17])
  datas[,23]<-as.numeric(datas[,23])
  
  ## Check that state and outcome are valid
  valid_outcome<-c("heart attack","heart failure", "pneumonia")
  
  if (!state %in% datas$State){
    stop("invalid state")
  }
  else if (!outcome %in% valid_outcome){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  hosp.list<-find.hospital(datas,state,outcome)
  if (num == "best"){
    hosp.list[1,1]
  }
  else if (num == "worst"){
    hosp.list[nrow(hosp.list),1]
  }
  #else if (num > length(hosp.list) {print(NA)}
  
  else{
    num<-as.numeric(num)
    hosp.list[num,1]
  }
}