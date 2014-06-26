best <- function(state, outcome) {
  
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  if(state %in% data$State){
    if(outcome %in% c("heart attack", "heart failure", "pneumonia")){
      
      if(outcome=="heart attack"){colnumber=11}
      else if(outcome=="heart failure"){colnumber=17}
      else{colnumber=23}
      
      stateData <- data[which(state == data$State),]
      stateData[,colnumber] <- as.numeric(stateData[,colnumber])
      resultIndex<-which(stateData[,colnumber] == min(stateData[,colnumber],na.rm=TRUE))
      result <- sort(stateData[resultIndex,2])[1]
    }
    else{
      stop("invalid outcome")
    }
  }
  else{
    stop("invalid state")
  }
}

# regex <- paste("[[:print:]]*(?!number)[[:print:]]*mortality[[:print:]]*", outcomeSplit[[1]][1],"[[:print:]]*",sep="")
# colnumber <- grep(regex,names(data))
