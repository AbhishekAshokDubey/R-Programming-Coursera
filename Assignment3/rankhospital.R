rankhospital <- function(state, outcome,num) {
  if(is.character(num))
  {
    if(num=="best"){num = 1}
    else if(num=="worst"){}
    else{
      stop("Invalid option for rank")
    }
    
  }
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  if(state %in% data$State){
    if(outcome %in% c("heart attack", "heart failure", "pneumonia")){
      
      if(outcome=="heart attack"){colnumber=11}
      else if(outcome=="heart failure"){colnumber=17}
      else{colnumber=23}
      
      stateData <- data[which(state == data$State),]
      stateData[,colnumber] <- as.numeric(stateData[,colnumber])

      sortOrder <- order(stateData[,colnumber],stateData[,2],na.last=TRUE)
      
      maxIndex = sum(!is.na(stateData[,colnumber]))
      if(is.character(num))
      {
        #It has to num = "worst"
        num = maxIndex        
      }
      else if(num>maxIndex){
        return(NA)
      }
      result <- stateData[sortOrder[num],2]
    }
    else{
      stop("invalid outcome")
    }
  }
  else{
    stop("invalid state")
  }
}