rankall<-function(outcome,num="best"){
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  a <- as.factor(data$State)
  stateList <- levels(a)
  noOfStates = length(stateList)
  if(is.character(num))
  {
    if(num=="best"){num = 1}
    else if(num=="worst"){}
    else{
      stop("Invalid option for rank")
    }
    
  }
  result <- cbind(rep("",noOfStates) , stateList)
  colnames(result) <- c("hospital","state")
  if(outcome %in% c("heart attack", "heart failure", "pneumonia")){      
    if(outcome=="heart attack"){colnumber=11}
    else if(outcome=="heart failure"){colnumber=17}
    else{colnumber=23}      
  }
  else{
    stop("invalid outcome")
  }
  
  
  for(i in 1:noOfStates)
  {
    stateData <- data[which(stateList[i] == data$State),]
    stateData[,colnumber] <- as.numeric(stateData[,colnumber])
    
    sortOrder <- order(stateData[,colnumber],stateData[,2],na.last=TRUE)
    
    maxIndex = sum(!is.na(stateData[,colnumber]))
    numLoop = num
    if(is.character(num))
    {
      #It has to num = "worst"
      numLoop = maxIndex        
    }
    else if(numLoop>maxIndex){
      result[i,1] <- NA
    }
    result[i,1] <- stateData[sortOrder[numLoop],2]
    
  }
  as.data.frame(result)
}
