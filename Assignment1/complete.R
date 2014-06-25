complete <- function(directory,id=1:332){
  
  nobs <- rep(0,length(id))
  
  for (index in 1:length(id)){
    
    toAddZerosCount <- 3 - length(strsplit(as.character(id[index]),"")[[1]])
    charId <- paste(paste(rep("0",toAddZerosCount),collapse=""),id[index],sep="")
    my_data <- read.csv(paste(directory,"/",charId,".csv",sep=""))    
    head(my_data)
    nobs[index] <- sum(complete.cases(my_data))
  }
  dataFrame = data.frame(id,nobs)
  colnames(dataFrame) <-(c("id","nobs"))  
  dataFrame
}