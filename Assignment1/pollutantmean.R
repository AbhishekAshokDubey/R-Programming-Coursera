pollutantmean <- function(directory,pollutant,id=1:332){
  
  sumCalulated <- rep(0,length(id))
  dataSizeCalulated <- rep(0,length(id))
  #meanCalulated <- <- rep(0,length(id))
  
  for (index in 1:length(id)){
    
    toAddZerosCount <- 3 - length(strsplit(as.character(id[index]),"")[[1]])
    charId <- paste(paste(rep("0",toAddZerosCount),collapse=""),id[index],sep="")
    my_data <- read.csv(paste(directory,"/",charId,".csv",sep=""))    
    
    dataSizeCalulated[index] <- sum(!is.na(my_data[pollutant]))
    sumCalulated[index] <- colSums(my_data[pollutant],na.rm=TRUE)
    
  }
  mean = sum(sumCalulated)/sum(dataSizeCalulated)
}
