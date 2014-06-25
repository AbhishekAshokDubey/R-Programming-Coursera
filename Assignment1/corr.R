corr <- function(directory,threshold = 0){
  
  fileList <- list.files("specdata")
  noOfFiles <- length(fileList)
  cr = c()

  for (index in 1:noOfFiles){
    if(complete(directory,index)[2]>threshold)
    {
      # One way can be (Commented below)
      #my_data <- read.csv(paste(directory,"/",fileList[index],sep=""))
      #sulphate <- my_data["sulfate"][!is.na(my_data["sulfate"]) & !is.na(my_data["nitrate"])]
      #nitrate <- my_data["nitrate"][!is.na(my_data["sulfate"]) & !is.na(my_data["nitrate"])]
      #cr <- c(cr,cor(sulphate,nitrate))
      
      # Oher way can be (below)
      my_data <- na.omit(read.csv(paste(directory,"/",fileList[index],sep="")))
      cr <- c(cr,cor(my_data["sulfate"],my_data["nitrate"]))
    }
  }
  
  cr
}
