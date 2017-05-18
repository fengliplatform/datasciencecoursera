pollutantmean <- function(directory, pollutant, id=1:332) {
  # set working dir
  dir <- ""
  dir <- paste(".", directory, sep="/")
  setwd(dir)
  
  ids <- c(id)
  id_length <- length(ids)
  means <- numeric(id_length)
  j <- 1
  for (i in ids) {
    if(i<10) {
      file_name <- paste("00",i,sep="")
    } else if(i >= 10 && i < 100) {
      file_name <- paste("0",i,sep="")
    } else {
      file_name <- i
    }
    file_name <- paste(file_name, "csv", sep = ".")
    #print(file_name)
    
    df <- read.csv(file_name,header=TRUE,stringsAsFactors = FALSE)
    my_mean <- mean(df[,pollutant], na.rm=TRUE)
    #print(my_mean)
    means[j] <- my_mean
    j <- j + 1
  }
  means
  final_mean <- mean(means, na.rm=TRUE)
  print(final_mean)
}