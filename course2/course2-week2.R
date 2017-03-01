# course 2, week 2



########## part 1
# set working directory
current_wd <- "~/course/ds_coursera"
setwd(current_wd)

# function "pollutantmean"
pollutantmean <- function(directory, pollutant, id=1:332) {
  # set working dir
  pwd <- getwd()
  dir <- paste(".", directory, sep="/")
  setwd(dir)

  ids <- c(id)

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
    if ( j == 1 ) {
      df_full <- df
    } else {
      df_full <- rbind(df_full, df)
    }
    j <- j + 1
  }
  
  my_mean <- mean(df_full[,pollutant], na.rm=TRUE)

  setwd(pwd)
  print(my_mean)
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")



########## part 2
# set working directory
current_wd <- "~/course/ds_coursera"
setwd(current_wd)

# function "complete"
complete <- function(directory, id=1:332) {
  # set working dir
  pwd <- getwd()
  dir <- paste(".", directory, sep="/")
  setwd(dir)
  
  ids <- c(id)
  ids_length <- length(ids)
  
  id_v <- numeric(ids_length)
  nobs_v <- numeric(ids_length)
  
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
    #nobs <- nrow(df[complete.cases(df),])
    cc_v <- complete.cases(df)
    nobs <- length(cc_v[cc_v ==TRUE])
    #print(nobs)
    
    id_v[j] <- i
    nobs_v[j] <- nobs
    
    j <- j + 1
  }
  
  setwd(pwd)
  
  return_df <- data.frame(id=id_v, nobs=nobs_v)
  return_df
}

complete("specdata", 1)
complete("specdata", c(2,4,8,10,12))
complete("specdata", 30:25)
complete("specdata", 3)

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])



############ part 3
# set working directory
current_wd <- "~/course/ds_coursera"
setwd(current_wd)

# function "corr"
corr <- function(directory, threshold = 0) {
  # set working dir
  pwd <- getwd()
  dir <- paste(".", directory, sep="/")
  setwd(dir)
  
  corr_v <- numeric(0)
  
  j <- 1
  for (i in 1:332) {
    if(i<10) {
      file_name <- paste("00",i,sep="")
    } else if(i >= 10 && i < 100) {
      file_name <- paste("0",i,sep="")
    } else {
      file_name <- i
    }
    file_name <- paste(file_name, "csv", sep = ".")

    df <- read.csv(file_name,header=TRUE,stringsAsFactors = FALSE)
    df2 <- df[complete.cases(df),]

    cc <- nrow(df2)
    if (cc > threshold) {
      #print(cc)
      corr_i <- cor(df2$sulfate, df2$nitrate)
      #print(corr_i)
      #print(i)

      corr_v[j] <- corr_i
    }
    
    j <- j + 1
  }
  
  setwd(pwd)
  
  corr_v
}

cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))





cr <- corr("specdata", 150)
head(cr)
summary(cr)
#> head(cr)
#[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
#> summary(cr)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.21060 -0.04999  0.09463  0.12530  0.26840  0.76310 


cr <- corr("specdata", 400)
head(cr)
summary(cr)
#> head(cr)
#[1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
#> summary(cr)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.17620 -0.03109  0.10020  0.13970  0.26850  0.76310 

cr <- corr("specdata", 5000)
#> head(cr)
#numeric(0)
#> summary(cr)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 

cr <- corr("specdata")
#> head(cr)
#[1] -0.22255256 -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667
#> summary(cr)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-1.00000 -0.05282  0.10720  0.13680  0.27830  1.00000 
#> length(cr)
#[1] 323








