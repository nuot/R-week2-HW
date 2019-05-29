# Part 1
# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a 
# specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant',
#  and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data 
#  from the directory specified in the 'directory' argument and returns the mean of the pollutant across all 
#  of the monitors, ignoring any missing values coded as NA.



##create a function, first argument is the directory/path, second argument 
##is what function you want to pass in(what kind of pollutant in this homework)
## id is the files name
pollutantmean <- function(directory, pollutant, id = 1:332){
  #create a filelist
  fileList <- list.files(path = directory, pattern = ".csv",full.names = TRUE)
  ##create an empty vector
  pollutants <- numeric()
  ##for each csv file, paste the data to the empty pollutants vector
  for(i in id){
    data <- read.csv(fileList[i])
    ##go through multiple files, calculate mean and save the mean
    pollutants <- c(pollutants, data[[pollutant]])
  }
  mean(pollutants, na.rm = TRUE)
}

## To test if function works or not
pollutantmean("/Users/tiannuo/Desktop/R_Coursera/specdata/","sulfate")



# Part 2
# Write a function that reads a directory full of files and reports the number of completely observed cases in 
# each data file. The function should return a data frame where the first column is the name of the file and the 
# second column is the number of complete cases. 


completefunc <- function(directory, id = 1:332){
  #create a list of files
  filelist <- list.files(path="specdata", pattern=".csv", full.names = TRUE)
  #create an empty vector
  nobs <- numeric()
  #looping in files
  for (i in id) {
    data <- read.csv(filelist[i])
    nobs <- c(nobs , sum(complete.cases(data)))
  }
  
  data.frame(id,nobs)
}
#testing 
completefunc("specdata/", 1)



# Part 3
# Write a function that takes a directory of data files and a threshold for complete cases and calculates the 
# correlation between sulfate and nitrate for monitor locations where the number of completely observed cases 
# (on all variables) is greater than the threshold. The function should return a vector of correlations for 
# the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the 
# function should return a numeric vector of length 0. 

corr <- function(directory, threshold = 0){
  #create a list reading all files
  filelist <- list.files(path = "specdata", pattern = ".csv", full.names = TRUE)
  correlations <- numeric()
  for (i in 1:332) {
    data <- read.csv(filelist[i])
    completecases <- data[complete.cases(data),]
    completed <- sum(complete.cases(data))
    if (completed > threshold){
      correlations = c(correlations, cor(completecases["nitrate"],completecases["sulfate"]))
    }
  }
  correlations
}
#testing
corr("specdata",150)

