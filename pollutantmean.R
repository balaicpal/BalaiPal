pollutantmean <- function(directory="specdata", pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## my program assumes data files are loaded in specdata directory
  ## under current working directory from where the program is run 
  
  ## checking if the current working directory is specdata or not
  ##wdir <- getwd()
  ##dir_mode <- grep(directory,wdir)
  ##if (dir_mode == 0) {
  ##  setwd(paste(wdir,'/',directory,sep=""))
  ##}
  
  ## change the id list three digited by adding leading zeroes
  ## as file names are all three digited
  id1 <- sprintf("%03d",id) 
  
  ## form the file_names from id
  fl_names <- paste(id1,".csv",sep="")
  
  ## compute average for all the files by summing up in a loop
  sum_sn <- 0
  count_sn <- 0
  
  for(ifl in fl_names){
      ifl_dat <- read.csv(ifl)

      if (pollutant == "sulfate") {
          ## fetch subset data ignoring NA values
          ifl_dat1 <- ifl_dat$sulfate[!is.na(ifl_dat$sulfate)]
      } else {
          ## fetch subset data ignoring NA values
          ifl_dat1 <- ifl_dat$nitrate[!is.na(ifl_dat$nitrate)]
      }
      ## sum up ith file data and add to grand sum
      sum_sn <- sum_sn + sum(ifl_dat1)
      ## add up ith file count to grant count 
      count_sn <- count_sn + length(ifl_dat1) 
  }
  ## Return the mean of the pollutant across all monitors list
  round(sum_sn/count_sn,digits=3)
}