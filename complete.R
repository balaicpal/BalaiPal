complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used

  ## change the id list three digited by adding leading zeroes
  ## as file names are all three digited
  id1 <- sprintf("%03d",id) 
  
  ## form the file_names from id
  fl_names <- paste(id1,".csv",sep="")
  
  ## initilize the vector for num_cases
  nobs <- vector(mode="integer", length=length(id1)) 

  for(f_seq in seq(fl_names)){
    ifl_dat <- read.csv(fl_names[f_seq])
    
    ## fetch subset data ignoring NA values
    ifl_dat1 <- ifl_dat[complete.cases(ifl_dat),]
    
    ## count num cases 
    nobs[f_seq] <- nrow(ifl_dat1) 
  }
  ## Return the data frame
  data.frame(id,nobs)
}
