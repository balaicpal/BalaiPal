corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
 
  
  ## change the id list three digited by adding leading zeroes
  ## as file names are all three digited

  id <- 1:332
  id1 <- sprintf("%03d",id) 
  
  ## form the file_names from id
  fl_names <- paste(id1,".csv",sep="")
  
  ## initilize the vector for num_cases
  cor_vec <- vector(mode="integer", length=0) 
  
  for(f_seq in seq(fl_names)){
    ifl_dat <- read.csv(fl_names[f_seq])
    
    ## fetch subset data ignoring NA values
    ifl_dat1 <- ifl_dat[complete.cases(ifl_dat),]
    
    if (nrow(ifl_dat1) > threshold) {
      
      ## find corr co-efficient
      correln <- cor(ifl_dat1[["sulfate"]],ifl_dat1[["nitrate"]])
      cor_vec<- append(cor_vec,correln)
    }
  }
  ## Return a numeric vector of correlations
  cor_vec
}
