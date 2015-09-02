rankhospital <- function(state, outcome, num = "best") {
  # Three arguments: 
  # state: the 2-character abbreviated name of a state
  # outcome name: returns a character vector with the name of the hospital: Hospital.Name
  # that has the best (i.e. lowest) 30-day mortality
  # num takes values best, worst,  
  
  ## First Check that state is valid
  # make sure state entered is valid
  if(!(state %in% ocmdat$State)) { 
    stop("invalid state")
  }
  
  ## Check that outcome is valid
  outcome_list<- c("heart attack","heart failure","pneumonia")
  if (!(outcome %in% outcome_list)) {
    stop("invalid outcome")
  }
  
  ## Chek that num argument is valid
  if(class(num) == "character"){
    if (! (num == "best" || num == "worst")){
      stop("invalid number")
    }
  }
  
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")[,c(2,7,11,17,23)]
  options(warn=-1) # set warnings off
  
  ## get subset data for the given state
  data <- data[data$State==state,]
  data <- data[,c(1,3,4,5)]
  
  ## reduce data for the given outcome for programming purpose  
  if(outcome == "heart attack") {
    data <- data[,c(1,2)]
  }
  else if(outcome == "heart failure") {
    data <- data[,c(1,3)]
  }
  else if(outcome == "pneumonia") {
    data <- data[,c(1,4)]
  }
  
  ## Rename three outcome columns to a single name for ease of understaning 
  names(data)[2] <- "Deaths"
  data[, 2] <- suppressWarnings( as.numeric(data[, 2]))
  
  ## Remove rows with NA
  data <- data[!is.na(data$Deaths),]

  ## Order by Deaths and then HospitalName
  data <- data[order(data$Deaths, data$Hospital.Name),]
  
  ## Return Hopital Name
  if (class(num) == "character") {
    if (num == "best") {
      ret <- data$Hospital.Name[1]
    }
    else if (num == "worst") {
      ret <- data$Hospital.Name[nrow(data)]
    }
  }  
  
  if (class(num) == "numeric") {
    if (num > nrow(data)) {
      ret <- "NA"
    } 
    else {
      ret <- data$Hospital.Name[num]
    }
  }
  
  ret # returns host name
}