rankall <- function(outcome,  num = "best") {
  # Two arguments: 
  # outcome name: returns data frame with Hospital names & state names  
  
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
  
  
  ## reduce data for the given outcome for programming purpose  
  if(outcome == "heart attack") {
    data <- data[,c(1,2,3)]
  }
  else if(outcome == "heart failure") {
    data <- data[,c(1,2,4)]
  }
  else if(outcome == "pneumonia") {
    data <- data[,c(1,2,5)]
  }
  
  ## Rename three outcome columns to a single name for ease of understaning 
  names(data)[3] <- "Deaths"
  data[, 3] <- suppressWarnings( as.numeric(data[, 3]))
  
  ## Remove rows with NA
  data <- data[!is.na(data$Deaths),]
  
  states <- unique(data$State)
  
  #compute ranking for each state
  rankMatrix <- sapply(states, function(state){
    ## Filter DF on state
    data <- data[data$State==state,]
    ## Convert using as.numeric, supressing coercian warnings

    ## Order by Deaths and then HospitalName for each state
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
    return(c(state,ret))  
  })
  
  #display result
  result <- data.frame(rankMatrix[2,], rankMatrix[1,])
  colnames(result) <- c("hospital", "state")
  rownames(result) <- result$state
  result <- result[order(result$state),]
  return(result)
}

