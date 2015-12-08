best <- function(state, outcome, list = FALSE) {
  #Add a "list" feature
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')
  colnames(data)[c(2, 11, 17, 23)] <- c("hospital name", "heart attack", "heart failure", "pneumonia")
  
  statearray <- data[which(data[, "State"] == state), ]
  if(dim(statearray)[1] == 0) {
    stop("Not a valid state.")
  }
  else if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("Not a valid outcome. Possible outcomes are 'heart attack', 'heart failure', 'pneumonia.'")
  }
  else if(!(outcome %in% colnames(statearray)[c(11, 17, 23)])) {
    stop("No information on that outcome in ", state, ".")
  }
  else {
    statearray[, outcome] <- as.numeric(statearray[, outcome])
  m <- min(na.omit(statearray[, outcome]))
  indices <- which(statearray[, outcome] == m)
  bestarray <- statearray[indices, ]
  finallist <- sort(bestarray["hospital name"])
  return(finallist[1, "hospital name"])
  }
}

rankhospital <- function(state, outcome, num = 1) {
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')
  colnames(data)[c(2, 11, 17, 23)] <- c("hospital name", "heart attack", "heart failure", "pneumonia")
  
  statearray <- data[which(data[, "State"] == state), ]
  
  if(dim(statearray)[1] == 0) {
    stop("Not a valid state.")
  }
  
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("Not a valid outcome. Possible outcomes are 'heart attack', 'heart failure', 'pneumonia.'")
  }
  
  A <- na.omit(data.frame(statearray[, "hospital name"], as.numeric(statearray[, outcome])))
  
  if(dim(A)[1] == 0) {
    stop("No information on ", outcome," in ", state, ".")
  }

  x <- order(as.character(A[, 1]))
  preliminarray <- A #data.frame(matrix(data = 0, nrow = dim(A)[1], ncol = dim(A)[2]))
  sortedarray <- A #data.frame(matrix(data = NA, nrow = dim(A)[1], ncol = dim(A)[2]))
  
  for(i in 1:dim(A)[1]) {
    preliminarray[i, ] <- A[x[i], ]
  }
  
  y <- order(as.numeric(as.character(preliminarray[, 2])))
  
  for(i in 1:dim(A)[1]) {
    sortedarray[i, ] <- preliminarray[y[i], ]
  }
  if(num == "worst") {
    num <- dim(A)[1]
  }
  if(num > dim(sortedarray)[1]) {
    return("Enter a smaller number.")
  }
  return(as.character(sortedarray[num, 1]))
}

rankall <- function(outcome, num = 1) {
  states <- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA", "ID","IL","IN","KS","KY","LA","MA", "MD","ME","MI","MN","MO","MS","MT","NC", "ND", "NE","NH","NJ","NM", "NV", "NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VI", "VT","WA","WI","WV","WY")
 
  x <- sapply(states, rankhospital, outcome, num)
  x <- as.character(x)
  B <- data.frame(states, x)
  colnames(B) <- c("State/Territory", "Hospital Name")
  
 return(B)
}
