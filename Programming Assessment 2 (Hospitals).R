data <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')
colnames(data)[c(2, 11, 17, 23)] <- c("hospital name", "heart attack", "heart failure", "pneumonia")

best <- function(state, outcome, list = FALSE) {
  
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("Not a valid outcome. Possible outcomes are 'heart attack', 'heart failure', 'pneumonia.'")
  }
  
  statearray <- data[which(data[, "State"] == state), ]
  
  if(dim(statearray)[1] == 0) {
    stop("Not a valid state.")
  }
  
  bestarray <- data.frame(statearray[, "hospital name"], as.numeric(statearray[, outcome]))
  if(length(na.omit(bestarray[, 2])) == 0) {
    stop("No information on ", outcome, " in  ", state, ".")
  }
  m <- min(na.omit(statearray[, outcome]))
  bestarray <- statearray[which(statearray[, outcome] == m), ]
  finallist <- sort(bestarray[, "hospital name"])
  return(finallist[1, "hospital name"])
}

rankhospital <- function(state, outcome, num = 1) {
  
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("Not a valid outcome. Possible outcomes are 'heart attack', 'heart failure', 'pneumonia.'")
  }
  
  statearray <- data[which(data[, "State"] == state), ]
  
  if(dim(statearray)[1] == 0) {
    stop("Not a valid state.")
  }
  
  A <- suppressWarnings(na.omit(data.frame(statearray[, "hospital name"], as.numeric(statearray[, outcome]))))
  
  if(dim(A)[1] == 0) {
    stop("No information on ", outcome," in ", state, ".")
  }

  x <- order(as.character(A[, 1]))
  preliminarray <- A 
  sortedarray <- A
  
  for(i in 1:dim(A)[1]) {
    preliminarray[i, ] <- A[x[i], ]
  }
  
  y <- order(preliminarray[, 2])
  
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
 
  x <- suppressWarnings(sapply(states, rankhospital, outcome, num))
  x <- as.character(x)
  B <- data.frame(states, x)
  colnames(B) <- c("State/Territory", "Hospital Name")
  
 return(B)
}