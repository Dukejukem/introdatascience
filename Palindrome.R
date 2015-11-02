countdigits <- function(x) {
  n <- 0
  while(x >= 10^n) {
    n <- n + 1
  }
  return(n)
}

vectorconverter <- function(x) {
  n <- countdigits(x)
  vector <- 1:n
  
  for(i in 1:n) {
    entry <- floor(x / 10^(i - 1)) %% 10
    vector[i] <- entry
  }
  return(vector)
}

flip <- function(x) {
  n <- length(x)
  vector <- 1:n

  for(i in 1:n) {
    vector[i] <- x[(n - i + 1)]
  }
  return(vector)
}

palindrome <- function(x) {
  
  vector_1 <- vectorconverter(x)
  
  vector_2 <- flip(vector_1)
  
  if(identical(vector_1, vector_2)) {
    "Yes, it's a palindrome!"
  } else {
    "No, its not a palindrome."
  }
}



