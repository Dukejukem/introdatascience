zero_adder <- function(x, k = 3) {
  n <- k - 1 - floor(log10(x))
  new_number <- paste(paste(rep(0, n), collapse = ''), x, sep = '')
  
  return(new_number)
}

sum <- 0
size <- 0
initial_dir <- "C:/Kapil/School/12th Grade/Computer Science/IntroDS_Pierrepont/Rprog/ProgAssessment1"
  
pollutantmean <- function(directory, pollutant, id) {
  setwd(paste(initial_dir, directory, sep = '/'))
  for(i in id) {
    table <- read.table(paste(zero_adder(i), ".csv", sep = ''), header = TRUE, sep = ",")
    column <- table[pollutant]
    
    sum <- sum + sum(column[!is.na(column)])
    size <- size + length(t(column[!is.na(column)]))
  }
  setwd(initial_dir)
  mean <- sum / size
  mean
}

complete <- function(id) {
  
  complete_matrix <- matrix(data = NA, nrow = length(id), ncol = 2)
  entry <- 1
  
  for(i in id) {
    n <- 0
    table <- read.table(paste(zero_adder(i), ".csv", sep = ''), header = TRUE, sep = ",")
    
    for(j in 1:dim(table)[1]) {
      if(!is.na(table[j, "sulfate"]) & !is.na(table[j, "nitrate"])) {
        n <- n + 1
      }
      else {
        next
      }
      complete_matrix[entry, 1] <- i
      complete_matrix[entry, 2] <- n
    }
    entry <- entry + 1
  }
  return(complete_matrix)
}