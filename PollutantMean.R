zero_adder <- function(x, k = 2) {
  n <- k - floor(log10(x))
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
  
}