genForcingData <- function(num, lower, upper) {
  #num is the amount of observations for each forcing data parameter
  # lower is the lower bound observations can have
  # upper is the upper bound observations can have
  
  #f <- "forcingDataTester.csv" # Forcing data for tests.
  f <- "forcingData.csv" # File to print forcing data observations to.
  
  params <- list("gpp", "lai", "hfls", "hfss", "albs") # The forcing data parameters, ts omitted
  forcingDataObs <- matrix(data = NA, nrow = num+1, ncol = length(params)) # Matrix of observations
  
  for (i in 1:length(params)) {
    x <- as.character(params[i]) # Create a variable with the parameter as its name
    xObs <- round(runif(num, lower, upper), 4) # Generate 'observations'
    forcingDataObs[,i] <- c(x, xObs) # Add the observations to the matrix
  }
  write.table(forcingDataObs, f, row.names = FALSE, col.names = FALSE) # Write matrix to a csv file
}