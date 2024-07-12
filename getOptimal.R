# Calculate optimal parameter values. In this toy function, the means of the forcing data values
# are the optimal values.
optimal <- function(f) {
  obs <- read.table(f)
  nrows <- length(obs[,1])
  ncols <- length(obs[1,])
  #print(dim(obs))
  optimalParams <- list()
  # Get optimal (mean) values for the parameters.
  for (i in 1:ncols) {
    param <- obs[1,i]
    paramObs <- as.numeric(obs[2:nrows,i])
    optimalParams <- append(optimalParams, round(mean(as.numeric(paramObs)), 4))
  }
  return(optimalParams)
}