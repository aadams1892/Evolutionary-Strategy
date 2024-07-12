# The cost (fitness) function of the GA.
# Uses the forcing data to calculate the current parameter predictions.

costfunc <- function(X, obs) {
  # Calculate the difference between the current prediction and the mean of the observed means.
  # gpp, lai, hfls, hfss, albd
  fitness <- list()
  
  # Go through all individuals in the list. If there is only 1 individual, then this will still
  # work since R treats everything as a list.
  for (indiv in X) {
    dif <- 0 # Difference between x and observations
    
    # Only the first 5 indices of the individual are the genes (the rest are/is mutation step size(s))
    for (i in 1:5) {
      dif <- dif + abs(as.numeric(indiv[i]) - as.numeric(obs[i])) *-1
    }
    fitness <- append(fitness, dif)
    
    Sys.sleep(0.0002) # Un-comment for parallel-sequential comparisons
  }
  return (fitness)
}

DEBUG <- 0
if (DEBUG) {
  source("genPop.R")
  p <- genPop(10, 1)
  source("getOptimal.R")
  f <- "forcingData.csv"
  opt <- optimal(f)
  for (i in p) {
    print(unlist(i))
    print(unlist(opt))
    c <- costfunc(i, opt)
    print(c)
    cat("\n\n")
  }
}