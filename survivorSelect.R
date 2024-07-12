# This script implements (mu, lambda) survivor selection.
mu_lambda <- function(mu, lambda, parallel) {
  # There are lambda children generated from mu parents.
  # Lambda should be much larger than mu, often lambda = 7mu.
  # (mu, lambda) discards all parents and picks the mu best of the lambda to be
  # the next generation.
  n <- length(mu) # Population size
  nextGen <- list()

  f <- "forcingData.csv" # The file of forcing data observations.
  optimalVals <- optimal(f) # Get optimal parameters.
  
  # Rank the offspring on fitness.
  if (parallel != FALSE) {
    
    # Split up the population into x = parallel number of segments with each segment getting executed on a
    # different node.
    segmentedLambda <- list()
    segLength <- length(lambda)%/%parallel
    origLambda <- lambda # copy of lambda
    
    for (s in 1:parallel) {
      seg <- lambda[1:segLength]
      segmentedLambda <- append(segmentedLambda, list(seg))
      lambda <- lambda[(segLength+1):length(lambda)]
    }
    
    # After this loop, it says lambda still has 2 elements. The first element is NULL and the
    # second element is the last element in the last segment of segmentedLambda. This likely
    # remains in lambda since we can't remove every element from the list.
    lambda <- list()
    
    # Perform parallel fitness evaluations.
    nextGen <- parallelFitEval(segmentedPop = segmentedLambda, func = costfunc, 
                             optimals = optimalVals, jobName = "fitParallel", 
                             num_nodes = parallel, cpus_per_node = 1,
                             lib_paths = "~/projects/def-cseiler-ab/aadams/AMBER/renv/",
                             options = list(time='00:01:00'))
    
  # Sequential fitness evaluation
  } else {
  
    for (indiv in lambda) {
      fit <- costfunc(list(indiv), optimalVals)
      indivFit <- list(indiv, as.numeric(fit)) 
      nextGen <- append(nextGen, list(indivFit))
    }
  }

  nextGen <- mergeSort(nextGen)
  
  return (nextGen[1:n])
}

DEBUG <- 0
if (DEBUG) {
  source("genPop.R")
  p <- genPop(10, "n")
  source("genOffspring.R")
  o <- genOffspring(p, "n")
  ml <- mu_lambda(p, o)
  
  for (i in ml) {
    print(unlist(i))
  }
}