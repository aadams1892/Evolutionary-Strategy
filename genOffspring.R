# An R script for generating an offspring population.
genOffspring <- function(pop, strat, popFactor) {
  # The list where all the offspring will go
  offspringPop <- list()
  numOffspring <- popFactor * as.numeric(length(pop))
  indivLength <- length(pop[[1]])
  
  # Generate offspring until the population is full
    
  # 1 mutation step size
  if (strat == 1) {
    
    while (length(offspringPop) < numOffspring) {
      offspring <- list()
      
      # Perform discrete crossover for the genes of the offspring.
      for (i in 1:(indivLength-1)) {
        parents <- getParents(pop)
        parent1 <- parents[[1]]
        parent2 <- parents[[2]]
        g <- discCrossover(parent1[[1]][[i]], parent2[[1]][[i]])
        offspring <- append(offspring, g)
      }
      
        # Perform intermediate crossover on the mutation step size.
        parents <- getParents(pop)
        parent1 <- parents[[1]]
        parent2 <- parents[[2]]
        # The mutation step size is the last element of the parents.
        mss <- interCrossover(parent1[[1]][[indivLength]], parent2[[1]][[indivLength]])
        offspring <- append(offspring, mss)
        
        # Add the finished offspring to the offspring population.
        offspringPop <- append(offspringPop, list(offspring))
    }
    
  # n mutation step sizes
  } else if (strat == "n") {
    
    while (length(offspringPop) < numOffspring) {
      offspring <- list()
      
      # Perform discrete crossover for the genes of the offspring.
      for (i in 1:(indivLength/2)) {
        parents <- getParents(pop)
        parent1 <- parents[[1]]
        parent2 <- parents[[2]]
        g <- discCrossover(parent1[[1]][[i]], parent2[[1]][[i]])
        offspring <- append(offspring, g)
      }
      
      # Perform intermediate crossover on the mutation step sizes.
      for (i in ((indivLength/2)+1):indivLength) {
        parents <- getParents(pop)
        parent1 <- parents[[1]]
        parent2 <- parents[[2]]
        mss <- interCrossover(parent1[[1]][[i]], parent2[[1]][[i]])
        offspring <- append(offspring, mss)
      }
      
      # Add the finished offspring to the offspring population.
      offspringPop <- append(offspringPop, list(offspring))
    }
  }
  
  return (offspringPop)
}

DEBUG <- 0
if (DEBUG) {
  source("genPop.R")
  p <- genPop(10, "n")
  op <- genOffspring(p, "n")
  print(op[1])
}
