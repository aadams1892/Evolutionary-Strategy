# Generate a random initial population.

genPop <- function(popSize, strat, mss, lower, upper) {
  pop <- list()
  counter <- 0
  upper <- as.numeric(upper)
  lower <- as.numeric(lower)

  while (counter < popSize) {
    indiv <- as.list(round(runif(5, lower, upper), 4))
    
    # 1 mutation step size for the entire population
    if (strat == 1) {
      indiv <- append(indiv, as.numeric(mss))
      
    # n mutation step sizes
    } else if (strat == "n") {
      mutationList <- rep(list(as.numeric(mss)), length(indiv))
      indiv <- append(indiv, mutationList)
      
    # Invalid strategy
    } else {
      print("Invalid mutation strategy. Options are '1' and 'n'.")
      break
    }
    
    pop <- append(pop, list(indiv))
    counter <- counter + 1
  }
  return(pop)
}

DEBUG <- 0
if (DEBUG) {
  p <- genPop(10, 1, 0.5, 0, 1)
  print(p[1])
}