# This script is for sorting a population of individuals based on their fitness values.
# The format of the population will be a list of lists. In the population, each element is:
# [individual, fitness].
# The sorting algorithm that will be used is merge sort.

merge <- function(left, right) {
  leftInd <- 1
  rightInd <- 1
  sortedPop <- list()
  
  # Compare the fitness values of the current individual in the left and right lists.
  # Whichever individual has a higher fitness will be added to the sorted population and that
  # list's index will be increased. Continue looping until we exhaust one of the lists.
  while (leftInd <= length(left) && (rightInd <= length(right))) {
    
    # Left side's fitness is greater
    if (left[[leftInd]][[2]] >= right[[rightInd]][[2]]) {
      sortedPop <- append(sortedPop, list(left[[leftInd]]))
      leftInd <- leftInd + 1
      
    # Right side's fitness is greater
    } else {
      sortedPop <- append(sortedPop, list(right[[rightInd]]))
      rightInd <- rightInd + 1
    }
  }
  
  # Once one of the list's individuals have been exhausted, add any remaining individuals to the
  # sorted population.
  while (leftInd <= length(left)) {
    sortedPop <- append(sortedPop, list(left[[leftInd]]))
    leftInd <- leftInd + 1
  }
  
  while (rightInd <= length(right)) {
    sortedPop <- append(sortedPop, list(right[[rightInd]]))
    rightInd <- rightInd + 1
  }
    
    return (sortedPop)
}

mergeSort <- function(popFit) {
  
  # If there is only 1 individual, return the individual.
  if (length(popFit) == 1) {
    return (popFit)
  }

  # Middle of the population, used to split the population into halves.
  middle <- length(popFit)%/%2
  leftHalf <- popFit[1:middle]
  rightHalf <- popFit[(middle+1):length(popFit)]
  
  # Recursively call the halves until they have only a single element
  left <- mergeSort(leftHalf)
  right <- mergeSort(rightHalf)
  
  # The logic here is that left will continuously call until there is only 1 element.
  # At this point, right will call until there is only 1 element. Then, we merge.
  # When the merge is returned, the next mergeSort call 'layer' will start with the
  # 'merged so far' list.

  sortedPopFit <- merge(left, right)
  
  return(sortedPopFit)
}

DEBUG <- 0
if (DEBUG) {
  source("genPop.R")
  p <- genPop(10, "n")
  source("getOptimal.R")
  f <- "forcingData.csv" # The file of forcing data observations.
  optimalVals <- optimal(f) # Get optimal parameters.

}