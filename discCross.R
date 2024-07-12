# Function that performs discrete crossover.

discCrossover <- function(par1Gene, par2Gene) {
  prob <- runif(1, 0, 1)
  # Parent 1 gene
  if (prob <= 0.5) {
    return (par1Gene)
  # Parent 2 gene
  } else {
    return (par2Gene)
  }
}