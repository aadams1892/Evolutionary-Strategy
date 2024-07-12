# Function that performs intermediate crossover.
interCrossover <- function(par1Gene, par2Gene) {
  offspringGene <- mean(as.numeric(list(par1Gene, par2Gene)))
  return (offspringGene)
}

#o <- xover(6, 5)
#print(o)