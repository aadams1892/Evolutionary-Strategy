.libPaths(c("~/daisy/renv", .libPaths()))
source("sourceScripts.R")
library(parallel)
DEBUG_PRIME <- 0

startTime <- format(Sys.time(), "%b %d %Y %X")
outFile <- paste0("ES_results_", startTime, ".txt") # Output file
outFile <- gsub(" ", "_", outFile) # Replace spaces with underscores
outFile <- gsub(":", "-", outFile) # Replace colons with hyphens
bestFits <- list()

es <- function(
    strategy,
    observations,
    popSize,
    lowerBound,
    upperBound,
    sigma,
    offspringPopFactor,
    epsilon,
    parallel,
    maxIter
  ) {

  genForcingData(observations, lowerBound, upperBound) # Generate the forcing data.
  
  f <- "forcingData.csv" # The file of forcing data observations.
  optimalVals <- optimal(f) # Get optimal parameters.
  optimalFound <- FALSE
  best_fit <- list()
  mean_fit <- list()
  iter <- 0
  
  if (DEBUG_PRIME) {
    print("Generating population...")
  }
  
  # Generate an initial random population.
  gen <- genPop(popSize, strategy, sigma, lowerBound, upperBound)
  
  if (DEBUG_PRIME) {
    print("Population generated!")
    cat("Population size:", length(gen), "\n")
  }
  
  while (!optimalFound && (iter < maxIter)) {
    
    cat(paste0("Iteration #", iter+1), "\n")
  
    if (DEBUG_PRIME) {
      print("Generating offspring...")
    }
    
    # Generate the offspring population
    offspringPop <- genOffspring(gen, strategy, offspringPopFactor)
    
    br <- 0
    if (DEBUG_PRIME) {
      print("Offspring population generated. Checking...")
      for (i in offspringPop) {
        if (length(i) != length(gen[[1]])) {
          cat("ERROR: Unequal length:", length(i), ". Should be:", length(gen[1]), "\n")
          br <- 1
          break
        }
      }
    }
    
    if (br && DEBUG_PRIME) {
      break
    } else if (!br && DEBUG_PRIME) {
      print("Offspring population generated successfully!")
      cat("First individual:", unlist(offspringPop[[1]]), "\n")
    }
    
    if (DEBUG_PRIME) {
      print("Mutating offspring...")
    }
    
    # Mutate the offspring
    mutatedOffPop <- list()
    for (indiv in offspringPop) {
      mutatedIndiv <- mutate(indiv, strategy, epsilon)
      mutatedOffPop <- append(mutatedOffPop, list(mutatedIndiv))
    }
    
    if (DEBUG_PRIME) {
      print("Offspring population mutated. Checking...")
      for (i in mutatedOffPop) {
        print(unlist(i))
        if (length(i) != length(gen[[1]])) {
          cat("ERROR: Unequal length:", length(i), ". Should be:", length(gen[[1]]), "\n")
          br <- 1
          break
        }
      }
    }
    
    if (br && DEBUG_PRIME) {
      break
    } else if (!br && DEBUG_PRIME) {
      print("Offspring population mutated successfully!")
    }
    
    if (DEBUG_PRIME) {
      print("Selecting next generation...")
    }
    
    # Select the survivors. 
    # Here, the survivor selection script also calculates the fitness of all
    # the individuals and sorts them.
    nextGen <- mu_lambda(gen, mutatedOffPop, parallel)
    
    if (DEBUG_PRIME) {
      print("Next generation selected. Checking...")
      if (length(nextGen) != length(gen)) {
        cat("ERROR: Unequal generation lengths:", length(nextGen), "Should be:", length(gen), "\n")
        br <- 1
        break
      }
    }
    
    if (br && DEBUG_PRIME) {
      break
    } else if (!br && DEBUG_PRIME) {
      print("Next generation selected successfully!")
    }
    
    gen <- nextGen
    # Get best individual
    bestIndiv <- gen[[1]][[1]]
    bestFit <- gen[[1]][[2]]
    bestFits <- append(bestFits, 1-(abs(bestFit)))
    
    # Check if termination conditions have been met.
    # No difference with optimal vals
    if (abs(gen[[1]][[2]]) != 0) {
      optimalFound <- TRUE
      
    # Still have not found optimal
    } else {
      
      # Remove the fitnesses for formatting purposes
      for (i in 1:length(gen)) {
        gen[[i]] <- gen[[i]][[1]]
      }
    }
    
    line1 <- paste0("Iteration #", iter+1)
    line2 <- paste0("Best individual: ")
    line3 <- paste0(list(bestIndiv))
    line4 <- paste0("Best individual's fitness: ", (1-abs(bestFit)))
    line5 <- cat("\n")
    write(line1, outFile, append = TRUE)
    write(line2, outFile, append = TRUE)
    write(line3, outFile, append = TRUE)
    write(line4, outFile, append = TRUE)
    write(line5, outFile, append = TRUE)
    
    iter <- iter + 1
  }
  
  if (optimalFound) {
    print("Optimal values found!")
  } else {
    print("Maximum iterations reached.")
  }
  
  return (list(bestFits, iter, optimalVals))
}

nodes <- 4
popSize <- 1000

evolutionaryStrategy <- es(
  strategy           = "n",
  observations       = 1000,
  popSize            = popSize,
  lowerBound         = 0,
  upperBound         = 1,
  sigma              = 0.01,
  offspringPopFactor = 6,
  epsilon            = 0.0001,
  parallel           = nodes,
  maxIter            = 50
)

endTime <- format(Sys.time(), "%b %d %Y %X")
line1 <- paste0("Optimal values: ", list(evolutionaryStrategy[[3]]))
line2 <- paste0("Population size: ", popSize)
line3 <- paste0("Start time: ", startTime)
line4 <- paste0("End time: ", endTime)
if (nodes) {line5 <- paste0("Nodes used: ", nodes)}
write(line1, outFile, append=TRUE)
write(line2, outFile, append=TRUE)
write(line3, outFile, append=TRUE)
write(line4, outFile, append=TRUE)
if (nodes) {write(line5, outFile, append=TRUE)}

bestFs <- evolutionaryStrategy[[1]]
numIters <- evolutionaryStrategy[[2]]

outPlot <- paste0("ES_plot_", endTime, ".pdf")
outPlot <- gsub(" ", "_", outPlot)
outPlot <- gsub(":", "-", outPlot)
pdf(outPlot)
plot(1:numIters, bestFs, type="l", xlab="Generation", ylab="Best Fitness")
title(main = "Best Fitness")

# Check if ES_output exists. If it doesn't, make it.
if (!file.exists("ES_output") {
    cmd <- paste0("mkdir ES_output")
    system(cmd)
}
    
# Create output folder
outFolder <- paste0(getwd(), "/ES_output/", endTime)
outFolder <- gsub(" ", "_", outFolder)
outFolder <- gsub(":", "-", outFolder)
cmd <- paste0("mkdir ", outFolder)
system(cmd)

while(!file.exists(outFolder)) {
  print("Waiting")
  print(outFolder)
  Sys.sleep(2)
}

# Move output files to output folder
outContents <- list(outFile, outPlot)
for (c in outContents) {
  cmd <- paste0("mv ", c, " ", outFolder)
  system(cmd)
}
