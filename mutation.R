# Performs mutation on an individual. The mutation operator depends on the mutation strategy.
mutate <- function(indiv, strat, epsilon) {
  
  # 1 step size
  if (strat == 1) {
    tau <- 1/sqrt(length(indiv)) # Learning rate
    ex <- tau*rnorm(n = 1) # Default values for rnorm are mean = 0 and sd = 1.
                         # n is the number of observations we are making.
                         # tau*N(0,1) = N(0, tau)
  
    sigma <- as.numeric(indiv[length(indiv)]) # For UMOSS, the last value of the individual
                                              # is the individual is the mutation step size 
                                              # for the entire individual.
    
    # Mutating σ in UMOSS is done using: σ' = σ * e^(τ*N(0,1))
    sigmaPrime <- sigma * as.numeric(exp(ex))
    
    # Check that sigmaPrime has not crossed the epsilon boundary. If it has, set it to the
    # boundary value.
    if (sigmaPrime < epsilon) {
      sigmaPrime <- epsilon
    }
    
    # Use sigmaPrime to update all the genes
    # Mutating gi in UMOSS is done using the following equation: gi' = gi + σ' * Ni(0,1)
    for (g in 1:(length(indiv)-1)) { # Loop through all but the last gene of the individual.
      gPrime <- as.numeric(indiv[g]) + (sigmaPrime*rnorm(n = 1))
      indiv[g] <- round(gPrime, 4)
    }
    
    # After mutating all the genes, add sigmaPrime to the end of the individual.
    indiv[length(indiv)] <- round(sigmaPrime, 4)
    
  # n step sizes
  } else if (strat == "n") {
    
    tau <- 1/sqrt(2*sqrt(length(indiv)))
    tauPrime <- 1/sqrt(2*length(indiv))
    # Mutating σi in UMNSS is done using: σi' = σi * e^(τ'*N(0,1) + τ*Ni(0,1))
    # - Note that Ni(0,1) is used in the mutation equation for both σ and g. 
    #   I will treat this as the same distribution with the same value.
    
    # Calculate sigmaPrime for each mutation step size and mutate the corresponding gene.
    
    for (i in ((length(indiv)/2)+1):length(indiv)) {
      gaussian <- rnorm(n = 1)
      sigma <- indiv[i]
      sigmaPrime <- as.numeric(sigma) * as.numeric(exp((tauPrime*rnorm(n = 1)) + (tau*gaussian)))
      
      # Check that sigmaPrime has not crossed the epsilon boundary. If it has, set it to the
      # boundary value.
      if (sigmaPrime < epsilon) {
        sigmaPrime <- epsilon
      }
      
      indiv[i] <- round(sigmaPrime, 4)
        
      # Mutating gi in UMNSS is done using the following equation: gi' = gi + σi' * Ni(0,1)
      g <- i-(length(indiv)/2)
      #cat("indiv[g]: ", as.numeric(indiv[g]), "\n")
      #cat("gaussian: ", gaussian, "\n")
      #cat("sigmaPrime:", sigmaPrime, "\n")
      gPrime <- as.numeric(indiv[g]) + (sigmaPrime * gaussian)
      #cat("gPrime:", gPrime, "\n")
      #cat("g -> gPrime:", as.numeric(indiv[g]), "->", gPrime, "\n")
      indiv[g] <- round(gPrime, 4)
      #cat("\n\n")
    }
  }
  
  return (indiv)
}

DEBUG <- 0
if (DEBUG) {
  h <- mutate(list(1, 2, 3, 4, 5, 6, 5, 4, 3, 2), "n")
  print(h)
}