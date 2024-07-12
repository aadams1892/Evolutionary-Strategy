# Script for managing and submitting the parallel fitness evaluations.

parallelFitEval <- function(segmentedPop, func, optimals, jobName, num_nodes, 
                              cpus_per_node, lib_paths, options) {

  library(rslurm)
  
  # Before submitting the jobs, check if a folder with the job name exists and remove it
  # if it does.
  jobFolder <- paste0("_rslurm_", jobName)
  if (file.exists(paste0(getwd(), "/", jobFolder))) {
    cmd <- paste0("rm -rf ", jobFolder)
    system(cmd)
  }
  Sys.sleep(2)
  
  # Submit the parallel fitness evaluation jobs.
  fit <- slurm_map(x = segmentedPop, f = func, obs = optimals, 
                   jobname = jobName, nodes = num_nodes, cpus_per_node = 1,
                   libPaths = lib_paths, slurm_options = options)
  
  # Wait for a short time to ensure the job folder gets created.
  Sys.sleep(2)
  
  nodes <- fit$nodes # Number of nodes used
  results_file <- paste0("results_", nodes-1, ".RDS") # Results file for last node.
  
  # Wait until the results have been calculated.
  filePath <- paste0(getwd(), "/", jobFolder, "/", results_file)
  while(!file.exists(filePath)) {
    Sys.sleep(5)
  }
  Sys.sleep(5) # Wait for a short time to ensure everything is loaded.
  
  # Read in all the results into fitness segments corresponding to the offspring segments.
  results <- list()
  # Go over every segment
  for (seg in 1:nodes) {
    resultFile <- paste0("results_", seg-1, ".RDS") # Results for current segment
    filePath <- paste0(getwd(), "/", jobFolder, "/", resultFile)
    segmentResults <- readRDS(filePath) # Read results for segment
    # Add the results for the segment to the results list as a single element equal so that
    # it corresponds to the segment of individuals.
    results <- append(results, segmentResults)
  }
  
  # Combine the individuals with their fitness value.
  popFit <- list()
  # For every segment s
  for (s in 1:length(segmentedPop)) {
    
    # For every index i in segment s
    for (i in 1:length(segmentedPop[[s]])) {
      # Combine the individual and their fitness.
      indiv <- segmentedPop[[s]][[i]]
      fit <- results[[s]][[i]]
      indivFit <- list(indiv, fit)
      popFit <- append(popFit, list(indivFit))
    }
  }

  return (popFit)
}