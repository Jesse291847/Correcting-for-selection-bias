library(parSim)


### SIMULATION SETUP ###
results <- parSim(
  
    # Conditions
    sample_size = c(500, 1000, 2500, 5000),
    cutoff = c(5),


    # Setup:
    write = FALSE,
    reps = 5,
    debug = FALSE,
    progressbar = TRUE,
    nCores = 12,
  

    # The simulation code:
    expression = {
      
      library(parSim)
      library(IsingSampler)
      library(igraph)
      library(qgraph)
      library(tidyverse)
      library(reshape2)
      library(IsingFit)
      library(glmnet)
      library(psychonetrics)
      source("comparison_functions.R")
      
      population <- as.data.frame(readRDS("objects/small_population_simulation.RDS"))
      true_network <- readRDS("objects/network_sim.RDS")

        # Select data:
        
        eligible <- population[rowSums(population) >= cutoff,]
        selection <- eligible %>% slice_sample(n = sample_size)

        # network estimation
        networks <- list(IF_cor = IsingFit(selection, min_sum = cutoff, plot = FALSE)$weiadj,
        IF_unc = IsingFit(selection, plot = FALSE)$weiadj,
        
        IS_cor = EstimateIsingUni(selection, min_sum = cutoff, thresholding = TRUE, alpha = 0.01)$graph,
        IS_unc = EstimateIsingUni(selection, thresholding = TRUE, alpha = 0.01)$graph,
        IS_cor_unr = EstimateIsingUni(selection, min_sum = cutoff, thresholding = FALSE)$graph,
        IS_unc_unr = EstimateIsingUni(selection, thresholding = FALSE)$graph,
        
        psy_cor = Ising(selection, min_sum = 5) %>% runmodel() %>% prune(alpha = 0.01) %>% getmatrix("omega"),
        psy_unc = Ising(selection) %>% runmodel() %>% prune(alpha = 0.01) %>% getmatrix("omega"),
        psy_cor_unr = Ising(selection, min_sum = 5) %>% runmodel()  %>% getmatrix("omega"),
        psy_unc_unr = Ising(selection) %>% runmodel() %>% getmatrix("omega"))
        
      results <- lapply(networks, function(est_network) {
          calculate_metrics(est_network, true_network)})
      
      do.call(data.frame, results)
        
       
    }
)

#save simulation results to external file
saveRDS(results, "objects/results_100_runs.RDS")



