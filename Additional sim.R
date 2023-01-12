library(IsingSampler)
library(tidyverse)
library(IsingFit)
library(parSim)

results <- parSim(
  
  # Conditions
  sample_size = 400000,
  
  
  
  # Setup:
  write = FALSE,
  reps = 100,
  debug = FALSE,
  nCores = 16,
  
  # The simulation code:
  expression = {
    library(IsingSampler)
    library(tidyverse)
    library(IsingFit)
    library(parSim)
    
    source("comparison_functions.R")
    source("IsingFit_correction.R")
    
    #loading true network
    weights <- readRDS("objects/true_network.RDS")
   
    
    # Select data:
    population <- readRDS("objects/large_population_sim.RDS") %>% slice_sample(n = sample_size)
    
    # network estimation
    
    eLasso <- IsingFit(population, plot = FALSE, progressbar = FALSE)$weiadj
    unregularized <- univariate(population, 0)
   
    
    
    # Make a data frame with one row to return results:
    data.frame(
      Sensitivity_eLasso = calculate_sensitivity(eLasso, weights),
      Specificity_eLasso = calculate_specificity(eLasso, weights),
      Correlation_eLasso = calculate_correlation(eLasso, weights),
      SpurNegEdges_eLasso = calculate_spur_neg_edges(eLasso, weights),
      Estimation_error_eLasso = calculate_estimation_error(eLasso, weights),
      Estimation_error_unregularized = calculate_estimation_error(unregularized, weights),
      Correlation_unregularized  = calculate_correlation(unregularized , weights),
      SpurNegEdges_unregularized  = calculate_spur_neg_edges(unregularized , weights)
     
    )
  })
  
saveRDS(results, "Lasso_vs_unregularized.RDS") 


options(scipen = 999)
round(sapply(results, mean), digits = 3)
round(sapply(results, sd), digits = 3)


