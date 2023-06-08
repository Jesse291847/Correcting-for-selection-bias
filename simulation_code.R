library(parSim)
library(IsingSampler)
library(igraph)
library(tidyverse)
library(reshape2)
library(IsingFit)
library(glmnet)
source("comparison_functions.R")
source("estimators.R")

# population <- readRDS("objects/population_simulation.RDS")
# true_network <- readRDS("objects/true_network.RDS")


### SIMULATION SETUP ###
results <- parSim(
  
    # Conditions
    sample_size = c(500, 1000, 2500, 5000),
    cutoff = c(0, 5, 8), # only for Appendix


    # Setup:
    write = FALSE,
    reps = 100,
    debug = FALSE,
    progressbar = TRUE,
    nCores = 12,
  

    # The simulation code:
    expression = {
      
      library(parSim)
      library(IsingSampler)
      library(igraph)
      library(tidyverse)
      library(reshape2)
      library(IsingFit)
      library(glmnet)
      source("comparison_functions.R")
      source("estimators.R")
      
      population <- as.data.frame(readRDS("objects/small_population_simulation.RDS"))
      true_network <- readRDS("network_sim.RDS")

        # Select data:
        eligible <- population[rowSums(population) >= cutoff, ]
        selection <- eligible %>% slice_sample(n = sample_size)

        # network estimation
        regular_univariate <- univariate(selection, min_sumscore = 0)
        #regular_multivariate <- multivariate(selection, min_sumscore = 0)
        regular_elasso <- IsingFit(selection, progressbar = FALSE, plot = FALSE)$weiadj
        correction_univariate <- univariate(selection, min_sumscore = cutoff)
        correction_elasso <- IsingFit_correction(selection, progressbar = FALSE, plot = FALSE, min_sumscore = cutoff)$weiadj
        correction_multivariate <- multivariate(selection, cutoff)
        


        # Make a data frame with one row to return results:
        data.frame(
            estimation_error_regular_univariate = calculate_estimation_error(regular_univariate, true_network),
            #estimation_error_regular_multivariate = calculate_estimation_error(regular_multivariate, true_network),
            estimation_error_regular_elasso = calculate_estimation_error(regular_elasso, true_network),
            sensitivity_regular_elasso = calculate_sensitivity(regular_elasso, true_network), 
            specificity_regular_elasso = calculate_specificity(regular_elasso, true_network),
            
            estimation_error_correction_univariate = calculate_estimation_error(correction_univariate, true_network),
            estimation_error_correction_multivariate = calculate_estimation_error(correction_multivariate, true_network),
            estimation_error_correction_elasso = calculate_estimation_error(correction_elasso, true_network),
            sensitivity_correction_elasso = calculate_sensitivity(correction_elasso, true_network), 
            specificity_correction_elasso = calculate_specificity(correction_elasso, true_network)
            
        )
    }
)

#save simulation results to external file
saveRDS(results, "results_100_runs_multi.RDS")

#loading a large population all with at least a sum score of 5 (still 14 symptoms FIX!)
population <- readRDS("objects/large_population_simulation.RDS")
true_network <- readRDS("objects/network_sim.RDS")
severe <- population[rowSums(population) >= 5,] %>% as.data.frame() %>% slice_sample(n = 500000)

#estimating the networks depends on the estimation method
regular <- univariate(severe, 0)
regular_elasso <- IsingFit(severe, plot = FALSE, progressbar = FALSE)
correction_univariate <- univariate(severe, 5)
correction_multivariate <- multivariate(severe, 5)
correction_elasso <- IsingFit_correction(severe, min_sumscore = 5, plot = FALSE, progressbar = FALSE)


results_single_run <- data.frame(
  estimation_error_regular_univariate = calculate_estimation_error(regular, true_network),
  #estimation_error_regular_multivariate = calculate_estimation_error(regular_multivariate, true_network),
  estimation_error_regular_elasso = calculate_estimation_error(regular_elasso$weiadj, true_network),
  sensitivity_regular_elasso = calculate_sensitivity(regular_elasso$weiadj, true_network), 
  specificity_regular_elasso = calculate_specificity(regular_elasso$weiadj, true_network),
  
  estimation_error_correction_univariate = calculate_estimation_error(correction_univariate, true_network),
  estimation_error_correction_multivariate = calculate_estimation_error(correction_multivariate, true_network),
  estimation_error_correction_elasso = calculate_estimation_error(correction_elasso$weiadj, true_network),
  sensitivity_correction_elasso = calculate_sensitivity(correction_elasso$weiadj, true_network), 
  specificity_correction_elasso = calculate_sensitivity(correction_elasso$weiadj, true_network)
)


#save to external file
saveRDS(results_single_run, "objects/results_large_pop.RDS")




