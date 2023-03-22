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
    cutoff = c(0, 5), # only for Appendix


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
            specificity_correction_elasso = calculate_sensitivity(correction_elasso, true_network)
            
        )
    }
)

results
warnings()
#save simulation results to external file
saveRDS(results, "results_100_runs_multi.RDS")

#loading a large population all with at least a sum score of 5
population <- readRDS("objects/large_population_sim.RDS") %>% slice_sample(n = 400,000)
population <- readRDS("objects/large_population_sim.RDS")
true_network <- readRDS("objects/true_network.RDS")

#estimating the networks depends on the esimation method
regular <- IsingFit(population, plot = FALSE, progressbar = FALSE)$weiadj
correction <- IsingFit_correction(large_severe_pop, sumscore = 5, plot = FALSE, progressbar = FALSE)$weiadj

results_single_run <- data.frame(
  Sensitivity_regular = calculate_sensitivity(regular, true_network),
  Specificity_regular = calculate_specificity(regular, true_network),
  Correlation_regular = calculate_correlation(regular, true_network),
  SpurNegEdges_regular = calculate_spur_neg_edges(regular, true_network),
  Estimation_error_regular = calculate_estimation_error(regular, true_network),
  Sensitivity_correction = calculate_sensitivity(correction, true_network),
  Specificity_correction = calculate_specificity(correction, true_network),
  Correlation_correction = calculate_correlation(correction, true_network),
  SpurNegEdges_correction = calculate_spur_neg_edges(correction, true_network),
  Estimation_error_correction = calculate_estimation_error(correction, true_network)
)

#save to external file
saveRDS(results_single_run, "results_large_pop.RDS")
population

sum(results$error)


