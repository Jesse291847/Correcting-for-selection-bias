library(IsingSampler)
library(tidyverse)
library(IsingFit)

weights <- read.delim("Empirical_data/EmpiricalWeightParameters.txt")
rownames(weights) <- colnames(weights)
thresholds <- read.delim("Empirical_data/EmpiricalThresholdParameters.txt", header = FALSE)

# because I change the parameters I don't want to show the symptom names
set.seed(4) #forgot this when creating the actual network I now simulated from
sim_data <- as.data.frame(IsingSampler(400000, as.matrix(weights), pull(thresholds), method = "CFTP"))
isSymmetric(as.matrix(weights))
colnames(sim_data) <- colnames(weights)



sim_data$sleep <- ifelse(sim_data$iso == 1 | sim_data$hso == 1, 1, 0) #hypersomnia and insomnia
sim_data$motor <- ifelse(sim_data$agi == 1 | sim_data$ret == 1, 1, 0) #psychomotor agitation and retardation
sim_data$weight <- ifelse(sim_data$gai == 1 | sim_data$los == 1| 
                          sim_data$dap == 1 | sim_data$iap == 1, 1, 0) #weight and appetite
sim_data <- sim_data[,-c(3:10)]
network <- IsingFit(sim_data)


weights <- network$weiadj
thresholds <- network$thresholds
weights[-1, 1] <- round(weights[-1, 1] * 0.4, digits = 4)
weights[upper.tri(weights)] <- t(weights)[upper.tri(weights)]


rownames(weights) <- colnames(weights)
thresholds <- thresholds * 0.7

saveRDS(thresholds, "objects/thresholds_sim.RDS")
saveRDS(weights, "objects/network_sim.RDS")

sim_data <- IsingSampler(400000, weights, thresholds, method = "CFTP")
saveRDS(sim_data, "objects/small_population_simulation.RDS")

#large population for 500,000
sim_data <- IsingSampler(2000000, weights, thresholds, method = "CFTP")
saveRDS(sim_data, "objects/large_population_simulation.RDS")

