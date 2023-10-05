# parameters from Cramer et al. (2016)
weights <- read.delim("Empirical_data/EmpiricalWeightParameters.txt")
rownames(weights) <- colnames(weights)
thresholds <- read.delim("Empirical_data/EmpiricalThresholdParameters.txt", header = FALSE)

# will not exactly reproduce the network from the paper it is based on simulated data
sim_data <- as.data.frame(IsingSampler(40000, as.matrix(weights), pull(thresholds), method = "CFTP"))
colnames(sim_data) <- colnames(weights)

sim_data$sleep <- ifelse(sim_data$iso == 1 | sim_data$hso == 1, 1, 0) #hypersomnia and insomnia
sim_data$motor <- ifelse(sim_data$agi == 1 | sim_data$ret == 1, 1, 0) #psychomotor agitation and retardation
sim_data$weight <- ifelse(sim_data$gai == 1 | sim_data$los == 1|
                            sim_data$dap == 1 | sim_data$iap == 1, 1, 0) #weight and appetite
sim_data <- sim_data[,-c(3:10)]
network <- IsingFit(sim_data)


weights <- network$weiadj
thresholds <- network$thresholds

#otherwise no variance in this symptom
weights[-1, 1] <- weights[1,-1] <- (weights[-1, 1] * 0.4)

#we set 7 edges (roughly ten pecent) to zero to calculate sensitivity and specificity
zet_to_zero <- max(sort(weights[upper.tri(weights)])[1:7])
weights[weights <= zet_to_zero] <- 0 

#get a bit more cases to reach the sum score criterion
thresholds <- thresholds * 0.6

saveRDS(thresholds, "objects/thresholds_sim.RDS")
saveRDS(weights, "objects/network_sim.RDS")


# get a population for simulation
sim_data2 <- IsingSampler(400000, weights, thresholds, method = "CFTP")

#check for variance and enough severe cases
nrow(sim_data2[rowSums(sim_data2) > 4,])
colMeans(sim_data2[rowSums(sim_data2) > 4,])

saveRDS(sim_data2, "objects/small_population_simulation.RDS")



