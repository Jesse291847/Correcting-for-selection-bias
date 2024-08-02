## THIS WILL PROBABLY REPLACE THE EMPRICAL EXAMPLE SCRIPT
library(IsingSampler)
library(IsingFit)
library(bootnet)
library(dplyr)
library(qgraph)


# load the data
NCS <- read.csv("Empirical_data/NCS.csv")

# select the data for just depression symptoms with variance
MDE <- NCS[, paste0("V", 2:9)] 


networks_unc <- list()
networks_cor <- list()
used_data <- list()

#estimate both corrected and uncorrected networks with cutoffs 0 to 5
cutoffs <- 0:5
for (i in seq_along(cutoffs)) {
  
  used_data[[i]] <- MDE[rowSums(MDE) >= cutoffs[i],] 
  
  networks_unc[[i]] <- estimateNetwork(used_data[[i]], default = "IsingSampler")
  networks_cor[[i]] <- estimateNetwork(used_data[[i]], min_sum = cutoffs[i], default = "IsingSampler")
  
}

AV <- averageLayout(networks_cor[[1]]$graph, networks_cor[[2]]$graph, 
                    networks_cor[[3]]$graph, networks_cor[[4]]$graph, 
                    networks_cor[[5]]$graph, networks_cor[[6]]$graph)
max <- max(networks_cor[[1]]$graph, networks_cor[[2]]$graph,
           networks_cor[[3]]$graph, networks_cor[[4]]$graph, 
           networks_cor[[5]]$graph, networks_cor[[6]]$graph)
# plot the resulting 12 networks

# pdf("corrected empirical networks.pdf")
par(mfrow = c(2,3))
for(i in 1:6) {qgraph(networks_cor[[i]]$graph, layout = AV, maximum = max, theme = "colorblind", title =
                        paste("Corrected estimation cutoff =", cutoffs[i]))}
#dev.off()

#pdf("uncorrected empirical networks.pdf")
par(mfrow = c(2,3))
for(i in 1:6) {qgraph(networks_unc[[i]]$graph, layout = AV, maximum = max, theme = "colorblind", title =
                        paste("Uncorrected estimation cutoff =", cutoffs[i]))}
#dev.off()

# The higher the cutoff the more the uncorrected networks seem to be pushed towards being negative

# For the uncorrected networks we expect all the cutoff values to result in the same network based on the simulation
# However, the network with cutoff = 0 has much stronger connections than the other networks


#if we now take the parameters from the estimated cutoff = 1 corrected network 
#we fail to recover the sum-score distribution, we seem to miss the healthy population
post_check <- IsingSampler(10000, networks_cor[[2]]$graph, networks_cor[[2]]$intercepts)

par(mfrow = c(1,2))
hist(rowSums(post_check), breaks = 0:8, main = "Sim cutoff = 1 network")
hist(rowSums(MDE), breaks = 0:8)


#The same holds if we simply take the parameters from cutoff = 0!
#we seem to miss the severe population
post_check1 <- IsingSampler(10000, networks_cor[[1]]$graph, networks_cor[[1]]$intercepts)

par(mfrow = c(1,2))
hist(rowSums(post_check1), breaks = 0:8, main = "Sim cutoff = 1 network" )
hist(rowSums(MDE), breaks = 0:8, main = "Original data")

# I think this indicates that a single Ising Model does not capture the data well, as associations differ between subgroups


#When I use simulated data from a homogeneous population, I can get the original sum score distribution back
#using either cutoff = 0  or cutoff = 1+, as I show here

sim_network <- readRDS("objects/network_sim.RDS")
sim_threshold <- readRDS("objects/thresholds_sim.RDS")
population <- as.data.frame(readRDS("objects/small_population_simulation.RDS"))

est <- population[rowSums(population) >= 1, ] %>% slice_sample(n = 2500) %>% 
  estimateNetwork(default = "IsingSampler", min_sum = 1)

postcheck2 <- IsingSampler(10000, est$graph, est$intercepts)

# I get the original distribution with cutoff = 1 and correction
par(mfrow = c(1,2))
hist(rowSums(postcheck2), breaks = 0:9, main = "Sim fromcutoff = 1 network")
hist(rowSums(population), breaks = 0:9, main = "Original simdata")

# I get the original distribution with cutoff = 0 and regular estimation
est <- population %>% slice_sample(n = 2500) %>% 
  estimateNetwork(default = "IsingSampler")

postcheck3 <- IsingSampler(10000, est$graph, est$intercepts)

par(mfrow = c(1,2))
hist(rowSums(postcheck3), breaks = 0:9, main = "Sim fromcutoff = 1 network")
hist(rowSums(population), breaks = 0:9, main = "Original simdata")


# In this case using the uncorrected parameter from a cutoff = 1 network will not get back the full population
est <- population[rowSums(population) >= 1, ] %>% slice_sample(n = 2500) %>% 
  estimateNetwork(default = "IsingSampler")

postcheck4 <- IsingSampler(10000, est$graph, est$intercepts)

par(mfrow = c(1,2))
hist(rowSums(postcheck4), breaks = 0:9, main = "Unccorected network sim")
hist(rowSums(population), breaks = 0:9, main = "Uncorrected sim data")

# This tells us that there is evidence for heterogeneity in the empirical sample because the networks differ

# We could not have made this claim based on the uncorrected cutoff network, because
# they will always suggest heterogeneity (Also according to Jonas' paper)
