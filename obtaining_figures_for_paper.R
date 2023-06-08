library(qgraph)
library(ggplot2)
library(tidyverse)
library(IsingSampler)
library(IsingFit)
library(ggdag)
library(glmnet)
source("estimators.R")


#create histogram
set.seed(4)
severe <- sample(0:9, 1000, replace = TRUE, prob = c(0.1, 0.7, 1.2, 1.8, 2, 3.7, 4.5, 5.1, 4.4, 3.5))
healthy <- sample(0:4, 1000, replace = TRUE, prob = c(5, 2.7, 2, 1.5, 0.8))
pdf("figures/hist_intro.pdf")
hist(severe, col=rgb(1,0,0,0.5), xlim=c(0,9), ylim = c(0, 400),  breaks = 0:9, xlab = "Sum score", main = "")  
hist(healthy, col=rgb(0,0,1,0.5), xlim=c(0,9), add = T, breaks = 0:5, right = FALSE)
abline(v = 5, lty = "dashed", lwd = 2.5)
axis(side=1,at=seq(0,9,1),labels=seq(0,9,1))
legend('topright', c('Severe population', 'Healthy population'), 
       fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)), xpd = TRUE, cex = 0.8, inset = c(-0.05,0))
dev.off()

### constructing plot for true network ###

weights <- read.delim("Empirical_data/EmpiricalWeightParameters.txt")
rownames(weights) <- colnames(weights)
thresholds <- read.delim("Empirical_data/EmpiricalThresholdParameters.txt", header = FALSE)

sim_data <- as.data.frame(IsingSampler(400000, as.matrix(weights), pull(thresholds), method = "CFTP"))
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

saveRDS(thresholds, "thresholds_sim.RDS")
saveRDS(weights, "network_sim.RDS")

sim_data <- IsingSampler(400000, weights, thresholds, method = "CFTP")
readRDS(sim_data, "objects/small_population_simulation.RDS")

# SE: One large dataset for all simulations:
large_population <- as.data.frame(IsingSampler(10000000, as.matrix(weights), pull(thresholds), method = "CFTP"))

# Save the object:
saveRDS(large_population, "objects/large_population_sim.RDS")



network <- readRDS("network_sim.RDS")

pdf("sim_network")
qgraph(network, layout = "spring", theme = "colorblind")
dev.off()

#summary statistics
sum(network[upper.tri(network)] == 0)
mean(network[upper.tri(network)])
length(network[upper.tri(network)])


### Empirical example ###
NCS <- read.csv("Empirical_data/NCS.csv")
labels <- c("depr", "inte", "weig", "sle", "motor", "fat", "repr", "con", 
            "suic", "anxi", "even", "ctrl", "edge", "gFat", "irri", "gCon", 
            "musc", "gSle")

MDD <- NCS %>% filter(V1 == 1) %>% select(paste0("V", 2:9)) 
severe <- MDD[rowSums(MDD) >= 4,]
regular <- IsingFit(severe, plot = FALSE, progressbar = FALSE)$weiadj
correction <- IsingFit_correction(severe, plot = FALSE, progressbar = FALSE, min_sumscore = 4)$weiadj

AL <- averageLayout(regular, correction)

pdf("figures/Empirical example correction selection bias.pdf", width = 10, height = 5)
par(mfrow = c(1,2))
qgraph(regular, theme = "colorblind", labels = labels[2:9], layout = AL, 
       title = "Estimation with IsingFit", maximum = max(c(regular, correction)))
qgraph(correction, theme = "colorblind", labels = labels[2:9], layout = AL, 
       title = "Estimation with correction", max(c(regular, correction)))
dev.off()

# # create collider plot
# collider_plot <- collider_triangle(x = "Motivation", y = "Intelligence", "Honors") %>%
#   ggdag_dseparated(controlling_for = "m", text = FALSE, use_labels = "label") +
#   theme_dag()
# 
# pdf("Collider_triangle.RDS")
# collider_plot
# dev.off()
# 
# # create distribution plot
# func_shaded <- function(x) {
#   y <- dnorm(x, mean = 0.5, sd = 0.15)
#   y[x < 0.7] <- NA
#   return(y)
# }
# 
# pdf("Illustration_selection.pdf")
# ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
#   stat_function(fun = dnorm, args = list(0.5, 0.15)) +
#   stat_function(fun = func_shaded, geom = "area", fill = "red", alpha = 0.5) +
#   theme_void()
# dev.off()


