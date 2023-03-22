library(qgraph)
library(IsingSampler)
library(IsingFit)
source("comparison_functions.R")
source("estimators.R")

set.seed(3)

#more vulnerable graph
Graph <- matrix(sample(0:1,9^2,TRUE,prob = c(0.5, 0.5)),9,9) * rnorm(9^1.5, 0.75)
Graph[lower.tri(Graph)] = t(Graph)[lower.tri(Graph)]
diag(Graph) <- 0
Thresh <- -(rnorm(9)^2)

#'healthy' graph
Graph2 <- matrix(sample(0:1,9^2,TRUE,prob = c(0.6, 0.4)),9,9) * rnorm(9^2, 0.5)
Graph2[lower.tri(Graph2)] = t(Graph2)[lower.tri(Graph2)]
diag(Graph2) <- 0
Thresh2 <- -(rnorm(9)^2)

pdf("example_networks.pdf")
par(mfrow = c(1,2))
qgraph(Graph, theme ="colorblind", labels = paste0(rep("S", 9), 1:9), maximum = max(c(Graph, Graph2)))
qgraph(Graph2, theme = "colorblind", labels = paste0(rep("S", 9), 1:9), maximum = max(c(Graph, Graph2)))
dev.off()

sim_data1 <- as.data.frame(IsingSampler(5000, Graph, Thresh))
sim_data1$pop <- "severe"


sim_data2 <- as.data.frame(IsingSampler(5000, Graph2, Thresh2 ))
sim_data2$pop <- "healthy"

par(mfrow = c(1,1))

pdf("multiple_pop.pdf")
hist( rowSums(sim_data1[1:9]), col=rgb(1,0,0,0.5), xlim=c(0,10), breaks = 0:9, xlab = "Sum score", main = "")  
hist( rowSums(sim_data2[1:9]), col=rgb(0,0,1,0.5), xlim=c(0,10), add = T, breaks = 0:9)
dev.off()

combined_data <- rbind(sim_data1, sim_data2)
sum_score_data <- combined_data[rowSums(combined_data[1:9]) >= 5,]
sum(sum_score_data$pop %in% "severe")/nrow(sum_score_data)

est_IsingFit <- IsingFit(sum_score_data[1:9], plot = FALSE, progressbar = FALSE)$weiadj
est_correction <- IsingFit_sumscore_corr(sum_score_data[1:9], plot = FALSE, 
                                         progressbar = FALSE, min_sumscore = 5)$weiadj
qgraph(est_correction)
qgraph(est_IsingFit)

calculate_estimation_error(est_IsingFit, Graph)
calculate_estimation_error(est_correction, Graph)





