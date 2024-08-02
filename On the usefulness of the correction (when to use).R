library(qgraph)
library(IsingSampler)
library(IsingFit)
source("comparison_functions.R")

#more vulnerable graph
Graph <- matrix(sample(0:1,9^2,TRUE,prob = c(0.3, 0.7)),9,9) * rnorm(9^1.5, 0.5, 0.1)
Graph[lower.tri(Graph)] = t(Graph)[lower.tri(Graph)]
diag(Graph) <- 0
Thresh <- rnorm((9)^2, -1, 0.1)
colnames(Graph) <- rownames(Graph) <- paste0("V", 1:9)

#'healthy' graph
Graph2 <- matrix(sample(0:1,9^2,TRUE,prob = c(0.5, 0.5)),9,9) * rnorm(9^2, 0.2, 0.2)
Graph2[lower.tri(Graph2)] = t(Graph2)[lower.tri(Graph2)]
diag(Graph2) <- 0
Thresh2 <- rnorm((9)^2, -3, 0.5)


par(mfrow = c(1,2))
qgraph(Graph, theme ="colorblind", labels = paste0(rep("S", 9), 1:9), maximum = max(c(Graph, Graph2)))
qgraph(Graph2, theme = "colorblind", labels = paste0(rep("S", 9), 1:9), maximum = max(c(Graph, Graph2)))


sim_data1 <- as.data.frame(IsingSampler(50000, Graph, Thresh))
sim_data1$pop <- "severe"


sim_data2 <- as.data.frame(IsingSampler(50000, Graph2, Thresh2 ))
sim_data2$pop <- "healthy"
dev.off()
hist( rowSums(sim_data1[1:9]), col=rgb(1,0,0,0.5), xlim=c(0,10), breaks = 0:9, xlab = "Sum score", main = "")  
hist( rowSums(sim_data2[1:9]), col=rgb(0,0,1,0.5), xlim=c(0,10), add = T, breaks = 0:9)


combined_data <- rbind(sim_data1, sim_data2)
sum_score_data <- combined_data[rowSums(combined_data[1:9]) >= 4,]
sum(sum_score_data$pop %in% "severe")/nrow(sum_score_data)

est_IsingFit <- IsingFit(sum_score_data[1:9], plot = FALSE, progressbar = FALSE)$weiadj
est_correction <- IsingFit(sum_score_data[1:9], plot = FALSE, 
                           progressbar = FALSE, min_sum = 4)$weiadj


est_full_sample <- IsingFit(combined_data[1:9], plot = FALSE, progressbar = FALSE)$weiadj

max <- max(est_full_sample, Graph, est_correction, est_IsingFit)
AV <- averageLayout(est_full_sample, Graph, est_correction, est_IsingFit)

pdf("Illustration_introduction.pdf", width = 10, height = 10)
par(mfrow = c(2,2))
qgraph(Graph, maximum = max, layout = AV, title = "Target: Network in the severe population", theme = "colorblind")
qgraph(est_full_sample, maximum = max, layout = AV, title = "Network full sample", theme = "colorblind")
qgraph(est_IsingFit, maximum = max, layout = AV, title = "Network on sum score sample without correction", theme = "colorblind")
qgraph(est_correction, maximum = max, layout = AV, title = "Network on sum score sample with correction", theme = "colorblind")
dev.off()



# calculate_estimation_error(est_IsingFit, Graph)
# calculate_estimation_error(est_correction, Graph) 
# calculate_estimation_error(est_full_sample, Graph)
# qgraph(Graph2, maximum = max, layout = AV)
# 
# 
# # par(mfrow = c(1,2))
# # qgraph(est_full_sample, maximum = max, layout = AV, title = "Network full sample")
# # qgraph(Graph2+Graph, maximum = max, layout = AV)
# # est_full_sample
# # Graph2+Graph
# 
# y <- numeric()
# z <- numeric()
# for(i in 1:9) {
# y[i] <- sum(Graph[,i] - est_IsingFit[,i])
# z[i] <- sum(Graph[,i] - est_correction[,i])
# }
# 
# cor(y, colMeans(sum_score_data[1:9])) 
# 
# cor(z, colMeans(sum_score_data[1:9])) 
# 
# 
# sum(Graph - est_correction)/2
# 
# 
# cor(y, abs(colMeans(sim_data1[1:9]) - colMeans(sum_score_data[1:9])))
# cor(z,  abs(colMeans(sim_data1[1:9]) - colMeans(sum_score_data[1:9])))
# 
# 
# 459*1.1
# 
# mgmsampler
# 
# library(mgm)
