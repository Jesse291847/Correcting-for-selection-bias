library(IsingSampler)
library(IsingFit) 
library(psychonetrics) 
library(dplyr)
library(qgraph)


#number of symptoms 
N <- 9

# load the data
NCS <- read.csv("Empirical_data/NCS.csv")

# select the data for just depression symptoms with variance
MDE <- NCS[, paste0("V", 2:9)] 

# define a cutoff score
cutoff <- 4

# filter the cases that do not reach the specified cutoff score
severe <- MDE[rowSums(MDE) >= cutoff,]


# estimate as usual with different packages

# IsingSampler
Network1 <- EstimateIsingUni(severe, thresholding = TRUE)$graph 
# IsingFit
Network2 <- IsingFit(severe, plot = FALSE)$weiadj #IsingFit-package
#psychonetrics
mod1 <-  Ising(severe)
Network3 <- mod1 %>% prune(alpha = 0.01) %>% getmatrix("omega")


# IsingSampler
Network4 <- EstimateIsingUni(severe, thresholding = TRUE, min_sum = 4)$graph 
# IsingFit
Network5 <- IsingFit(severe, plot = FALSE, min_sum = 4)$weiadj #IsingFit-package
#psychonetrics
mod2 <-  Ising(severe, min_sum = 4)
Network6 <- mod2 %>% prune(alpha = 0.01) %>% getmatrix("omega")


AL <- averageLayout(list(Network1, Network2, Network3, Network4, Network5, Network6))

labels <- c("depr", "inte", "weig", "sle", "motor", "fat", "repr", "con", 
            "suic", "anxi", "even", "ctrl", "edge", "gFat", "irri", "gCon", 
            "musc", "gSle")

maximum <- max(c(Network1, Network2, Network3, Network4, Network5, Network6))


pdf("figures/fig4_empirical_example.pdf", width = 10, height = 5)
par(mfrow = c(1,2))
qgraph(Network1 , theme = "colorblind", labels = labels[2:9], layout = AL, 
       title = "Uncorrected IsingSampler", maximum = maximum)
qgraph(Network4, theme = "colorblind", labels = labels[2:9], layout = AL, 
       title = "Corrected IsingSampler", maximum = maximum)


dev.off()

pdf("figures/figS3_empirical_example.pdf", width = 10, height = 5)
par(mfrow = c(1,2))
qgraph(Network2, theme = "colorblind", labels = labels[2:9], layout = AL, 
       title = "Uncorrected IsingFit", maximum = maximum)
qgraph(Network5 , theme = "colorblind", labels = labels[2:9], layout = AL, 
       title = "Corrected IsingFit", maximum = maximum)
dev.off()

pdf("figures/figS4_empirical_example.pdf", width = 10, height = 5)
par(mfrow = c(1,2))
qgraph(Network3, theme = "colorblind", labels = labels[2:9], layout = AL, 
       title = "Uncorrected psychonetrics", maximum = maximum)
qgraph(Network6, theme = "colorblind", labels = labels[2:9],layout = AL, 
       title = "Corrected pyschonetrics", maximum = maximum)
dev.off()
                                                       
calculate_strength_dens <- function(weights) {                                                     
all_edges <- weights[upper.tri(weights)]
non_zero <- all_edges[all_edges != 0]

average_strength <- mean(abs(all_edges))
density <- length(non_zero) / length(all_edges)
return(round(c(average_strength, density),3))
}

calculate_strength_dens(Network1)
calculate_strength_dens(Network2)
calculate_strength_dens(Network3)
calculate_strength_dens(Network4)
calculate_strength_dens(Network5)
calculate_strength_dens(Network6)




