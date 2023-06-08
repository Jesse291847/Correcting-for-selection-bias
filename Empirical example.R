library(glmnet)
library(qgraph)
library(IsingFit)
library(tidyverse)
source("estimators.R")
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
       title = "Uncorrected network", maximum = max(c(regular, correction)))
qgraph(correction, theme = "colorblind", labels = labels[2:9], layout = AL, 
       title = "Corrected network", max(c(regular, correction)))
dev.off()





