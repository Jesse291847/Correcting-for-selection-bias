library(tidyverse)
library(reshape2)
library(ggpubr)

results <- readRDS("results_100_runs.RDS")
#check if errors occurred 
if(sum(results$error) == 0){
  results <- results %>% select(-c(error, errorMessage))
  
} else {
  print("Error(s) occurred in the simulation, better see what happened")
}

#reshaping results to plot
results <- melt(results, id = c("rep", "id", "cutoff", "sample_size"))
results$condition <- ifelse(grepl("regular", results$variable), "Regular", "Correction")
results$outcome <- ifelse(grepl("Sensitivity", results$variable), "Sensitivity",
                          ifelse(grepl("Neg", results$variable), "Spurious negative edges",
                                 ifelse(grepl("error", results$variable), "Estimation Error",
                                        ifelse(grepl("Specificity", results$variable), "Specificity",
                                               ifelse(grepl("Cor", results$variable), "Correlation", "Unknown")))))

#getting plot without estimation error
p1 <- results %>% filter(cutoff == 5 & outcome != "Estimation Error") %>%
  ggplot(mapping = aes(x = factor(sample_size), y = value, fill = condition)) +
  geom_boxplot() +
  facet_wrap(~outcome) +
  theme_bw() +
  scale_fill_discrete(name = "Condition", labels = c("Corrected", "Uncorrected")) +
  theme(legend.position = "bottom") +
  labs(x = "Sample size", y = "")

#getting plot with estimation error
p2 <- results %>% filter(cutoff == 5 & outcome == "Estimation Error") %>%
  ggplot(mapping = aes(x = factor(sample_size), y = value, fill = condition)) +
  geom_boxplot() +
  facet_wrap(~outcome) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "", y = "") +
  ylim(c(0,max(results$value))) 
  
#saving plots to pdf
pdf("plot_results_com.pdf")
ggarrange(p2, p1, nrow = 2, ncol = 1, heights = c(0.5, 1))
dev.off()



