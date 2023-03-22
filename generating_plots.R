library(tidyverse)
library(reshape2)
library(ggpubr)

results <- readRDS("results_100_runs_multi.RDS")
#check if errors occurred 
if(sum(results$error) == 0){
  results <- results %>% select(-c(error, errorMessage))
  
} else {
  print("Error(s) occurred in the simulation, better see what happened")
}
results
#reshaping results to plot
results <- melt(results, id = c("rep", "id", "cutoff", "sample_size"))
results$condition <- ifelse(grepl("regular_univariate", results$variable), "regular_univariate",
                            ifelse(grepl("regular_elasso", results$variable), "regular_elasso" ,
                            ifelse(grepl("correction_univariate", results$variable), "correction_univariate",
                            ifelse(grepl("correction_elasso", results$variable), "correction_elasso",
                            ifelse(grepl("correction_multivariate", results$variable),"correction_multivariate", "Should not exist")))))
                            
results$outcome <- ifelse(grepl("Sensitivity", results$variable), "Sensitivity",
                          ifelse(grepl("Neg", results$variable), "Spurious negative edges",
                                 ifelse(grepl("error", results$variable), "Estimation Error",
                                        ifelse(grepl("Specificity", results$variable), "Specificity",
                                               ifelse(grepl("Cor", results$variable), "Correlation", "Unknown")))))
unique(results$outcome)
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
p2 <- results %>% filter(cutoff == 5 & outcome == "Estimation Error" & !grepl("elasso", condition)) %>%
  ggplot(mapping = aes(x = factor(sample_size), y = value, fill = condition)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_discrete(name = "Condition", labels = c("Corrected multivariate estimation", 
  "Corrected univariate estimation", "Regular univariate estimation")) +
  labs(x = "Sample size", y = "Estimation Error") +
  ylim(c(0,max(results$value))) 
p2  


grepl("elasso")

#saving plots to pdf
pdf("plot_results_com.pdf")
ggarrange(p2, p1, nrow = 2, ncol = 1, heights = c(0.5, 1))
dev.off()



