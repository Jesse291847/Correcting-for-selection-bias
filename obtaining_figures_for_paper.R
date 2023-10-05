library(qgraph)
library(ggplot2)
library(tidyverse)
library(IsingSampler)
library(IsingFit)
library(glmnet)



#create fig1
set.seed(4)
severe <- sample(0:9, 1000, replace = TRUE, prob = c(0.1, 0.7, 1.2, 1.8, 2, 3.7, 4.5, 5.1, 4.4, 3.5))
healthy <- sample(0:4, 1000, replace = TRUE, prob = c(5, 2.7, 2, 1.5, 0.8))
pdf("figures/fig_1_illustration_selection_bias.pdf")
par(las = 1)
hist(severe, col=rgb(1,0,0,0.5), xlim=c(0,9), ylim = c(0, 400),  breaks = 0:9, xlab = "Sum score", main = "")  
hist(healthy, col=rgb(0,0,1,0.5), xlim=c(0,9), add = T, breaks = 0:5, right = FALSE)
abline(v = 5, lty = "dashed", lwd = 2.5)
axis(side=1,at=seq(0,9,1),labels=seq(0,9,1))

legend(list(x = 4.8, y = 415), c('Severe population'), text.col = rgb(1,0,0,0.5),
        box.col = "white",bg = "white", cex = 1.5, bty = "n")

legend(list(x = 0.5, y = 415), c('Healthy population'), text.col = rgb(0,0,1,0.5),
       box.col = "white",bg = "white", cex = 1.5, bty = "n")
dev.off()



# create Figure 2
#plot network figure 2
network <- readRDS("objects/network_sim.RDS")
colnames(network) <- paste0("S", 1:9)
pdf("figures/fig_2_true_network.pdf", width = 5, height = 5)
qgraph(network, theme = "colorblind", layout = "spring")
dev.off()


# create Figure 3
results <- readRDS("objects/results_100_runs.RDS")

#check if errors occurred 
if(sum(results$error) == 0){
  results <- results %>% select(-c(error, errorMessage))
  
} else {
  print("Error(s) occurred in the simulation, better see what happened")
}

results <- melt(results, id = c("rep", "id", "cutoff", "sample_size"))
results$outcome <- ifelse(grepl("sensitivity", results$variable), "Sensitivity",
                          ifelse(grepl("neg", results$variable), "Spurious negative edges",
                                 ifelse(grepl("error", results$variable), "Estimation Error",
                                        ifelse(grepl("specificity", results$variable), "Specificity",
                                               ifelse(grepl("cor", results$variable), "Correlation", "Unknown")))))

#getting plot with estimation error
summary <- results %>% group_by(variable, outcome, sample_size) %>% summarize(mean = mean(value), sd = sd(value)) %>% ungroup() 
summary$model_selection <- ifelse(grepl("unr", summary$variable), "no", "yes")
#summary <- summary %>% filter(!(model_selection == "no" & outcome %in% c("Sensitivity", "Specificity", "Spurious negative edges")))
summary <- summary %>% filter(model_selection == "yes")

summary$variable <- sub("\\..*", "", summary$variable)
summary$Correction <- ifelse(grepl("cor", summary$variable), "Yes", "No")
summary$package <- ifelse(grepl("IF", summary$variable), "IsingFit", ifelse(grepl("IS", summary$variable), 
                                                                            "IsingSampler", "Psychonetrics"))
#to get different ylims in facet_wrap()
dummy <- data.frame(outcome = c("Estimation Error", "Estimation Error", "Sensitivity", "Sensitivity", "Specificity", 
                                "Specificity", "Spurious negative edges", "Spurious negative edges"), x = 0, y = c(0, 20, 0, 1, 0, 1, 0,1))

#plotting

plot <- summary %>% 
  ggplot(mapping = aes(x = factor(sample_size), y = mean)) +
  geom_line(aes(group = variable, color = package, linetype = Correction), linewidth = 1, alpha = 0.8) +
  
  geom_point(aes(shape = Correction, color = package), size = 3) +
  geom_blank(data = dummy, aes(x = x, y = y)) +
  facet_wrap(~outcome, scales = "free_y") + 
  
  labs(color = "Estimator", x = "Sample size", y = "") + 
  scale_color_discrete(labels = c("IsingFit", "IsingSampler", "Psychonetrics")) +
  theme_bw() 

pdf("figures/fig_3_results_plot.pdf")
plot
dev.off()


#create_figure_4: see tutorial file











