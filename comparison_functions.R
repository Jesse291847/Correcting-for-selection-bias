calculate_correlation <- function(est_network, true_network) {
  
  # Edges of both networks:
  true_edges <- true_network[upper.tri(true_network)]    
  est_edges <- est_network[upper.tri(est_network)]  
  
  # Calculate correlation:
  correlation <- cor(true_edges, est_edges)
  return(correlation)
}

calculate_sensitivity <- function(est_network, true_network) {
  
  true_edges <- true_network[upper.tri(true_network)]    
  est_edges <- est_network[upper.tri(est_network)] 
  # True positives cutof:    
  true_pos <- sum(true_edges != 0 & est_edges != 0)   
  
  # False Negatives cutoff:    
  false_neg <- sum(true_edges != 0 & est_edges == 0)
  
  # Calculate Sensitivity: 
  return((true_pos) / (true_pos + false_neg))
}

calculate_specificity <- function(est_network, true_network) {
  
  true_edges <- true_network[upper.tri(true_network)]    
  est_edges <- est_network[upper.tri(est_network)] 
  # False positives cutoff:
  false_pos <- sum(true_edges == 0 & est_edges !=  0) 
  
  # True Negatives cutoff:    
  true_neg <- sum(true_edges == 0 & est_edges == 0)  
  
  # Calculate Specificity: 
  return((true_neg) / (true_neg + false_pos))
}


calculate_spur_neg_edges <- function(est_network, true_network) {
  
  # Edges 
  true_edges <- true_network[upper.tri(true_network)]
  est_edges <- est_network[upper.tri(est_network)]
  
  # number of spurious negative edges 
  spurious_edges <- sum(est_edges < 0 & true_edges >= 0)
  
  # number of possible edges
  all_possible_edges <- sum(complete.cases(true_edges))
  
  # return number of spurious negative edges relative to the number of possible edges
  return(spurious_edges / all_possible_edges)
  
}

calculate_estimation_error <- function(est_network, true_network){
  # Edges:
  true_edges <- true_network[upper.tri(true_network)]
  est_edges <- est_network[upper.tri(est_network)]
  
  # Difference
  diff <- abs(true_edges - est_edges)
  
  #Estimation error
  estimation_error <- sum(diff)
  
  #Estimation error
  return(estimation_error)
  
}

calculate_metrics <- function(est_network, true_network) {
  estimation_error <- calculate_estimation_error(est_network, true_network)
  sensitivity <- calculate_sensitivity(est_network, true_network)
  specificity <- calculate_specificity(est_network, true_network)
  spur_neg_egdes <- calculate_spur_neg_edges(est_network, true_network)
  
  return(list(estimation_error = estimation_error, sensitivity = sensitivity, specificity = specificity, spur_neg_edges = spur_neg_egdes))
}
