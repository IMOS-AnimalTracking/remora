qc_test_distance <- function(data, qc_results, dist) {
  if (length(dist) == 1) {
    qc_results["Distance_QC"] <- ifelse(dist <= 1000, 1, 2)
  } 
  else if (length(dist) > 1) {
    dist_next <- c(dist[2:nrow(dist)], NA)
    
    ## Distance test
    qc_results[, "Distance_QC"] <- ifelse(dist > 1000 & dist_next > 1000, 2, 1)
    qc_results[1, "Distance_QC"] <- ifelse(dist[1] > 1000, 2, 1)
    qc_results[nrow(data), "Distance_QC"] <- ifelse(dist[nrow(data)] > 1000, 2, 1)
  }
  
  return(qc_results)
}