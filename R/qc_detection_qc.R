qc_detection_qc <- function(qc_result) {
  ones <- as.numeric(rowSums(qc_result[, c(1:length(qc_result)-1)] == 1))
  
  qc_result[which(ones <= 2), "Detection_QC"] <- 4
  qc_result[which(ones == 3), "Detection_QC"] <- 3
  qc_result[which(ones == 4), "Detection_QC"] <- 2
  qc_result[which(ones == 5), "Detection_QC"] <- 1
  #qc_result$Velocity_QC <- as.numeric(qc_result$Velocity_QC)
  #qc_result$Distance_QC <- as.numeric(qc_result$Distance_QC)
  
  return(qc_result)
}