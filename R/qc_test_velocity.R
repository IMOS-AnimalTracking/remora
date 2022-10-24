qc_test_velocity <- function(data, qc_results, dist) {
  
  if (length(dist) == 1) {
    if("Velocity_QC" %in% colnames(qc_results)) {
      timediff <- as.numeric(
        difftime(
          data$transmitter_deployment_datetime,
          data$detection_datetime,
          tz = "UTC",
          units = "secs"
        )
      )
      
      velocity <- (dist * 1000) / timediff
      qc_results["Velocity_QC"] <- ifelse(velocity <= 10, 1, 2)
    }
  } 
  
  else if (length(dist) > 1) {
    dist_next <- c(dist[2:nrow(dist)], NA)
    
    time <-
      c(data$transmitter_deployment_datetime[1],
        data$detection_datetime)
    
    timediff <-
      abs(as.numeric(difftime(
        time[1:(length(time) - 1)], time[2:length(time)],
        tz = "UTC", units = "secs"
      )))
    timediff_next <- c(timediff[2:length(timediff)], NA)
    
    ## Exception to overcome the fact that a same tag may be detected by
    ##  two neighbouring stations at the exact same time, thus creating
    ##  infinite velocity values
    timediff[which(timediff == 0)] <- 1
    timediff_next[which(timediff_next == 0)] <- 1
    velocity <- (dist * 1000) / timediff
    velocity_next <- (dist_next * 1000) / timediff_next
    
    ## Velocity test
    qc_results[, "Velocity_QC"] <- ifelse(velocity > 10 & velocity_next > 10, 2, 1)
    qc_results[1, "Velocity_QC"] <- ifelse(velocity[1] > 10, 2, 1)
    qc_results[nrow(data), "Velocity_QC"] <- ifelse(velocity[nrow(data)] > 10, 2, 1)
  }
  return(qc_results)
}