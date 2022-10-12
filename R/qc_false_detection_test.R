#False detection algorithm function. 

qc_false_detection_test <- function(data, temporal_outcome) {
  sta_rec <- unique(data$installation_name)
  sta_rec <- sta_rec[order(sta_rec)]
  
  for (j in 1:length(sta_rec)){
    sel <- which(data$installation_name == sta_rec[j])
    sub <- data[sel, ]
    
    ## Calculate time differences between detections (in minutes)
    time_diff <- as.numeric(difftime(sub$detection_datetime[2:nrow(sub)],
                                     sub$detection_datetime[1:(nrow(sub)-1)],
                                     tz = "UTC", units = "mins"))
    temporal_outcome[sel, 1] <-
      ifelse(sum(time_diff <= 30) > sum(time_diff >= 720) & nrow(sub) > 1, 1, 2)
  }
}