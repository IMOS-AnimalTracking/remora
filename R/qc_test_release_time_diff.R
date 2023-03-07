qc_test_release_time_diff <- function(data, qc_result) {
  if("transmitter_deployment_datetime" %in% colnames(data)) {
    message("transmitter_deployment_datetime column exists.")
    ## IDJ - dropped lubridate::as.difftime b/c it was yielding different time diff's (why???)
    release_timediff <- as.numeric(difftime(data$detection_datetime,
                                            data$transmitter_deployment_datetime,
                                            tz = "UTC",
                                            units = "mins"))
    message("release_timediff: ", release_timediff)
    ## -720 minutes (12 h) to take into account potential time zone differences
    qc_result[which(release_timediff >= (-720)), "ReleaseDate_QC"] <- 1
    qc_result[which(release_timediff < (-720)), "ReleaseDate_QC"] <- 2
  } else {
    message("No transmitter deployment time in dataframe, skipping release time diff check.")
  }
  return(qc_result)
}