qc_test_dist_release <- function(data, qc_result) {
  if("transmitter_deployment_longitude" %in% colnames(data) &&
     "transmitter_deployment_latitude" %in% colnames(data)) {
    dist_r <- distGeo(cbind(data$transmitter_deployment_longitude[rep(1, nrow(data))],
                            data$transmitter_deployment_latitude[rep(1, nrow(data))]),
                      cbind(data$longitude, data$latitude)) / 1000 ## return in km
    qc_result[, "DistanceRelease_QC"] <- ifelse(dist_r > 500, 2, 1)
  } else {
    message("No transmitter lat/long in dataframe, skipping distance-from-release check.")
  }
  return(qc_result)
}