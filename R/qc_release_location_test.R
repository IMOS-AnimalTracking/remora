qc_release_location_test <- function(data, qc_result, species_range, distances, latlons) {
  message("Starting release location test.")
  ## Release location test
  if(!is.null(species_range)) {
    message("We have a shapefile")
    qc_result[, "ReleaseLocation_QC"] <- ifelse(distances[1] > 500 &
                                                         sum(is.na(sp::over(latlons, species_range))) > 0, 2, 1)
  } else {
    message("We have no shapefile.")
    qc_result[, "ReleaseLocation_QC"] <- ifelse(distances[1] > 500, 2, 1)
  }
  message("release location test done")
  return(qc_result)
}