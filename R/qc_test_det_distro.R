qc_test_det_distro <- function(data, latlons, qc_result, species_range) {
  qc_result[, "DetectionDistribution_QC"] <- ifelse(is.null(species_range), 3, 1)
  if(!is.null(species_range)) {
    out <- which(is.na(over(latlons, species_range)))
    if(length(out) > 0) {
      qc_result[data$longitude %in% latlons@coords[out, 1] &
                         data$latitude %in% latlons@coords[out, 2], "DetectionDistribution_QC"] <- 2
    }
  }
  return(qc_result)
}