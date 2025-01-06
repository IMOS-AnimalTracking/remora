##' \pkg{remora}
##'
##' Rapid Extraction of Marine Observations for Roving Animals
##'
##' @name remora-package
##' @aliases remora remora-package
##' "_PACKAGE"
##' @author Ross Dwyer, Xavier Hoenner, Charlie Huveneers, Fabrice Jaine, Ian Jonsen, Francisca Maron, Kylie Scales, Vinay Udyawer
##'
##' @seealso runQC extractEnv extractMoor shinyReport
##' @references Hoenner, X et al. (2018) Australia’s continental-scale acoustic
##' tracking database and its automated quality control process. Scientific Data
##' 5, 170206. https://doi.org/10.1038/sdata.2017.206
##'
##' @keywords remora
##' @importFrom utils data flush.console globalVariables
##' @importFrom stats approx median time
NULL

##' @name TownsvilleReefQC
##' @docType data
##' @title Quality-controlled bull shark detections (5 individuals, 
##' subsampled for efficiency)
##' @format .RData
##' @keywords data
##' @description Example bull shark acoustic tracking data. Data were sourced from
##' the Integrated Marine Observing System (IMOS) - IMOS is supported by the
##' Australian Government through the National Collaborative Research Infrastructure
##' Strategy and the Super Science Initiative.
NULL

##' @name data_with_sst_and_current
##' @docType data
##' @title ??
##' @format .RData
##' @keywords data
##' @description ??
NULL

##' @name imos_variables_table
##' @docType data
##' @title ??
##' @format .RData
##' @keywords data
##' @description ??
NULL

## stop R CMD check generating NOTES about global variables
globalVariables(c(".", "ID", "Buffer", "start_dt", "end_dt", "year", "html", "type", "a", "fromdate", "todate", "median", 
                  "var_fill", "layer", "par_function", "receiver_name", "receiver_deployment_longitude",
                  "receiver_deployment_latitude", "receiver_deployment_datetime",
                  "installation_name","station_name","transmitter_id","species_common_name",
                  "transmitter_deployment_longitude", "transmitter_deployment_latitude",
                  "transmitter_deployment_datetime_new","transmitter_deployment_datetime",
                  "rs_vcur", "rs_ucur", "moor_depth", "detection_id", "depth_diff_m",
                  "index", "moor_sea_temp", "moor_ucur", "moor_vcur", "moor_psal",
                  "detection_id1", "moor_site_code", "moor_coverage_start", "moor_coverage_end",
                  "is.coverage", "transmitter_deployment_id", "receiver_deployment_id",
                  "tag_id", "tagging_project_name", "species_scientific_name",
                  "animal_sex", "detection_datetime", "receiver_deployment_longitude.x",
                  "receiver_deployment_latitude.x", "installation_name.x",
                  "station_name.x", "receiver_name.x", "receiver_id",
                  "depth_below_surface", "purchasing_organisation", "receiver_status",
                  "receiver_recovery_datetime", "receiver_recovery_longitude", 
                  "receiver_recovery_latitude", "receiver_name.y", "receiver_project_name.y",
                  "installation_name.y", "station_name.y", "receiver_deployment_longitude.y",
                  "receiver_deployment_latitude.y", "transmitter_serial_number.y", 
                  "tagging_project_name.y", "transmitter_type.y", "transmitter_sensor_type.y",
                  "transmitter_sensor_slope.y", "transmitter_sensor_intercept.y",
                  "transmitter_sensor_unit.y", "transmitter_estimated_battery_life.y",
                  "transmitter_status.y", "species_common_name.y", "species_scientific_name.y",
                  "animal_sex.y", "transmitter_deployment_longitude.x", "transmitter_deployment_longitude.y",
                  "transmitter_deployment_latitude.x", "transmitter_deployment_latitude.y",
                  "transmitter_deployment_datetime.x", "transmitter_deployment_datetime.y",
                  "tagging_project_name.x", "species_common_name.x", "species_scientific_name.x",
                  "animal_sex.x", "transmitter_sensor_type.x", "transmitter_sensor_unit.x",
                  "transmitter_sensor_slope.x", "transmitter_sensor_intercept.x", 
                  "transmitter_type.x", "transmitter_serial_number.x", 
                  "transmitter_estimated_battery_life.x", "transmitter_status.x",
                  "measurement_type", "measurement_value", "measurement_unit",
                  "s", "QC", "FDA_QC", "Velocity_QC", "Distance_QC", "DetectionDistribution_QC",
                  "DistanceRelease_QC", "ReleaseDate_QC", "ReleaseLocation_QC",
                  "Detection_QC", "transmitter_serial_number", "placement",
                  "transmitter_deployment_locality", "transmitter_deployment_comments",
                  "embargo_date", "embargo_date.x", "embargo_date.y", 
                  "transmitter_recovery_datetime", "transmitter_recovery_longitude",
                  "transmitter_recovery_latitude", "receiver_depth", "filename", "Variable",
                  "moor_timestamp", "time", "moor_depth", "breakthres", "i.moor_site_code",
                  "site_code", "mooring.date", "moor_site_code", "depth_bin",
                  "moor_sea_temp", "CAAB_species_id", "caab_dump", "CAAB_id",
                  "latitude", "longitude", "n.tags", "n.species", "detections_date_range",
                  "date_range", "Count", "receiver_project_name.x", "transmitter_sensor_value",
                  "transmitter_type", "transmitter_sensor_type", "transmitter_sensor_slope",
                  "transmitter_sensor_unit", "transmitter_estimated_battery_life",
                  "transmitter_status", "receiver_project_name", "measurement", 
                  "detection_date", "transmitter_sensor_intercept",
                  "imos_variables_table", "transmitter_sensor_raw_value", "tag_deployment_project_name",
                  "tag_deployment_project_name.x","tag_deployment_project_name.y", "valid"))