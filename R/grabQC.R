##' @title grab subsets of the QC output
##'
##' @description \code{grabQC()} lets you obtain subsets of the QC output
##'
##' @param x a QC output object (a nested tibble with class `remora_QC`)
##' @param what defined subset of the QC output is to be grabbed; either
##' `detections`, `QCflags`, `dQC` (detections and QCflags),
##' `tag_meta` (transmitter deployment metadata),
##' `rec_meta` (receiver deployment metadata), or
##' `meas` (animal measurements)
##' @return a data frame with the requested subset of the QC output
##'
##' @examples
##' ## grab detections and QCflags from example QC output
##' data(TownsvilleReefQC)
##' d.qc <- grabQC(TownsvilleReefQC, what = "dQC")
##'
##' @importFrom dplyr '%>%' select ungroup distinct
##' @importFrom tidyr unnest
##'
##' @export
grabQC <-
  function(x,
           what = c("dQC",
                    "detections",
                    "QCflags",
                    "tag_meta",
                    "rec_meta",
                    "meas")) {
    what <- match.arg(what)

    if (!inherits(x, "remora_QC"))
      stop("x must be a `remora_QC` output object")
    
    if (!what %in% c("detections", "QCflags", "dQC", "tag_meta", "rec_meta", "meas"))
      stop(
        "only `detections`, `QCflags`, `dQC`, `tag_meta`, `rec_meta`, or `meas` can be grabbed from a remora_QC object"
      )

  out <- switch(what,
         detections = {
           suppressMessages(unnest(x, cols = QC) %>%
             select(
               transmitter_id,
               tag_id,
               transmitter_deployment_id,
               tagging_project_name,
               species_common_name,
               species_scientific_name,
               detection_datetime,
               receiver_deployment_longitude,
               receiver_deployment_latitude,
               transmitter_sensor_value,
               installation_name,
               station_name,
               receiver_name,
               receiver_deployment_id
             ))
         },
         QCflags = {
           suppressMessages(unnest(x, cols = QC) %>%
             select(
               transmitter_id,
               tag_id,
               transmitter_deployment_id,
               FDA_QC,
               Velocity_QC,
               Distance_QC,
               DetectionDistribution_QC,
               DistanceRelease_QC,
               ReleaseDate_QC,
               ReleaseLocation_QC,
               Detection_QC
             ))
         },
         dQC = {
           suppressMessages(unnest(x, cols = QC) %>%
             select(
               transmitter_id,
               tag_id,
               transmitter_deployment_id,
               tagging_project_name,
               species_common_name,
               species_scientific_name,
               detection_datetime,
               receiver_deployment_longitude,
               receiver_deployment_latitude,
               transmitter_sensor_value,
               installation_name,
               station_name,
               receiver_name,
               receiver_deployment_id,
               FDA_QC,
               Velocity_QC,
               Distance_QC,
               DetectionDistribution_QC,
               DistanceRelease_QC,
               ReleaseDate_QC,
               ReleaseLocation_QC,
               Detection_QC
            ))
         },
         tag_meta = {
           try(suppressMessages(unnest(x, cols = QC) %>%
             select(
               transmitter_id,
               transmitter_serial_number,
               tagging_project_name,
               transmitter_type,
               transmitter_sensor_type,
               transmitter_sensor_slope,
               transmitter_sensor_intercept,
               transmitter_sensor_unit,
               transmitter_estimated_battery_life,
               transmitter_status,
               transmitter_deployment_id,
               species_common_name,
               species_scientific_name,
               animal_sex,
               placement,
               transmitter_deployment_locality,
               transmitter_deployment_longitude,
               transmitter_deployment_latitude,
               transmitter_deployment_datetime,
               transmitter_deployment_comments,
               embargo_date,
               transmitter_recovery_datetime,
               transmitter_recovery_longitude,
               transmitter_recovery_latitude
             ) %>%
             distinct()), silent = TRUE)
         },
         rec_meta = {
           try(suppressMessages(unnest(x, cols = QC) %>%
             select(
               receiver_deployment_id,
               receiver_name,
               purchasing_organisation,
               receiver_project_name,
               receiver_status,
               receiver_deployment_datetime,
               installation_name,
               station_name,
               receiver_deployment_longitude,
               receiver_deployment_latitude,
               receiver_depth,
               receiver_recovery_datetime,
               receiver_recovery_longitude,
               receiver_recovery_latitude
             ) %>%
            rename(depth_below_surface = receiver_depth) %>%
              distinct()), silent = TRUE)
         },
         meas = {
           try(suppressMessages(unnest(x, cols = QC) %>%
            select(
              transmitter_id,
              tag_id,
              transmitter_deployment_id,
              measurement
            ) %>%
            distinct()), silent = TRUE)
         })

  if(inherits(out, "try-error")) {
    if(what == "tag_meta")
      stop("transmitter metadata can not be returned as they were not supplied for the QC")
    else if(what == "rec_meta")
      stop("receiver metadata can not be returned as they were not supplied for the QC")
    else if(what == "meas")
      stop("animal measurements can not be returned as they were not supplied for the QC")
  } else {
    out %>% ungroup() %>% select(-filename)
  }

}
