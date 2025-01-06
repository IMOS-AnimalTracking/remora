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
##' @param flag specifies which quality controlled detections to return 
##' (see examples): any combination of: `valid`, `likely valid`, 
##' `likely invalid`, `invalid`, or `all`. The default is to return all 
##' detections. Ignored if `what` is any of `tag_meta`, `rec_meta` or `meas`.
##' @return a data frame with the requested subset of the QC output
##'
##' @examples
##' ## grab detections and QCflags from example QC output & return only the
##' ## `valid` and `likely valid` detections
##' data(TownsvilleReefQC)
##' d.qc <- grabQC(TownsvilleReefQC, what = "dQC", flag = c("valid", "likely valid"))
##' 
##' ## return all detections
##' d.qc <- grabQC(TownsvilleReefQC, what = "dQC")
##'
##' @importFrom dplyr %>% select ungroup distinct nest_by
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
                    "meas"),
           flag = "all") {
    what <- match.arg(what)
    flag <- match.arg(
      flag,
      choices = c("valid",
                  "likely valid",
                  "likely invalid",
                  "invalid",
                  "all"),
      several.ok = TRUE
    )

    if (!inherits(x, "remora_QC"))
      stop("\033[31;1mx must be a `remora_QC` output object\033[0m")
    
    if (!what %in% c("detections", "QCflags", "dQC", "tag_meta", "rec_meta", "meas"))
      stop(
        "\033[31;1monly `detections`, `QCflags`, `dQC`, `tag_meta`, `rec_meta`, or `meas` can be grabbed from a remora_QC object\033[0m"
      )
    if (any(!flag %in% c("valid",
                     "likely valid",
                     "likely invalid",
                     "invalid",
                     "all")))
      stop(
        "\033[31;1monly the flags: `valid`, `likely valid`, `likely invalid`, `valid`, or `all` can be returned from a remora_QC object\033[0m"
      )
    
    ## if using old `transmitter_sensor_value` then change name
    xx <- unnest(x, cols = c(QC)) %>% ungroup()
    if(!"transmitter_sensor_raw_value" %in% names(xx)) {
      names(xx)[names(xx) == "transmitter_sensor_value"] <- "transmitter_sensor_raw_value"
      x <- nest_by(xx, filename, .key = "QC")
    }

    fl <- which(c("valid",
                  "likely valid",
                  "likely invalid",
                  "invalid",
                  "all") %in% flag)
    
    if(all(length(fl) == 1, fl == 5)) {
        fl <- 1:4 
      } else if(length(fl) > 1 & 5 %in% fl) {
        # avoids error if "all" included with other options
        fl <- 1:4
      }

    ## account for late 2024 AODN variable name changes for "tagging_project_name"
    if("tagging_project_name" %in% names(unnest(x, cols = QC))) {
      tag_id_vars <- c("transmitter_id","tag_id","transmitter_deployment_id","tagging_project_name")
      tag_meta_vars <- c("transmitter_id","transmitter_serial_number","tagging_project_name")
    } else {
      tag_id_vars <- c("transmitter_id","tag_id","transmitter_deployment_id","tag_deployment_project_name") 
      tag_meta_vars <- c("transmitter_id","transmitter_serial_number","tag_deployment_project_name")
    }
    
  out <- switch(what,
         detections = {
           suppressMessages(unnest(x, cols = QC) %>%
             select(
               tag_id_vars,
               species_common_name,
               species_scientific_name,
               detection_datetime,
               receiver_deployment_longitude,
               receiver_deployment_latitude,
               transmitter_sensor_raw_value,
               installation_name,
               station_name,
               receiver_name,
               receiver_deployment_id,
               Detection_QC
             ) %>% 
               filter(Detection_QC %in% fl) %>% 
               select(-Detection_QC))
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
             ) %>%
               filter(Detection_QC %in% fl))
         },
         dQC = {
           suppressMessages(unnest(x, cols = QC) %>%
             select(
               tag_id_vars,
               species_common_name,
               species_scientific_name,
               detection_datetime,
               receiver_deployment_longitude,
               receiver_deployment_latitude,
               transmitter_sensor_raw_value,
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
            ) %>%
               filter(Detection_QC %in% fl))
         },
         tag_meta = {
           try(suppressMessages(unnest(x, cols = QC) %>%
             select(
               tag_meta_vars,
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
               distinct()
               ), silent = TRUE)
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
               distinct()
               ), silent = TRUE)
         },
         meas = {
           try(suppressMessages(unnest(x, cols = QC) %>%
            select(
              transmitter_id,
              tag_id,
              transmitter_deployment_id,
              measurement
            ) %>% 
              distinct()
            ), silent = TRUE)
         })
  
  if(inherits(out, "try-error")) {
    if(what == "tag_meta")
      stop("transmitter metadata can not be returned as they were not supplied for the QC")
    else if(what == "rec_meta")
      stop("receiver metadata can not be returned as they were not supplied for the QC")
    else if(what == "meas")
      stop("animal measurements can not be returned as they were not supplied for the QC")
  } else {
    out <- out %>% ungroup() %>% select(-filename)
  }

  if(nrow(out) == 0) message(paste0("no quality-controlled detections with the flag(s): ", flag, "\n"))
  return(out)
}
