##' @title read acoustic detection data and metadata from IMOS WebApp-downloaded files
##'
##' @description accesses files downloaded from the IMOS ATF WebApp and convert them to
##' a format expected by the IMOS ATF Quality Control functions. For internal use only
##'
##' @param det path to detections text file
##' @param rmeta path to receiver metadata text file
##' @param tmeta path to tag deployment metadata text file
##' @param meas path to animal measurements text file
##' @param logfile path to logfile; default is the working directory
##'
##' @return a list of data.frames by individual tag deployments ready for QC
##'
##' @importFrom utils read.csv
##' @importFrom readr read_csv cols_only col_character col_integer col_guess
##' @importFrom readr col_logical col_datetime col_double
##' @importFrom dplyr %>% mutate left_join select group_by ungroup distinct
##' @importFrom dplyr transmute any_of everything
##' @importFrom lubridate dmy_hms dmy
##'
##' @keywords internal
##'

get_data <- function(det=NULL, rmeta=NULL, tmeta=NULL, meas=NULL, logfile) {

  rec_meta <- tag_meta <- anim_meas <- NULL

  ## detections
  if(is.null(det)) stop("\033[31;1mCan not run QC without a detections file!\033[0m\n")
 
  ## account for old & new tag project name variables: tagging_project_name (old);
  ##  tag_deployment_project_name (new)
  tpdet.log <- "tagging_project_name" %in% names(read.csv(det))
  
  if(tpdet.log) {
    det.cols <- cols(
      detection_datetime = "T",
      detection_corrected_datetime = "T",
      transmitter_id = "c",
      tag_id = "i",
      transmitter_deployment_id = "i",
      tagging_project_name = "c",
      species_common_name = "c",
      species_scientific_name = "c",
      CAAB_species_id = "i",
      WORMS_species_aphia_id = "i",
      animal_sex = "c",
      receiver_project_name = "c",
      installation_name = "c",
      station_name = "c",
      receiver_id = "i",
      receiver_name = "c",
      receiver_deployment_id = "i",
      transmitter_sensor_type = "c",
      transmitter_sensor_unit = "c",
      transmitter_type = "c",
      transmitter_serial_number = "?",
      transmitter_estimated_battery_life = "i",
      transmitter_status = "c",
      transmitter_deployment_datetime = "T",
      transmitter_dual_sensor = "l",
      .default = "d"
    )
  } else {
    det.cols <- cols(
      detection_datetime = "T",
      detection_corrected_datetime = "T",
      transmitter_id = "c",
      tag_id = "i",
      transmitter_deployment_id = "i",
      tag_device_project_name = "c",
      tag_deployment_project_name = "c",
      species_common_name = "c",
      species_scientific_name = "c",
      CAAB_species_id = "i",
      WORMS_species_aphia_id = "i",
      animal_sex = "c",
      receiver_project_name = "c",
      installation_name = "c",
      station_name = "c",
      receiver_id = "i",
      receiver_name = "c",
      receiver_deployment_id = "i",
      transmitter_sensor_type = "c",
      transmitter_sensor_unit = "c",
      transmitter_type = "c",
      transmitter_serial_number = "?",
      transmitter_estimated_battery_life = "i",
      transmitter_status = "c",
      transmitter_deployment_datetime = "T",
      transmitter_dual_sensor = "l",
      .default = "d"
    )
  }

  det_data <- suppressWarnings(read_csv(det,
                                          col_types = det.cols,
                                        na = c("","null","NA")
                                        ))
  
  ## drop any unnamed columns, up to a possible 20 of them...
  if(any(paste0("X",1:20) %in% names(det_data))) {
    drops <- paste0("X",1:20)[paste0("X",1:20) %in% names(det_data)]
    det_data <- det_data %>% select(-any_of(drops))
  }

  ## add embargo_date variable if not present so downstream code works
  if(!"embargo_date" %in% names(det_data)) {
    det_data <- det_data %>% 
      mutate(embargo_date = NA)
  }

  ## receiver deployment metadata - required for receiver depth
  if(!is.null(rmeta)) {
    rec_meta <- suppressMessages(suppressWarnings(read_csv(rmeta)))

    ## drop any unnamed columns, up to a possible 20 of them...
    if(any(paste0("X",1:20) %in% names(rec_meta))) {
      drops <- paste0("X",1:20)[paste0("X",1:20) %in% names(rec_meta)]
      rec_meta <- rec_meta %>% select(-any_of(drops))
    }
  }

  ## tag deployment metadata
  if(!is.null(tmeta)) {
    ## account for old & new tag project name variables: tagging_project_name (old);
    ##  tag_deployment_project_name (new)
    tpmeta.log <- "tagging_project_name" %in% names(read.csv(tmeta))
    
    if(tpmeta.log) {
      tmeta.cols <- cols(
        transmitter_id = "c",
        transmitter_serial_number = "i",
        tagging_project_name = "c",
        transmitter_type = "c",
        transmitter_sensor_type = "c",
        transmitter_sensor_slope = "c",
        transmitter_sensor_intercept = "c",
        transmitter_sensor_unit = "c",
        transmitter_estimated_battery_life = "i",
        transmitter_status = "c",
        transmitter_deployment_id = "i",
        species_common_name = "c",
        species_scientific_name = "c",
        animal_sex = "c",
        placement = "c",
        transmitter_deployment_locality = "c",
        transmitter_deployment_latitude = "d",
        transmitter_deployment_longitude = "d",
        transmitter_deployment_datetime = "T",
        transmitter_deployment_comments = "c",
        embargo_date = "T",
        transmitter_recovery_datetime = "T",
        transmitter_recovery_latitude = "d",
        transmitter_recovery_longitude = "d",
        .default = "d"
      )
      
    } else {
      tmeta.cols <- cols(
        transmitter_id = "c",
        transmitter_serial_number = "i",
        tag_device_project_name = "c",
        tag_deployment_project_name = "c",
        transmitter_type = "c",
        transmitter_sensor_type = "c",
        transmitter_sensor_slope = "c",
        transmitter_sensor_intercept = "c",
        transmitter_sensor_unit = "c",
        transmitter_estimated_battery_life = "i",
        transmitter_status = "c",
        transmitter_deployment_id = "i",
        species_common_name = "c",
        species_scientific_name = "c",
        animal_sex = "c",
        placement = "c",
        transmitter_deployment_locality = "c",
        transmitter_deployment_latitude = "d",
        transmitter_deployment_longitude = "d",
        transmitter_deployment_datetime = "T",
        transmitter_deployment_comments = "c",
        embargo_date = "T",
        transmitter_recovery_datetime = "T",
        transmitter_recovery_latitude = "d",
        transmitter_recovery_longitude = "d",
        .default = "d"
      )
    }
    
    tag_meta <- suppressWarnings(read_csv(tmeta,
                                          col_types = tmeta.cols,
                                          na = c("","null","NA")
                                          ))

    ## retain only the metadata for the current tagging_project_name/tag_deployment_project_name
    if(tpmeta.log) {
      tag_meta <- tag_meta |> 
        filter(tagging_project_name %in% unique(det_data$tagging_project_name))
    } else {
      tag_meta <- tag_meta |> 
        filter(tag_deployment_project_name %in% unique(det_data$tag_deployment_project_name))
    }
    

    ## drop any unnamed columns, up to a possible 20 of them...
    if(any(paste0("X",1:20) %in% names(tag_meta))) {
      drops <- paste0("X",1:20)[paste0("X",1:20) %in% names(tag_meta)]
      tag_meta <- tag_meta %>% select(-any_of(drops))
    }
    
    ## check for duplicate entries of same transmitter_id & retain only the most
    ##  recent deployment date with valid deployment lon, lat entries (not NA's)
    tmp <- split(tag_meta, tag_meta$transmitter_id)
    tmp <- lapply(tmp, function(x) {
      if (nrow(x) > 1) {
        xx <- x |> filter(
          !is.na(transmitter_deployment_longitude) |
            !is.na(transmitter_deployment_latitude)
        )
        if (nrow(xx) != 0) {
          xx <- x |>
            arrange(transmitter_deployment_datetime)
          xx <- xx[1, ]
        } 
        xx
      } else {
        x
      }
    }) |>
      bind_rows()
    
    ## check for NA's in deployment lat,lon entries & use mean reported value
    ##  for NA's at same transmitter_deployment_locality
    tag_meta <- tmp |> 
      group_by(transmitter_deployment_locality) |>
      mutate(transmitter_deployment_longitude = 
               ifelse(is.na(transmitter_deployment_longitude),
                      mean(transmitter_deployment_longitude, na.rm = TRUE),
                      transmitter_deployment_longitude),
             transmitter_deployment_latitude = 
               ifelse(is.na(transmitter_deployment_latitude),
                      mean(transmitter_deployment_latitude, na.rm = TRUE),
                      transmitter_deployment_latitude)) |>
      ungroup()
  }
  
  ## animal measurements data
  if(!is.null(meas)) {
    anim_meas <- suppressWarnings(read_csv(meas,
                                           col_types = cols(
                                             transmitter_id = col_character(),
                                             transmitter_deployment_id = col_integer(),
                                             measurement_type = col_character(),
                                             measurement_value = col_double(),
                                             measurement_unit = col_character(),
                                             comments = col_character()
                                           )))
    ## drop any unnamed columns, up to a possible 20 of them...
    if(any(paste0("X",1:20) %in% names(meas))) {
      drops <- paste0("X",1:20)[paste0("X",1:20) %in% names(meas)]
      meas <- meas %>% select(-any_of(drops))
    }

  }

  if(!is.null(tag_meta)) {
    ## check for & report any tags in detections but not in tag metadata
    missing_ids <-
      unique(det_data$transmitter_deployment_id)[!unique(det_data$transmitter_deployment_id) %in% tag_meta$transmitter_deployment_id]
    ## create filenames
    fns <-
      with(
        subset(det_data, transmitter_deployment_id %in% missing_ids),
        paste(
          unique(transmitter_id),
          unique(tag_id),
          unique(transmitter_deployment_id),
          sep = "_"
        )
      )
    ## how many missing_ids
    n <- length(missing_ids)
    if (n > 0) {
      ## write to logfile
      write(
        paste0(
          fns,
          ":  transmitter_deployment_id in detections data but not in transmitter metadata"
        ),
        file = logfile,
        append = TRUE
      )
    }
  } else {
    warning("transmitter metadata not supplied, skipping tests for missing metadata records",
            call. = FALSE, immediate. = TRUE)
  }

  if (!is.null(rec_meta)) {
    ## check for & report any receiver_deployment_id's in detections data but not in receiver metadata
    missing_ids <-
      unique(det_data$receiver_deployment_id)[!unique(det_data$receiver_deployment_id) %in% rec_meta$receiver_deployment_id]
    
    ## create filenames
    fns <-
      with(
        subset(det_data, receiver_deployment_id %in% missing_ids),
        paste(
          unique(installation_name),
          unique(station_name),
          unique(receiver_deployment_id),
          sep = "_"
        )
      )
    ## how many missing_ids
    n <- length(missing_ids)
    if (n > 0) {
      ## write to logfile
      write(
        paste0(
          fns,
          ":  receiver_deployment_id in detections data but not in receiver metadata"
        ),
        file = logfile,
        append = TRUE
      )
    }
  } else {
    warning(
      "receiver metadata not supplied, skipping tests for missing metadata records",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  if(!is.null(rec_meta)) {
  ## merge detections with receiver metadata - to get receiver_depth,
  ##    but merge everything & keep detections data version of common variables
    
    ## coerce old tag project name variable to new variable in detections data
    if("tagging_project_name" %in% names(det_data)) {
      id.r <- which(names(det_data) == "tagging_project_name")
      names(det_data)[id.r] <- "tag_deployment_project_name"
    }
    
  dd <-
    left_join(det_data, rec_meta, by = "receiver_deployment_id") %>%
    select(
      transmitter_id,
      tag_id,
      transmitter_deployment_id,
      tag_deployment_project_name,
      species_common_name,
      species_scientific_name,
      CAAB_species_id = ifelse(
        "CAAB_species_id" %in% names(det_data),
        "CAAB_species_id",
        "species_id"
      ),
      WORMS_species_aphia_id = ifelse(
        "WORMS_species_aphia_id" %in% names(det_data),
        "WORMS_species_aphia_id",
        "species_aphia_id"
      ),
      animal_sex,
      detection_datetime,
      longitude = receiver_deployment_longitude.x,
      latitude = receiver_deployment_latitude.x,
      receiver_project_name = receiver_project_name.x,
      installation_name = installation_name.x,
      station_name = station_name.x,
      receiver_name = receiver_name.x,
      receiver_id,
      receiver_deployment_id,
      receiver_depth = depth_below_surface,
      purchasing_organisation,
      receiver_status,
      receiver_deployment_datetime,
      receiver_recovery_datetime,
      receiver_recovery_longitude,
      receiver_recovery_latitude,
      everything()
    ) %>%
    select(
      -receiver_name.y,-receiver_project_name.y,-installation_name.y,-station_name.y,-receiver_deployment_longitude.y,-receiver_deployment_latitude.y
    )
  } else {
    dd <- det_data %>%
      select(
        transmitter_id,
        tag_id,
        transmitter_deployment_id,
        tag_deployment_project_name,
        species_common_name,
        species_scientific_name,
        CAAB_species_id = ifelse(
          "CAAB_species_id" %in% names(det_data),
          "CAAB_species_id",
          "species_id"
        ),
        WORMS_species_aphia_id = ifelse(
          "WORMS_species_aphia_id" %in% names(det_data),
          "WORMS_species_aphia_id",
          "species_aphia_id"
        ),
        animal_sex,
        detection_datetime,
        longitude = receiver_deployment_longitude,
        latitude = receiver_deployment_latitude,
        receiver_id,
        receiver_name,
        receiver_deployment_id,
        everything()
      )
  }

  if(!is.null(tag_meta)) {
    ## coerce old tag project name variable to new variable in tag metadata
    if("tagging_project_name" %in% names(tag_meta)) {
      id.t <- which(names(tag_meta) == "tagging_project_name")
      names(tag_meta)[id.t] <- "tag_deployment_project_name"
    }
    
    dd <- left_join(dd,
                    tag_meta,
                    by = c("transmitter_id", "transmitter_deployment_id")) |>
      select(
        -transmitter_serial_number.y,
        -tag_deployment_project_name.y,
        -transmitter_type.y,
        -transmitter_sensor_type.y,
        -transmitter_sensor_slope.y,
        -transmitter_sensor_intercept.y,
        -transmitter_sensor_unit.y,
        -transmitter_estimated_battery_life.y,
        -transmitter_status.y,
        -species_common_name.y,
        -species_scientific_name.y,
        -animal_sex.y,
        -embargo_date.x)
    
    if("tag_device_project_name.x" %in% names(dd)) {
      dd <- dd |> 
        rename(tag_device_project_name = tag_device_project_name.x) |>
        select(-tag_device_project_name.y)
    }
    
## deal with any cases where deploy lon/lat is missing in detections but not metadata
    if(any(is.na(dd$transmitter_deployment_longitude.x)) |
       any(is.na(dd$transmitter_deployment_latitude.x))) {

      dd <- dd %>%
        mutate(transmitter_deployment_longitude.x =
                 ifelse(is.na(transmitter_deployment_longitude.x),
                        transmitter_deployment_longitude.y,
                        transmitter_deployment_longitude.x)) %>%
        mutate(transmitter_deployment_latitude.x =
                 ifelse(is.na(transmitter_deployment_latitude.x),
                        transmitter_deployment_latitude.y,
                        transmitter_deployment_latitude.x)) %>%
        mutate(transmitter_deployment_datetime.x =
                 ifelse(is.na(transmitter_deployment_datetime.x),
                        transmitter_deployment_datetime.y,
                        transmitter_deployment_datetime.x))
    }
    dd <- dd %>%
      select(
        -transmitter_deployment_latitude.y,
        -transmitter_deployment_longitude.y,
        -transmitter_deployment_datetime.y
      ) %>%
      rename(
        tag_deployment_project_name = tag_deployment_project_name.x,
        species_common_name = species_common_name.x,
        species_scientific_name = species_scientific_name.x,
        animal_sex = animal_sex.x,
        transmitter_sensor_type = transmitter_sensor_type.x,
        transmitter_sensor_unit = transmitter_sensor_unit.x,
        transmitter_sensor_slope = transmitter_sensor_slope.x,
        transmitter_sensor_intercept = transmitter_sensor_intercept.x,
        transmitter_type = transmitter_type.x,
        transmitter_serial_number = transmitter_serial_number.x,
        transmitter_estimated_battery_life = transmitter_estimated_battery_life.x,
        transmitter_status = transmitter_status.x,
        transmitter_deployment_longitude = transmitter_deployment_longitude.x,
        transmitter_deployment_latitude = transmitter_deployment_latitude.x,
        transmitter_deployment_datetime = transmitter_deployment_datetime.x,
        embargo_date = embargo_date.y
      ) 
    
    if (!inherits(dd$transmitter_deployment_datetime, "POSIXt")) {
      if (inherits(dd$transmitter_deployment_datetime, "numeric")) {
        dd <- dd %>% mutate(
          transmitter_deployment_datetime =
            as.POSIXct(
              transmitter_deployment_datetime,
              origin = "1970-01-01",
              tz = "UTC"
            )
        )
      } 
      if (inherits(dd$transmitter_deployment_datetime, "character")) {
        dd <- dd %>% mutate(transmitter_deployment_datetime =
                              ymd_hms(transmitter_deployment_datetime,
                                      tz = "UTC"))
      }
    }
  }
  
  ## ensure old tagging_project_name variable is returned if exists in input data
  if(exists("id.r")) {
    id <- which(names(dd) == "tag_deployment_project_name")
    names(dd)[id] <- "tagging_project_name"
  }
  ## ensure old tagging_project_name variable is returned if exists in input tag_metadata
  if(exists("id.t")) {
    id <- which(names(dd) == "tag_deployment_project_name")
    names(dd)[id] <- "tagging_project_name"
  }
  
  
  if(!is.null(anim_meas)) {
    ## concatenate all measurements into single record by unique id
    anim_meas <- anim_meas %>%
      mutate(meas =
               paste(measurement_type, "=", measurement_value, measurement_unit)) %>%
      select(transmitter_deployment_id, meas) %>%
      group_by(transmitter_deployment_id) %>%
      dplyr::transmute(measurement = paste0(meas, collapse = "; ")) %>%
      distinct() %>%
      ungroup()

    dd <- dd %>%
      left_join(., anim_meas, by = "transmitter_deployment_id")

  }

  ## split by tag_id, transmitter_deployment_id and
  ##   output as a list of individual files - this combines dual_sensor tags
  ## order by detection_datetime within each data set
  dd.lst <- dd %>%
    mutate(filename = paste(tag_id,
                            transmitter_deployment_id,
                            sep = "_")) %>%
    split(., .$filename) %>%
    lapply(., function(x)
      x[order(x$detection_datetime),])

  ## check which are dual sensor tags & name mono & dual tag data sets appropriately
  dual_tags <-
    which(as.vector(sapply(dd.lst, function(x)
      length(
        unique(x$transmitter_id)
      ))) == 2)
  mono_tags <-
    which(as.vector(sapply(dd.lst, function(x)
      length(
        unique(x$transmitter_id)
      ))) == 1)
  names(dd.lst)[dual_tags] <- paste(sapply(dd.lst[dual_tags],
                                           function(x)
                                             paste(
                                               unique(x$transmitter_id)[1],
                                               unique(x$transmitter_id)[2],
                                               sep = "_")
                                           ),
                                    names(dd.lst[dual_tags]),
                                    sep = "_")
  names(dd.lst)[mono_tags] <-
    paste(sapply(dd.lst[mono_tags], function(x)
      unique(x$transmitter_id)), names(dd.lst[mono_tags]), sep = "_")

  ## replace filename var in each data set with list element name
  dd.lst <- lapply(1:length(dd.lst), function(i) {
    dd.lst[[i]]$filename <- names(dd.lst)[i]
    dd.lst[[i]]
  })

  return(dd.lst)
}
