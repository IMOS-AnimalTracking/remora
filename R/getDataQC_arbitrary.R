get_data_arbitrary <- function(det=NULL, rmeta=NULL, tmeta=NULL, meas=NULL, logfile, checks = c("all"),
                               det_id_column = "transmitter_deployment_id", tag_id_column = "transmitter_deployment_id",
                               det_rcvr_column = "receiver_deployment_id", rcvr_id_column = "receiver_deployment_id", data_format = "imos") {
  library(tools)
  library(dplyr)
  
  #Remember to formalize all of the new variables as comments up above:
  # - checks: vector containing all of the specific chekcs you want to turn on or off while pulling in the data.
  # - det_id_column and tag_id_column- we need to know what columns to use for ID checks in the check for tags that are in detections but not in the tag
  #metadata. 
  # - data_format- the type of format and therefore the type of columns we'll need to use when we join all this together. 
  
  #Since we may only get detection data, we set the other data chunks to NULL by default.
  rec_meta <- tag_meta <- anim_meas <- NULL
  
  #Now we need to load the column names for whatever data format we're going to be loading. If you want to add a new one, save them as RData files in the
  #'data' folder, in the format <institution name>_detections/receiver/transmitter/animal_measurement_columns, choosing whichever of the four is appropriate for
  #'the type of file these columns will be referenced in.'
  
  #Leaving these here for now, might need to delete later. I think my current way of doing this obsolesces them.
  #det_columns = data(paste0(data_format, 'detections_columns'))
  #rec_columns = data(paste0(data_format, 'receiver_columns'))
  #tag_columns = data(paste0(data_format, 'transmitter_columns'))
  #meas_columns = data(paste0(data_format, 'animal_measurement_columns'))
  
  ## detections
  #Can't run without a detections file, makes sense.
  if(is.null(det)) stop("\033[31;1mCan not run QC without a detections file!\033[0m\n")
  
  #Original version has a huge col_types specification here, we're not going to do that since it relies on column names that will not be held over in non-
  #IMOS data formats.
  det_data <- read_csv(det, na = c("", "null", "NA"))
  
  ## drop any unnamed columns, up to a possible 20 of them...
  # BD - There must be a nicer and more sensible way to drop unnamed columns. Also keep an eye out for whether or not this gets printed anywhere, 
  #some version of this should be made visible to the user. Also this exact code gets used multiple times, should probably be hived off into a function. 
  det_data <- remove_unnamed_columns(det_data)
  
  ## add embargo_date variable if not present so downstream code works
  if(!"embargo_date" %in% names(det_data)) {
    det_data <- det_data %>% 
      mutate(embargo_date = NA)
  }
  
  ## receiver deployment metadata - required for receiver depth
  if(!is.null(rmeta)) {
    rec_meta <- read_csv(rmeta)
    
    rec_meta <- remove_unnamed_columns(rec_meta)
  }
  
  #tag deployment metadata
  if(!is.null(tmeta)) {
    extension <- file_ext(tmeta)
    if(extension == "csv"){
      tag_meta <- read_csv(tmeta, na = c("", "null", "NA"))
    }
    if(extension == "xlsx"){
      tag_meta <- read_excel(tmeta, sheet="Tag Metadata", skip = 4, col_names = TRUE)
    }
    tag_meta <- remove_unnamed_columns(tag_meta)
  }
  
  if(!is.null(meas)) {
    #Why no null spec in this one as opposed to the others? Very strange - BD
    anim_meas <- read_csv(meas)
    
    anim_meas <- remove_unnamed_columns(anim_meas)
  }
  
  #In the original column here follows some initial QC and a big merge of the four data files, complete with hardcoded column names.
  #We're gonna do the same merge but first we're going to take OTN formatted data and massage the columns into an IMOS-friendly setup.
  
  if(tolower(data_format) == "otn") {
    processed_data <- otn_imos_column_map(det_data, rec_meta, tag_meta, derive=TRUE)
    det_data <- processed_data$detections
    rec_meta <- processed_data$receivers
    tag_meta <- processed_data$tags
  }
  
  #Check for and report any tags in detections but not in the tag metadata.
  missing_ids <- vector()
  
  if(!is.null(tag_meta)){
    missing_ids <- 
      unique(det_data[det_id_column])[!unique(det_data[det_id_column]) %in% tag_meta[tag_id_column]]
  }
  
  #The original code has a more detailed process here of creating detection names, we don't have the luxury of having all our
  #columns predetermined though so we're going to go a little narrower. 
  if(length(missing_ids) > 0) {
    #Write to the logfile
    write(
      #This might not work, will have to check later.
      paste0(
        "The following IDs are in the detections data, but not the transmitter metadata.",
        paste(missing_ids, collapse = " \n ")
      ),
      file = logfile,
      append = TRUE
    )
  }
  
  else if(!is.null(tag_meta)){
    warning("No missing IDs found in tag metadata.", call. = FALSE, immediate. = TRUE)
  } else {
    warning("transmitter metadata not supplied, skipping tests for missing metadata records",
            call. = FALSE, immediate. = TRUE)
  }
  
  #Now do the same with receiver metadata (Can this maybe be busted out to a function of its own? Food for thought - BD)
  missing_ids <- vector()
  if(!is.null(rec_meta)){
    missing_ids <- 
      unique(det_data[det_rcvr_column])[!unique(det_data[det_rcvr_column]) %in% rec_meta[rcvr_id_column]]
  }
  
  #The original code has a more detailed process here of creating detection names, we don't have the luxury of having all our
  #columns predetermined though so we're going to go a little narrower. 
  if(length(missing_ids) > 0) {
    #Write to the logfile
    write(
      #This might not work, will have to check later.
      paste0(
        "The following IDs are in the detections data, but not the receiver metadata.",
        paste(missing_ids, collapse = " \n ")
      ),
      file = logfile,
      append = TRUE
    )
  }
  
  else {
    warning("receiver metadata not supplied, skipping tests for missing metadata records",
            call. = FALSE, immediate. = TRUE)
  }
  
  #View(det_data)
  
  if(!is.null(rec_meta)) {
    message("rec_meta is not null")
    ## merge detections with receiver metadata - to get receiver_depth,
    ##    but merge everything & keep detections data version of common variables
    dd <-
      left_join(det_data, rec_meta, by = "receiver_deployment_id") %>%
      dplyr::select(
        transmitter_id,
        tag_id,
        transmitter_deployment_id,
        tagging_project_name,
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
      dplyr::select(
        -receiver_name.y,-receiver_project_name.y,-installation_name.y,-station_name.y,-receiver_deployment_longitude.y,-receiver_deployment_latitude.y
      )
  } else {
    dd <- det_data %>%
      dplyr::select(
        transmitter_id,
        tag_id,
        transmitter_deployment_id,
        tagging_project_name,
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
    dd <- left_join(dd,
                    tag_meta,
                    by = c("transmitter_id", "transmitter_deployment_id")) %>%
      dplyr::select(
        -transmitter_serial_number.y,
        -tagging_project_name.y,
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
      dplyr::select(
        -transmitter_deployment_latitude.y,
        -transmitter_deployment_longitude.y,
        -transmitter_deployment_datetime.y
      ) %>%
      rename(
        tagging_project_name = tagging_project_name.x,
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
        embargo_date = embargo_date.y,
        latitude = latitude.x,
        longitude = longitude.x,
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
  
  if(!is.null(anim_meas)) {
    ## concatenate all measurements into single record by unique id
    anim_meas <- anim_meas %>%
      mutate(meas =
               paste(measurement_type, "=", measurement_value, measurement_unit)) %>%
      dplyr::select(transmitter_deployment_id, meas) %>%
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
                               
#Just copying the ugly code above for now until I figure out something nicer and less magic-numbery.
remove_unnamed_columns <- function(dataframe) {
  if(any(paste0("X",1:20) %in% names(dataframe))) {
    drops <- paste0("X",1:20)[paste0("X",1:20) %in% names(dataframe)]
    dataframe <- dataframe %>% dplyr::select(-any_of(drops))
  }
  return(as.data.frame(dataframe))
}
