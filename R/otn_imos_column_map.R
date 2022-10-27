#In order to save ourselves work going forward, this function takes a set of OTN-formatted data and mutates the columns in each dataframe to produce equivalencies
#To the IMOS spec. This lets most of REMORA run without further argument- hopefully. 

#Derive is just a flag for if we're working with OTN data, have a detection extract but no receiver metadata or tag metadata. This way, we can run it
#both ways- if Derive is false, we'll knock together an IMOS-format detections dataframe and run it through Remora's built-in functionality for running
#detections without receiver/tag metadata; if it's true, then we'll try to scuff rcvr and tag dataframes out of the detection extract file. 

otn_imos_column_map <- function(det_dataframe, rcvr_dataframe = NULL, tag_dataframe = NULL, derive = TRUE) {
  #We need to ultimately produce the following:
  # - A detections dataframe with columns appropriate to the IMOS spec. 
  # - A receiver dataframe with appropriate columns, if necessary with data derived from the detections dataframe.
  # - A tag dataframe, same constraints as above.
  # - An animal measurements dataframe with data derived from the tag dataframe.
  
  #At the end of all this we return three dataframes- det_data, rec_data, tag_data- that can be passed through to the merge at the bottom of the get_data function.
  #Animal measurements is a bit weird so I'm going to ignore it for now.
  
  #This probably won't have the full range of columns that the equivalent IMOS data would. 
  #Let's start with just the detections, since that's a possibility we have to account for- that we only get a detection extract and have to work
  #outwards from there.
  
  #NOTE TO SELF: Remora can already handle if you don't have rcvr/tag metadata. Don't make this more complicated than it has to be. 
  
  #This way, if we don't end up having any way to change these throughout- i.e, no rcvr/tag sheets have been passed-
  #we just return whatever we got, unaltered. Probably null. 
  tag_return <- tag_dataframe
  rcvr_return <- rcvr_dataframe
  
  #Quit instantly if there is no detections dataframe. This is unlikely since this check already happens in the function above, but for completeness'
  #sake we'll include it. 
  if(is.null(det_dataframe)) stop("\033[31;1mCan not run otn -> imos conversion without a detections file!\033[0m\n")
  
  #If we don't get passed a receiver or tag dataframe, we derive them from det. This will give us hopefully enough info that we can create the final
  #detection dataframe to be returned, which will be valid for Remora. Ideally. 
  if(is.null(rcvr_dataframe) && derive) {
    message("Deriving receiver dataframe...")
    rcvr_return <- derive_rcvr_from_det(det_dataframe)
  }
  
  if(is.null(tag_dataframe) && derive) {
    message("Deriving tag dataframe...")
    tag_return <- derive_tag_from_det(det_dataframe)
  }
  
  #Start by mapping the Detections dataframe.
  det_return <- det_dataframe %>%
    dplyr::select(
      datecollected,
      catalognumber,
      tagname,
      collectioncode,
      commonname,
      scientificname,
      detectedby,
      receiver_group,
      station,
      receiver,
      sensortype,
      sensorvalue,
      sensorunit,
      longitude,
      latitude,
      yearcollected,
      monthcollected,
      daycollected
    ) %>%
    mutate(
      cleandate = ymd(as_date(datecollected)),
      CAAB_species_id = NA,
      WORMS_species_aphia_id = NA,
      animal_sex = NA,
      receiver_name = NA,
      receiver_project_name = NA,
      transmitter_serial_number = NA,
      transmitter_type = NA,
      transmitter_sensor_type = NA,
      transmitter_sensor_slope = NA,
      transmitter_sensor_intercept = NA,
      transmitter_sensor_unit = NA,
      transmitter_status = NA,
      transmitter_estimated_battery_life = NA,
      embargo_date = NA,
      transmitter_deployment_latitude = NA,
      transmitter_deployment_longitude = NA,
      transmitter_deployment_datetime = NA,
      
    )%>%
    #We need to make these transmitter and receiver deployment ID columns so that we have something to join on. These do not necessarily correspond to our
    #own catalognumber fields. 
    unite(
      receiver_deployment_id, c("receiver_group", "station", "receiver"), sep = "-", remove = FALSE
    ) %>%
    #unite(
    #  transmitter_deployment_id, c("tagname", "cleandate", "latitude", "longitude"), sep = "-",  remove = FALSE
    #) %>%
    rename(
      transmitter_id = tagname,
      tag_id = catalognumber,
      tagging_project_name = collectioncode, 
      species_common_name = commonname,
      species_scientific_name = scientificname,
      detection_datetime = datecollected,
      installation_name = receiver_group,
      station_name = station,
      receiver_id = receiver,
      #receiver_name = receiver, #Could be not the right thing to do to have both this and receiver_id be receiver?
      receiver_deployment_longitude = longitude,
      receiver_deployment_latitude = latitude #counterintuitively, we rename these here even though they get renamed BACK to their originals
      #back outside this function. The code outside still has to work for IMOS formatted data so we can't change it too much, so when we massage
      #the column names like so, we have to introduce a step that maybe we'd rather skip. 
    )
  det_return$transmitter_deployment_id = det_return$tag_id
  
  #If we have receiver_meta, convert that to an IMOS friendly version. 
  if(!is.null(rcvr_dataframe) && !derive) {
    #Build this out once basic case is handled.
    rcvr_return <- rcvr_dataframe %>%
      dplyr::select(
        OTN_ARRAY,
        STATION_NO,
        DEPLOY_DATE_TIME,
        DEPLOY_LAT,
        DEPLOY_LONG,
        BOTTOM_DEPTH,
        RISER_LENGTH,
        INSTRUMENT_DEPTH,
        INS_MODEL_NUMBER,
        INS_SERIAL_NUMBER,
        CODE_SET,
        TRANSMITTER,
        TRANSMIT_MODEL,
        AR_MODEL_NUMBER,
        AR_SERIAL_NUMBER,
        DEPLOYED_BY,
        RECOVERED,
        RECOVER_DATE_TIME,
        RECOVER_LAT,
        RECOVER_LONG,
        DATA_DOWNLOADED,
        DOWNLOAD_DATE_TIME,
        FILENAME
      ) %>%
      #We're going to merge INS_MODEL_NUMBER and INS_SERIAL_NUMBER to make receiver_name
      unite(
        receiver_name, c("INS_MODEL_NUMBER", "INS_SERIAL_NUMBER"), sep = "-", remove = FALSE
      ) %>%
      unite(
        receiver_deployment_id, c("OTN_ARRAY", "STATION", "INS_SERIAL_NUMBER"), sep = "-", remove = FALSE
      )%>%
      rename(
        receiver_status = RECOVERED,
        receiver_deployment_datetime = DEPLOY_DATE_TIME,
        installation_name = OTN_ARRAY,
        station_name = STATION_NO,
        receiver_deployment_latitude = DEPLOY_LAT,
        receiver_deployment_longitude = DEPLOY_LONG,
        depth_below_surface = INSTRUMENT_DEPTH, 
        receiever_recovery_datetime = RECOVER_DATE_TIME,
        receiver_recovery_latitude = RECOVER_LAT,
        receiver_recovery_longitude = RECOVER_LONG,
      ) %>%
      mutate(
        purchasing_organization = NA,
        receiver_project_name = NA, 
      )
  }
  
  #And if we have tag metadata, convert that too.
  if(!is.null(tag_dataframe) && !derive) {
    tag_return <- tag_dataframe %>%
      dplyr::select(
        `ANIMAL_ID   (floy tag ID, pit tag code, etc.)`,
        TAG_TYPE,
        TAG_MANUFACTURER,
        TAG_MODEL,
        TAG_SERIAL_NUMBER,
        TAG_ID_CODE,
        TAG_CODE_SPACE,
        TAG_IMPLANT_TYPE,
        TAG_IMPLANT_METHOD,
        TAG_ACTIVATION_DATE,
        EST_TAG_LIFE,
        TAGGER,
        TAG_OWNER_PI,
        TAG_OWNER_ORGANIZATION,
        COMMON_NAME_E,
        SCIENTIFIC_NAME,
        CAPTURE_LOCATION,
        CAPTURE_LATITUDE,
        CAPTURE_LONGITUDE,
        WILD_OR_HATCHERY,
        STOCK,
        #`LENGTH (m)`,
        #`WEIGHT (kg)`,
        LENGTH_TYPE,
        #`LENGTH2 (m)`,
        #LENGTH2_TYPE,
        LIFE_STAGE,
        AGE,
        AGE_UNITS,
        SEX,
        DNA_SAMPLE_TAKEN,
        TREATMENT_TYPE,
        RELEASE_GROUP,
        UTC_RELEASE_DATE_TIME,
        RELEASE_LOCATION,
        RELEASE_LATITUDE,
        RELEASE_LONGITUDE,
        COMMENTS, 
        HARVEST_DATE
      ) %>%
      mutate(
        cleandate = ymd(as_date(UTC_RELEASE_DATE_TIME)),
        transmitter_sensor_slope = NA,
        transmitter_sensor_intercept = NA,
        transmitter_sensor_unit = NA,
        transmitter_status = NA,
        embargo_date = NA,
        transmitter_recovery_latitude = NA,
        transmitter_recovery_longitude = NA,
        tagging_project_name = NA,
      ) %>%
      unite(
        transmitter_id, c("TAG_CODE_SPACE", "TAG_ID_CODE"), sep="-", remove = FALSE
      ) %>%
      unite(
        transmitter_deployment_id, c("TAG_CODE_SPACE", "TAG_ID_CODE", "cleandate", "RELEASE_LATITUDE", "RELEASE_LONGITUDE"), sep="-", remove = FALSE
      ) %>%
      rename(
        transmitter_serial_number = TAG_SERIAL_NUMBER,
        transmitter_type = TAG_MODEL,
        transmitter_sensor_type = TAG_TYPE,
        transmitter_estimated_battery_life = EST_TAG_LIFE,
        species_common_name = COMMON_NAME_E,
        species_scientific_name = SCIENTIFIC_NAME,
        animal_sex = SEX,
        placement = TAG_IMPLANT_TYPE,
        transmitter_deployment_locality = RELEASE_LOCATION,
        transmitter_deployment_latitude = RELEASE_LATITUDE,
        transmitter_deployment_longitude = RELEASE_LONGITUDE,
        transmitter_deployment_datetime = UTC_RELEASE_DATE_TIME,
        transmitter_deployment_comments = COMMENTS,
        transmitter_recovery_datetime = HARVEST_DATE,
      )
  }
  return(list("detections" = det_return, "receivers" = rcvr_return, "tags" = tag_return))
}

#Hack together a piecemeal receiver metadata dataframe for instances where we get detection data, no receiver/tag metadata, but still want to act
#as though we DID get receiver/tag metadata.
derive_rcvr_from_det <- function(det_dataframe) {
  #Group by receiver name and station ID. 
  rcvr_grouped <- det_dataframe %>%
    group_by(receiver, station) %>%
    mutate(
      minDetectionDate = min(datecollected),
      maxDetectionDate = max(datecollected)
    ) %>%
    distinct(receiver, station, .keep_all = TRUE)
  
  rcvr <- rcvr_grouped %>%
    dplyr::select(
      #Select the columns from the detection extract that we have access to.
      detectedby,
      station,
      receiver,
      receiver_depth,
      longitude,
      latitude,
      receiver_group,
      minDetectionDate,
      maxDetectionDate
    ) %>%
    unite(
      receiver_deployment_id, c("receiver_group", "station", "receiver"), sep = "-", remove = FALSE
    ) %>%
    rename (
      #Rename those columns to fit the imos spec
      installation_name = detectedby,
      station_name = station,
      receiver_name = receiver,
      depth_below_surface = receiver_depth,
      receiver_deployment_longitude = longitude,
      receiver_deployment_latitude = latitude
    ) %>%
    mutate (
      #Add NA-filled columns for anything that can't be derived.
      purchasing_organisation = NA,
      receiver_project_name = NA,
      receiver_status = NA, 
      receiver_deployment_datetime = NA,
      receiver_recovery_datetime = NA,
      receiver_recovery_longitude = NA,
      receiver_recovery_latitude = NA
    )
  
  return(as.data.frame(rcvr))
  
  
}

derive_tag_from_det <- function(det_dataframe) {
  #Group by tagname. We may need to add the option to use alternative columns in the future, but that's doable, I think. 
  distinctTag <- det_dataframe %>%
    group_by(tagname) %>%
    distinct(tagname, .keep_all=TRUE)
  
  #To get the correct transmitter lat/lon, we need to get the releases.
  releases <- det_dataframe %>%
    filter(receiver == "release") %>%
    group_by(catalognumber) %>%
    distinct(catalognumber, .keep_all = TRUE) %>%
    rename(
      transmitter_deployment_id = catalognumber,
      transmitter_deployment_latitude = latitude,
      transmitter_deployment_longitude = longitude,
      transmitter_deployment_datetime = datecollected
    )
  
  tag <- distinctTag %>%
    dplyr::select(
      tagname,
      commonname,
      scientificname,
      yearcollected,
      monthcollected,
      daycollected,
      longitude,
      latitude,
      catalognumber
    ) %>%
    rename (
      transmitter_id = tagname,
      species_common_name = commonname,
      species_scientific_name = scientificname,
      transmitter_deployment_id = catalognumber
    ) %>%
    mutate (
      transmitter_serial_number = NA,
      tagging_project_name = NA,
      transmitter_type = NA,
      transmitter_sensor_type = NA,
      transmitter_sensor_slope = NA,
      transmitter_sensor_intercept = NA,
      transmitter_sensor_unit = NA,
      transmitter_estimated_battery_life = NA,
      transmitter_status = NA,
      #transmitter_deployment_id = NA,
      animal_sex = NA,
      placement = NA,
      transmitter_deployment_locality = NA,
      transmitter_deployment_comments = NA,
      embargo_date = NA,
      transmitter_recovery_datetime = NA,
      transmitter_recovery_latitude = NA,
      transmitter_recovery_longitude = NA,
    )
  
  
  #Now we can join the releases to get the appropriate transmitter_deployment_lat/lon
  tag <- left_join(tag,
                   releases %>% dplyr::select(transmitter_deployment_id, 
                                              transmitter_deployment_latitude, 
                                              transmitter_deployment_longitude,
                                              transmitter_deployment_datetime),
                   by = "transmitter_deployment_id")
  
  return(as.data.frame(tag))
}