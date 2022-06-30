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
  
  #Quit instantly if there is no detections dataframe. This is unlikely since this check already happens in the function above, but for completeness'
  #sake we'll include it. 
  if(is.null(det_dataframe)) stop("\033[31;1mCan not run otn -> imos conversion without a detections file!\033[0m\n")
  
  #If we don't get passed a receiver or tag dataframe, we derive them from det. This will give us hopefully enough info that we can create the final
  #detection dataframe to be returned, which will be valid for Remora. Ideally. 
  if(is.null(rcvr_dataframe) && derive) {
    rcvr_dataframe <- derive_rcvr_from_det(det_dataframe)
  }
  
  if(is.null(tag_dataframe) && derive) {
    tag_dataframe <- derive_tag_from_det(det_dataframe)
  }
  
  #If we have receiver_meta, we start working with that to convert.
  if(!is.null(rcvr_dataframe)) {
    #Build this out once basic case is handled.
  } else {
    #If we don't have receiver metadata, we derive our imos-friendly detection dataframe from our otn detection dataframe.
    det_return <- det_dataframe %>%
      select(
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
        latitude
      ) %>%
      rename(
        transmitter_id = tagname,
        tag_id = catalognumber,
        #transmitter_deployment_id = catalognumber,
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
      ) %>%
      mutate(
        CAAB_species_id = NA,
        WORMS_species_aphia_id = NA,
        animal_sex = NA,
        transmitter_deployment_id = NA, #This could be a problem one, since this is part of the join against the tag dataframe
        receiver_deployment_id = NA,
        receiver_name = NA
      )
  }
  
  #Write in functions for deriving minimal tag/receiver dataframes from the detection extract
  
  #Old version commented so I don't have to do all the mapping again. 
  # if(!is.null(det_dataframe)) {
  #   #We need to start by renaming the columns containing the appropriate data to the equivalent names that imos uses.
  #   dd <- det_dataframe %>%
  #     select(
  #       datecollected,
  #       tagname,
  #       collectioncode,
  #       commonname,
  #       scientificname,
  #       detectedby,
  #       receiver_group,
  #       station,
  #       receiver,
  #       sensortype,
  #       sensorvalue,
  #       sensorunit,
  #     ) %>%
  #     rename(
  #       detection_datetime = datecollected,
  #       transmitter_id = tagname,
  #       #tag_id = not sure, derived from tag dataframe?
  #       #transmitter_deployment_id = same as above
  #       tagging_project_name = collectioncode, 
  #       species_common_name = commonname,
  #       species_scientific_name = scientificname,
  #       #CAAB_species_id = I think we're just going to have to set this as null since most of what we're gonna be working with doesn't have one.
  #       #WORMS_species_aphia_id = might have to actually call out to WORMS for this? 
  #       #animal_sex = derive from tag sheet
  #       receiver_project_name = detectedby,
  #       installation_name = receiver_group,
  #       station_name = station,
  #       receiver_id = receiver,
  #       #receiver_deployment_longitude = same
  #       #receiver_deployment_latitude = same
  #       transmitter_sensor_type = sensortype,
  #       transmitter_sensor_value = sensorvalue,
  #       transmitter_sensor_unit = sensorunit,
  #       #transmitter_sensor_slope = , I don't think we store this information anywhere
  #       #transmitter_sensor_intercept = ,
  #       #transmitter_type = , Derived, I think, from tagmodel, as with all the ones below.
  #       #transmitter_serial_number = ,
  #       #transmitter_estimated_battery_life= ,
  #       #transmitter_status = ,
  #       #transmitter_deployment_longitude = ,
  #       #transmitter_deployment_latitude = ,
  #       #transmitter_deployment_datetime = ,
  #       #transmitter_dual_sensor = ,
  #     )
  # }
  
  return(list("detections" = det_return, "receivers" = rcvr_dataframe, "tags" = tag_dataframe)) #Don't forget to change these when you have stuff for the other two dataframes
}

#Hack together a piecemeal receiver metadata dataframe for instances where we get detection data, no receiver/tag metadata, but still want to act
#as though we DID get receiver/tag metadata.
derive_rcvr_from_det <- function(det_dataframe) {
  rcvr <- det_dataframe %>%
    select(
      #Select the columns from the detection extract that we have access to.
      detectedby,
      station,
      receiver,
      receiver_depth,
      longitude,
      latitude
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
      receiver_deployment_id = NA,
      purchasing_organisation = NA,
      receiver_project_name = NA,
      receiver_status = NA, 
      receiver_deployment_datetime = NA,
      receiver_recovery_datetime = NA,
      receiver_recovery_longitude = NA,
      receiver_recovery_latitude = NA
    )
  
  return(rcvr)
  
  
}

derive_tag_from_det <- function(det_dataframe) {
  tag <- det_dataframe %>%
    select(
      tagname,
      commonname,
      scientificname
    ) %>%
    rename (
      transmitter_id = tagname,
      species_common_name = commonname,
      species_scientific_name = scientificname
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
      transmitter_deployment_id = NA,
      animal_sex = NA,
      placement = NA,
      transmitter_deployment_locality = NA,
      transmitter_deployment_latitude = NA,
      transmitter_deployment_longitude = NA,
      transmitter_deployment_datetime = NA,
      transmitter_deployment_comments = NA,
      embargo_date = NA,
      transmitter_recovery_datetime = NA,
      transmitter_recovery_latitude = NA,
      transmitter_recovery_longitude = NA,
    )
  
  return(tag)
}