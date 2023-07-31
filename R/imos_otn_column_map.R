##' @title Map IMOS receiver metadata to an OTN-like format
##' @description In the same way that otn_imos_column_map takes OTN data and massages it into an IMOS-like format for REMORA, 
##' this function and its ilk take IMOS data (in this case, receiver metadata) and massage it into an OTN-like format, for the
##' purposes of reporting and more general applicability within the OTN suite of programs.
##' 
##' @param det_dataframe ...
##' @param rcvr_dataframe A dataframe containing IMOS receiver metadata. 
##' @param tag_dataframe ...
##' @param derive ...
##'
##' @return A dataframe containing the above data in an OTN-like format. 
##' 
##' @importFrom dplyr '%>%' mutate rename
##' @export
##'

imos_otn_column_map <- function(det_dataframe,
                                rcvr_dataframe = NULL,
                                tag_dataframe = NULL,
                                derive = TRUE) {
  
  
  #This way, if we don't end up having any way to change these throughout- i.e, no rcvr/tag sheets have been passed-
  #we just return whatever we got, unaltered. Probably null. 
  tag_return <- tag_dataframe
  rcvr_return <- rcvr_dataframe
  
  #Quit instantly if there is no detections dataframe. This is unlikely since this check already happens in the function above, but for completeness'
  #sake we'll include it. 
  if(is.null(det_dataframe)) stop("\033[31;1mCan not run imos -> otn conversion without a detections file!\033[0m\n")
  
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
      detection_datetime,
      receiver_name,
      transmitter_id,
      transmitter_sensor_raw_value,
      transmitter_sensor_unit,
      station_name
    ) %>%
    mutate(
      #cleandate = ymd(as_date(detection_datetime)),
      transmitter_name = NA,
      transmitter_serial = NA,
      latitude = NA,
      longitude = NA
    )%>%
    rename(
      Date_and_time = detection_datetime,
      receiver = receiver_name,
      transmitter = transmitter_id,
      sensor_value = transmitter_sensor_raw_value,
      sensor_unit = transmitter_sensor_unit
    )
  det_return$transmitter_deployment_id <- det_return$tag_id
  
  if(!is.null(rcvr_dataframe) && !derive) {
    rcvr_return <- rcvr_dataframe %>%
      dplyr::select(
        receiver_project_name,
        station_name,
        receiver_deployment_datetime,
        receiver_deployment_latitude,
        receiver_deployment_longitude,
        depth_below_surface,
        receiver_name,
        receiver_status,
        receiver_recovery_datetime
      ) %>%
      mutate (
        BOTTOM_DEPTH = NA,
        RISER_LENGTH = NA,
        CODE_SET = NA,
        AR_MODEL_NUMBER = NA,
        AR_SERIAL_NUMBER = NA,
        DATA_DOWNLOADED = NA,
        DOWNLOAD_DATE_TIME = NA,
        COMMENTS = NA
      ) %>%
      rename (
        OTN_ARRAY = receiver_project_name,
        STATION_NO = station_name,
        DEPLOY_DATE_TIME = receiver_deployment_datetime,
        DEPLOY_LAT = receiver_deployment_latitude,
        DEPLOY_LONG = receiver_deployment_longitude,
        INSTRUMENT_DEPTH = depth_below_surface,
        
        #Receiver_name needs to be split somewhere up above to get the right values for those two. 
        #INS_MODEL_NUMBER = receiver_name,
        #INS_SERIAL_NUMBER = receiver_name,
        
        RECOVERED = receiver_status,
        
        RECOVER_DATE_TIME = receiver_recovery_datetime
      )
    
    #Have to do a little extra manipulation on the dataframe to give "RECOVERED"
    #sensible values. 
    
    #First we have to update the comments where the receiver has been returned to vendor.
    rcvr_return <- within(rcvr_return, 
                          COMMENTS[RECOVERED == 'returned to vendor'] <- 'returned to vendor')
    
    #Now we can change the values in the RECOVERED column to fit our standard.
    rcvr_return$RECOVERED[rcvr_return$RECOVERED=='damaged' ||
                            rcvr_return$RECOVERED=='returned to vendor'] <- 'failed'
  }
  
  #And if we have tag metadata, convert that too.
  if(!is.null(tag_dataframe) && !derive) {
    tag_return <- tag_dataframe %>%
      dplyr::select(
        #Columns we can get from the file.
        transmitter_type,
        transmitter_serial_number,
        transmitter_id,
        placement,
        transmitter_estimated_battery_life,
        species_common_name,
        species_scientific_name,
        measurement_value,
        measurement_type,
        measurement_unit,
        animal_sex,
        transmitter_deployment_locality,
        transmitter_deployment_latitude,
        transmitter_deployment_longitude,
        transmitter_deployment_datetime,
        transmitter_recovery_datetime,
        comments,
        transmitter_deployment_comments
      ) %>%
      mutate(
        #Columns we need to change, or add as NAs.
        TAG_TYPE = "ACOUSTIC",
        TAG_MANUFACTURER = "VEMCO",
        TAGGER = NA,
        TAG_OWNER_PI = NA,
        TAG_OWNER_ORGANIZATION = NA,
        WILD_OR_HATCHERY = NA,
        STOCK = NA,
        `WEIGHT (kg)` = NA,
        LIFE_STAGE = NA,
        AGE = NA,
        AGE_UNITS = NA,
      
      ) %>%
      rename(
        #Columns we now have that need to be renamed. 
        TAG_MODEL = transmitter_type,
        TAG_SERIAL_NUMBER = transmitter_serial_number,
        TAG_IMPLANT_TYPE = placement,
        EST_TAG_LIFE = transmitter_estimated_battery_life,
        COMMON_NAME_E = species_common_name,
        SCIENTIFIC_NAME = species_scientific_name,
        SEX = animal_sex,
        RELEASE_LOCATION = transmitter_deployment_locality,
        RELEASE_LATITUDE = transmitter_deployment_latitude,
        RELEASE_LONGITUDE = transmitter_deployment_longitude,
        UTC_RELEASE_DATE_TIME = transmitter_deployment_datetime,
        HARVEST_DATE = transmitter_recovery_datetime
      )
      # ) %>%
      # separate(
      #   col = transmitter_id,
      #   into = c("code_space_1", "code_space_2", "TAG_ID_CODE")
      # ) %>%
      # unite(
      #   "TAG_CODE_SPACE",
      #   code_space_1, code_space_2,
      #   sep = "-"
      # ) %>%
      # unite(
      #   "COMMENTS",
      #   comments, transmitter_deployment_comments,
      #   sep = ";"
      # )
      
  }
}

#use  receiver_status = lost as 'lost', damaged as 'failed', 
#returned to vendor as 'failed' with comment 'returned to vendor'
map_receiver_status <- function(status) {
  
}