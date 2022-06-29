get_data_arbitrary <- function(det=NULL, rmeta=NULL, tmeta=NULL, meas=NULL, logfile, checks = c("all"),
                               det_id_column = "transmitter_deployment_id", tag_id_column = "transmitter_deployment_id",
                               det_rcvr_column = "receiver_deployment_id", rcvr_id_column = "receiver_deployment_id", data_format = "otn") {
  
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
  
  det_columns = data(paste0(data_format, 'detections_columns'))
  rec_columns = data(paste0(data_format, 'receiver_columns'))
  tag_columns = data(paste0(data_format, 'transmitter_columns'))
  meas_columns = data(paste0(data_format, 'animal_measurement_columns'))
  
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
  
  #OG code has a bit here about adding embargo_date column, don't think we'll include that since it's just to make the code later work and we're here
  #To make sure that the hard-coding is ripped out. So we won't be including explicit column names.
  
  ## receiver deployment metadata - required for receiver depth
  if(!is.null(rmeta)) {
    rec_meta <- read_csv(rmeta)
    
    rec_meta <- remove_unnamed_columns(rec_meta)
  }
  
  #tag deployment metadata
  if(!is.null(tmeta)) {
    tag_meta <- read_csv(tmeta, na = c("", "null", "NA"))
    
    tag_meta <- remove_unnamed_columns(tag_meta)
  }
  
  if(!is.null(meas)) {
    #Why no null spec in this one as opposed to the others? Very strange - BD
    anim_meas <- read_csv(meas)
    
    anim_meas <- remove_unnamed_columns(anim_meas)
  }
  
  #Check for and report any tags in detections but not in the tag metadata.
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
  
  else {
    warning("transmitter metadata not supplied, skipping tests for missing metadata records",
            call. = FALSE, immediate. = TRUE)
  }
  
  #Now do the same with receiver metadata (Can this maybe be busted out to a function of its own? Food for thought - BD)
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
    warning("transmitter metadata not supplied, skipping tests for missing metadata records",
            call. = FALSE, immediate. = TRUE)
  }
  
  #In the original column here follows a big merge of the four data files, complete with hardcoded column names. This is something I want additional input on.
  
}
                               
#Just copying the ugly code above for now until I figure out something nicer and less magic-numbery.
remove_unnamed_columns <- function(dataframe) {
  if(any(paste0("X",1:20) %in% names(dataframe))) {
    drops <- paste0("X",1:20)[paste0("X",1:20) %in% names(dataframe)]
    dataframe <- dataframe %>% select(-any_of(drops))
  }
  return(dataframe)
}
