#' @title Link the QC Detection Dataset to the Sensor Value obtained by the Nearest IMOS Mooring
#' @description helper function, not called by user
#'
#' @param trackingData dataframe containing acoustic detection data in IMOS QC format. This could also have the moorings site_code next to it as generated using the \code{getDistance} function.
#' @param moorData optional dataframe containing the sensor data from a single IMOS mooring. 
#' @param timeMaxh optional numeric string containing the maximum time threshold in hours to merge detection and mooring sensor values. 
#' @param distMaxkm optional numeric string containing the maximum distance threshold in km to merge detection and mooring sensor values. 
#' @return The \code{trackingData} dataframe with the sensor values from the nearest  \code{moor_site_code} 
#' @importFrom data.table setDT := setkey setcolorder setattr
#' @importFrom lubridate parse_date_time2 interval dhours as.duration
#' @importFrom dplyr arrange mutate case_when select
#' @keywords internal

merge.moor <- function(trackingData, moorData, timeMaxh=Inf, distMaxkm=Inf) {

  setDT(trackingData)   # Convert to a data table - required for the nearest function to work
  setDT(moorData)   # Convert to a data table - required for the nearest function to work
  
  # reset the time stamps to character strings
  trackingData <- trackingData[,detection_datetime := as.character(detection_datetime)]
  moorData <- moorData[,moor_timestamp := as.character(moor_timestamp)]
  
  # now set the datetime columns to identical format and tz
  trackingData <- trackingData[,detection_datetime := parse_date_time2(detection_datetime, orders="YmdHMS", tz="UTC")]
  moorData <- moorData[,moor_timestamp := parse_date_time2(moor_timestamp, orders="YmdHMS", tz="UTC")]
  
  # Create new column in the tracking and moorings datasets to assist with merging
  trackingData[, time := as.POSIXct(round(detection_datetime, units="hours"))]
  moorData[, time := moor_timestamp]
  
  setkey(trackingData,time)    
  setkey(moorData,time)    
  
  # perform a Rolling join where each tag_detection_datetime in trackingData is matched to the nearest moor_timestamp in moorData
  Envmatch <- moorData[trackingData, roll="nearest", allow.cartesian=TRUE] 
  
  Envmatch <- Envmatch[, time := NULL]   # remove the time column from the datatable 
  trackingData <- trackingData[, time := NULL]   # remove the time column from the datatable
  moorData <- moorData[, time := NULL]  # remove the time column from the datatable
  
  # Set the column order in the output table
  setcolorder(Envmatch, c(names(trackingData), names(moorData)[-1])) # Set the column order in the merge but drop the duplicate column for moor_site_code
  
  # Set the row order in the output table
  Envmatch <- Envmatch %>% arrange(detection_id, moor_depth)
  
  # Extract the time difference between mooring and detection time
  #elapsed.time <- Envmatch$detection_datetime %--% Envmatch$moor_timestamp
  elapsed.time <- interval(Envmatch$detection_datetime,Envmatch$moor_timestamp)
  Envmatch$timediff_h <- abs(as.duration(elapsed.time) / dhours(1)) # Add the mismatch in time between the two datasets in hours
  Envmatch$timediff_h <- round(Envmatch$timediff_h,4) # round to 4 decimal places
  
  varnames <- names(moorData)[names(moorData) %in% c("moor_sea_temp", "moor_ucur", "moor_vcur", "moor_psal")] 
  
  ## convert to tibble
  setattr(Envmatch, "class", c("tbl", "tbl_df", "data.frame"))
  
  ### where time or distance thresholds are broken, return NA in new column, varname_filter
  timethres <- ifelse(Envmatch$timediff_h >= timeMaxh, FALSE, TRUE)
  distthres <- ifelse(Envmatch$closest_moor_km >= distMaxkm, FALSE, TRUE)
  Envmatch$breakthres <- timethres&distthres
  
  Envmatch <- 
    Envmatch %>% 
    mutate(var = case_when(breakthres %in% T ~ !!as.name(varnames[1]))) 

  colnames(Envmatch)[colnames(Envmatch) %in% "var"] <- paste0(varnames[1], "_filter")
 
  if (length(varnames) > 1) {
    Envmatch <- 
      Envmatch %>% 
      mutate(var = case_when(breakthres %in% T ~ !!as.name(varnames[2])))
    
    colnames(Envmatch)[colnames(Envmatch) %in% "var"] <- paste0(varnames[2], "_filter")  
    }

  # replace mooring observations column with observations filtered by time and distance thresholds  
  Envmatch <- Envmatch %>% select(-breakthres)
  
  Envmatch[,varnames[1]] <- Envmatch[,paste0(varnames[1], "_filter")]
  Envmatch <- Envmatch %>% 
    select(-!!as.name(paste0(varnames[1], "_filter")))
  
  if(length(varnames)>1){
    Envmatch[,varnames[2]] <- Envmatch[,paste0(varnames[2], "_filter")]
    Envmatch <- Envmatch %>% 
    select(-!!as.name(paste0(varnames[2], "_filter")))
  }

  Envmatch <- Envmatch %>% select(-i.moor_site_code)
  return(Envmatch)
}

