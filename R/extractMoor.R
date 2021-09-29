##' @title Link the QC Detection Dataset to the Sensor Value obtained by the Nearest IMOS Mooring
##' @description Extracts specified sensor value(s) observed by the IMOS mooring nearest to the supplied QC'd detection location(s)
##' @param trackingData dataframe containing acoustic detection data in IMOS QC format with the \code{moor_site_code} next to it as generated using the \code{mooringDistance} function.
##' @param file_loc character string describing folder location in which NetCDF files of mooring data have been saved after running mooringDownload function
##' @param sensorType character string containing name of mooring sensor to query. Can be "temperature", "velocity" or "salinity".
##' @param timeMaxh optional numeric string containing the maximum time threshold to merge detection and mooring sensor values. 
##' @param distMaxkm optional numeric string containing the maximum distance threshold in kilometers to include in output.
##' @param targetDepthm extracts the nearest sensor to this depth value. if set as NA then all sensors returned for this timestamp at this mooring
##' @param scalc select the lower or higher depth value when there are 2 options for sensors nearest the targetDepth provided. Users can also specify a mean or \code{NA} to return all sensor values. 
##' @return the \code{trackingData} dataframe as a nested tibble object with the sensor values from the nearest \code{moorLocations} that fall within the specified time, distance and depth thresholds.
##' 
##' @importFrom dplyr as_tibble group_by mutate select filter slice distinct ungroup arrange
##' @importFrom tidyr nest drop_na
##' @export

extractMoor <- function(trackingData,
                          file_loc,
                          sensorType = "temperature",
                          timeMaxh=Inf,
                          distMaxkm=Inf,
                          targetDepthm=NA, # from the depth selector function. NA is include all replicates and a number extracts the nearest sensor to this depth value
                          scalc=c("min","max","mean")) # If there are 2 options for the depth sensor nearest the value provided user can select the lower or higher value or the mean of the two
  
{
  sensorType <- match.arg(sensorType, choices = c("temperature","velocity","salinity","oxygen"))
  if(!sensorType %in% c("temperature","velocity","salinity")) 
    stop('Only "temperature", "velocity" or "salinity can be provided as a valid sensor type')
  
  # Split the detection dataframe into a list of data frames based on common value in the "site_code" column
  out <- split(as_tibble(trackingData), 
                     f = trackingData$moor_site_code)
  
  merge.moor.multi <- function(x){
    
    moorName.1 <- names(out)[x] # Get the name of the single mooring
    trackingData.1 <- out[[x]] # Get the tracking dataset associated with a single mooring
    
    # load moorings dataframe from a single site.code matching that of the detections dataframe
    moorData.1 <- mooringDownload(moor_site_code=moorName.1,
                                  fromWeb = FALSE,
                                  sensorType=sensorType,
                                  itimeout = 240,
                                  file_loc = file_loc)
    
    # merge the moorings and detections datasets
    comb.data.1 <- merge.moor(trackingData = trackingData.1, 
                              moorData = moorData.1[[1]],
                              timeMaxh = timeMaxh,
                              distMaxkm = distMaxkm)   
    return(comb.data.1)
  }
  
  comb.data.l <- lapply(1:length(out), merge.moor.multi)
  comb.data.df <- do.call(rbind,comb.data.l)
  
  #Remove sensor values that don't meet the distance and time threshold
  # comb.data.df <- comb.data.df %>%
  #   tidyr::drop_na(tempC)
  
  # Now extract the sensor data closest to our targetDepthm threshold
  # If targetDepthm is not provided, just return the original dataset with replicates representing the replicate sensors placed at various depths on the mooring
  if(is.na(targetDepthm)){
    dd2 <- comb.data.df
  }
  
  # If targetDepthm is provided, extract the sensor values nearest the depth provided
  if(is.numeric(targetDepthm)){
    dd <- comb.data.df %>% 
      drop_na(moor_depth) %>%
      mutate(detection_id=factor(detection_id),
                    depth_diff_m = moor_depth - targetDepthm,
                    index = abs(depth_diff_m)) %>% 
      group_by(detection_id) %>% 
      dplyr::filter(index %in% min(index))  %>%
      select(-index) 
    
    
    #  If there are 2 options for the depth sensor nearest the value provided user can select the lower or higher value or the mean of the two
    if(scalc== "min"){
      dd1 <- dd %>%
        slice(which.min(moor_depth)) 
    }
    
    if(scalc== "max"){
      dd1 <- dd %>%
        slice(which.max(moor_depth))
    }
    
    if(sensorType == "temperature"){
      if(scalc== "mean"){
        dd1 <- dd %>%
          mutate(moor_sea_temp = mean(moor_sea_temp,na.rm=T)) %>%
          distinct()
      }
    }
    
    if(sensorType == "velocity"){
      if(scalc== "mean"){
        dd1 <- dd %>%
          mutate(moor_ucur = mean(moor_ucur,na.rm=T)) %>%
          mutate(moor_vcur = mean(moor_vcur,na.rm=T)) %>%
          distinct()
      }
    }
    
    if(sensorType == "salinity"){
      if(scalc== "mean"){
        dd1 <- dd %>%
          mutate(moor_psal = mean(moor_psal))%>%
          distinct()
      }
    }
    
    # Change the detection id column back into a numeric string for sorting 
    dd2 <- dd1 %>%
      ungroup() %>%
      mutate(detection_id1 = as.numeric(levels(detection_id))[detection_id]) %>%
      arrange(detection_id1)
  }
  
  # Change column names so easily distinguishable from rs data
  # Group and nest by transmitter id
  
  if(sensorType == "temperature"){
  comb.data.out <- dd2 %>% 
    # dplyr::rename(moor_sea_temp = temperature,
    #               moor_depth = depth) %>%
    group_by(transmitter_id) %>% 
    tnest()
  }
  
  if(sensorType == "velocity"){
    comb.data.out <- dd2 %>% 
      # dplyr::rename(moor_ucur = ucur,
      #               moor_vcur = vcur,
      #               moor_depth = depth) %>%
      group_by(transmitter_id) %>% 
      nest()
  }
  
  if(sensorType == "salinity"){
    comb.data.out <- dd2 %>% 
      # dplyr::rename(moor_salinity = psal,
      #               moor_depth = depth) %>%
      group_by(transmitter_id) %>% 
      nest()
  }
  
  return(comb.data.out)
}
