##' @title Find closest mooring 
##' @description Links the QC Detection Dataset to the Nearest IMOS Mooring ID
##'
##' @param moorLocations dataframe containing the locations of IMOS moorings
##' @param trackingData dataframe containing acoustic detection data in IMOS QC format
##' @return The \code{trackingData} dataframe with the nearest  \code{moor_site_code} to each acoustic detection
##' 
##' @importFrom sf st_as_sf st_distance
##' @importFrom lubridate interval %within%
##' @importFrom fasttime fastPOSIXct
##' 
##' @export

getDistance <- function(trackingData, moorLocations){

  trackingData <- cbind(detection_id =1:nrow(trackingData),trackingData)
  det_sf <- st_as_sf(trackingData, coords = c("receiver_deployment_longitude", 
                                              "receiver_deployment_latitude"),crs=4326)
  moor_sf <- st_as_sf(moorLocations, coords = c("longitude", "latitude"), crs = 4326)
  dst <- st_distance(det_sf,moor_sf)
  trackingData$closest_moor_km <- round(apply(dst,1,FUN=min)/1000,3) # Convert to km
  moor.close <- apply(dst,1,FUN=which.min) 
  trackingData$moor_site_code <- moorLocations$site_code[moor.close] # Binds the site code (need for data lookup)
  trackingData$moor_coverage_start <- moorLocations$time_coverage_start[moor.close] # bind moorings data start date
  trackingData$moor_coverage_end <- moorLocations$time_coverage_end[moor.close] # bind moorings data end date
  int2 <- interval(trackingData$moor_coverage_start, trackingData$moor_coverage_end) # Create interval object
  trackingData$detection_datetime <- fastPOSIXct(trackingData$detection_datetime) # Function to quickly convert to POSIXct
  trackingData$is.coverage <- `%within%`(trackingData$detection_datetime, int2)
    
    #trackingData$detection_datetime %within% int2 # Returns TRUE if detection timestamp within mooring deployment period
  return(trackingData)
}


