##' @title Find closest mooring 
##' @description Links the QC Detection Dataset to the Nearest IMOS Mooring ID
##'
##' @param moorLocations dataframe containing the locations of IMOS moorings
##' @param trackingData dataframe containing acoustic detection data in IMOS QC format
##' @param X name of column with X coordinate or longitude (EPSG 4326)
##' @param Y name of column with Y coordinate or latitude (EPSG 4326)
##' @param datetime name of column with date time stamp (Coordinated Universal 
##' Time; UTC)
##' @return The \code{trackingData} dataframe with the nearest  \code{moor_site_code} to each acoustic detection
##' 
##' @importFrom sf st_as_sf st_distance
##' @importFrom lubridate interval %within%
##' @importFrom fasttime fastPOSIXct
##' 
##' @export

getDistance <- function(trackingData, moorLocations, X="receiver_deployment_longitude", Y="receiver_deployment_latitude", datetime="detection_datetime"){
  
  if(!X %in% colnames(trackingData)){stop("Cannot find X coordinate in dataset, provide column name where variable can be found")}
  if(!Y %in% colnames(trackingData)){stop("Cannot find Y coordinate in dataset, provide column name where variable can be found")}
  if(!datetime %in% colnames(trackingData)){stop("Cannot find date timestamp column in dataset, provide column name where variable can be found")}
  
  names(trackingData)[names(trackingData) == datetime] <- "detection_datetime" # Allows flexibility with regards to datetime column input, standardizes name for other moorings function
  
  trackingData <- cbind(detection_id =1:nrow(trackingData),trackingData)
  det_sf <- st_as_sf(trackingData, coords = c(X, 
                                              Y),crs=4326)
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
    
  return(trackingData)
}


