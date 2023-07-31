##' @title Create Buffer to Crop Shapefile 
##'
##' @description Create a buffer from detection data to crop species range shapefile
##'
##' @param detection_extract A detection extract file containing a latitude/longitude column.
##' @param buffer_percentage The percentage (between 0 and 1) to add to the lat/lon of the bounding box, in case the user wants more space around their area.
##' 
##' @details Creates a buffer for shapefile cropping from location info in the detection_extract 
##'
##' @return returns a buffer as a 4-element vector
##'
##' @export

#Short utility function to get some buffer for shapefile cropping. 

getBuffer <- function(detection_extract, 
                      buffer_percentage = 0.15) {
  #First we've got to get the min/max of each of lat and lon
  
  minLat <- min(detection_extract$latitude)
  maxLat <- max(detection_extract$latitude)
  
  minLon <- min(detection_extract$longitude)
  maxLon <- max(detection_extract$longitude)
  
  #Now we get the difference between the two
  latDiff <- abs(maxLat - minLat)
  lonDiff <- abs(maxLon - minLon)
  
  #Now we can get how much buffer to apply on each axis.
  latBuffer <- latDiff * buffer_percentage
  lonBuffer <- lonDiff * buffer_percentage
  
  #With that in hand we can apply it to the lat and lon. 
  minLat <- min(detection_extract$latitude) - latBuffer
  minLon <- min(detection_extract$longitude) - lonBuffer
  maxLat <- max(detection_extract$latitude) + latBuffer
  maxLon <- max(detection_extract$longitude) + lonBuffer
  
  #Now we can return a vector of the lats and lons so they can be used.
  #These are named so that they can be passed directly into sf::st_crop to generate the cropped shapefile.
  buffers <- c(xmin=minLon, ymin=minLat, xmax=maxLon, ymax=maxLat)
  
  return(buffers)
}