#Short utility function to get some buffer for shapefile cropping. 

getBuffer <- function(detection_extract, buffer_percentage = 0.15) {
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
  minLat <- min(otn_test_data$latitude) - latBuffer
  minLon <- min(otn_test_data$longitude) - lonBuffer
  maxLat <- max(otn_test_data$latitude) + latBuffer
  maxLon <- max(otn_test_data$longitude) + lonBuffer
  
  #Now we can return a vector of the lats and lons so they can be used.
  buffers <- c(xmin=minLon, ymin=minLat, xmax=maxLon, ymax=maxLat)
  
  return(buffers)
}