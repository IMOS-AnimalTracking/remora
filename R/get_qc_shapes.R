#Function to generate shapefiles and transitionlayers for QC based on user-supplied parameters.

get_qc_shapes <- function(detection_extract, shapefile, worldimage = "./testDataOTN/NE2_50M_SR.tif") {
  library(st)
  library(raster)
  library(glatos)
  
  #Get the extent
  minLat = min(detection_extract$latitude) + 5
  minLon = min(detection_extract$longitude) + 5
  maxLat = max(detection_extract$latitude) + 5
  maxLon = max(detection_extract$longitude) + 5
  
  #Crop the range shapefile
  shapefile_crop <- st_crop(blue_shark_shp,  xmin=minLon, ymin=minLat, xmax=maxLon, ymax=maxLat)
  
  #Generate a transition layer
  transition_layer <- glatos::make_transition2(shapefile_crop)
  
  #Crop the world raster
  if(file.exists(worldimage)) {
    world_raster <- raster(worldimage)
  }
  else {
    world_raster <- worldimage
  }
  
  world_raster_crop <- crop(world_raster, shapefile_crop)
  
  return(
    list(shapefile_crop, transition_layer, world_raster_crop)
    )
  
}