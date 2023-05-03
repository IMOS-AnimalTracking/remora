##' @title generate shapefiles and transition layers
##' 
##' @description generate shapefiles and transition layers for QC based on user-supplied parameters
##' 
##' @param detection_extract ...
##' @param shapefile ...
##' @param worldimage ...
##' 
##' @return a list of cropped shapefile, transition layer, and world raster
##' 
##' @importFrom sf st_crop st_bbox
##' @importFrom raster raster crop
##' @importFrom glatos make_transition2
##' 
##' @keywords internal

get_qc_shapes <- function(detection_extract, shapefile, worldimage = "./testDataOTN/NE2_50M_SR.tif") {

  if(!inherits(detection_extract, "sf")) {
    minLat = min(detection_extract$latitude) - 5
    minLon = min(detection_extract$longitude) - 5
    maxLat = max(detection_extract$latitude) + 5
    maxLon = max(detection_extract$longitude) + 5
    #Crop the range shapefile
    shapefile_crop <- st_crop(shapefile,  xmin=minLon, ymin=minLat, xmax=maxLon, ymax=maxLat)
  } else {
    ext <- st_bbox(detection_extract)
    ext[1] <- ext[1] + 5 * sign(ext[1])
    ext[2] <- ext[2] + 5 * sign(ext[2])
    ext[3] <- ext[3] + 5 * sign(ext[3])
    ext[4] <- ext[4] + 5 * sign(ext[4])
    #Crop the range shapefile
    shapefile_crop <- st_crop(shapefile,  ext)
  }

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