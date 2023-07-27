#Function made by Bruce Delo but the underlying code was developed by Jessica Castellanos (UGuelph). Many of the comments are hers too.

createPolygon <- function(occurrenceFile, 
                          fraction = 0.70, 
                          buff = 1000, 
                          partCount = 20,  
                          coordHeaders = c("decimalLongitude", "decimalLatitude"), 
                          clipToCoast = "aquatic",
                          returnWhole = FALSE) {
  library(tidyverse)
  library(rangeBuilder)
  library(sf)
  
  #Read in the occurrence CSV. 
  occurrence <- read_csv(occurrenceFile)
  
  #Using the R package rangeBuilder to generate an alpha hull polygon which defines a concave hull or boundary around a set of points in two or three dimensions. 
  #The alpha hull polygon is a generalization of the convex hull, allowing for the creation of concave regions. 
  #The function getDynamicAlphaHull integrates the function ahull, which calculates the α-convex hull of a given sample of points in the plane for α >0. 
  #It also includes other parameters of relevance, like the minimum fraction of occurrences that must be included in a polygon, the maximum number of disjunct polygons allowed, and setting a buffer zone.
  
  if("flags" %in% colnames(occurrence)) {
    occurrence <- occurrence %>% filter(is.na(flags))
  }
  
  #Using the function getDynamicAlphaHull to generate a polygon based on occurrence data. In this case, the parameters represent:
  #fraction: the minimum fraction of occurrences that must be included in the polygon - 70%
  #buff: buffering distance in meters - 1000. This should be adjusted considering OBIS buffering
  #partCount: the maximum number of disjunct polygons that are allowed. I set this to a high number to allow for global distribution and multiple polygons.
  #clipToCoast: Either "no" (no clipping), "terrestrial" (only terrestrial part of the range is kept) or "aquatic" (only non-terrestrial part is clipped).
  polygon <- getDynamicAlphaHull(occurrence, fraction = fraction, buff = buff, partCount = partCount, coordHeaders = coordHeaders, clipToCoast = clipToCoast)
  
  if(returnWhole == TRUE) {
    return(polygon)
  }
  else{
    return(polygon[[1]])
  }
}
