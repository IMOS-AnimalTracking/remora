library(tidyverse)
library(rangeBuilder)
library(sf)

cobia <- read_csv("/Users/bruce/Downloads/r_canadum_OBIS.csv")

#Using the R package rangeBuilder to generate an alpha hull polygon which defines a concave hull or boundary around a set of points in two or three dimensions. The alpha hull polygon is a generalization of the convex hull, allowing for the creation of concave regions. The function getDynamicAlphaHull integrates the function ahull, which calculates the α-convex hull of a given sample of points in the plane for α >0. It also includes other parameters of relevance, like the minimum fraction of occurrences that must be included in a polygon, the maximum number of disjunct polygons allowed, and setting a buffer zone.
#Reading OBIS occurrence data. This is a list where each element has the occurrence data for each species.
spsOcurren <- readRDS("spsOcurren.RData")

#Test using Carcharhinus limbatus
c_limbatus <- spsOcurren[[128]]

#Checking records with flags and filtering them out
unique(c_limbatus$flags)
unique(cobia$flags)

#Remove all records with flags. It includes: ON_LAND, NO_DEPTH, DEPTH_EXCEEDS_BATH, MIN_DEPTH_EXCEEDS_MAX
c_limbatus2 <- c_limbatus %>%
  filter(is.na(flags))

cobia2 <- cobia %>% filter(is.na(flags))

#Using the function getDynamicAlphaHull to generate a polygon based on occurrence data. In this case, the parameters represent:
#fraction: the minimum fraction of occurrences that must be included in the polygon - 70%
#buff: buffering distance in meters - 1000. This should be adjusted considering OBIS buffering
#partCount: the maximum number of disjunct polygons that are allowed. I set this to a high number to allow for global distribution and multiple polygons.
#clipToCoast: Either "no" (no clipping), "terrestrial" (only terrestrial part of the range is kept) or "aquatic" (only non-terrestrial part is clipped).
sps1Poly <- getDynamicAlphaHull(c_limbatus2, fraction = 0.70, buff = 1000, partCount = 20, coordHeaders = c("decimalLongitude", "decimalLatitude"), clipToCoast = "aquatic")
sps1Poly <- getDynamicAlphaHull(cobia2, fraction = 0.70, buff = 1000, partCount = 20, coordHeaders = c("decimalLongitude", "decimalLatitude"), clipToCoast = "aquatic")


#Plot the polygons
plot(sps1Poly[[1]], col=transparentColor('dark green', 0.5), border = NA)

#Plot the points
points(c_limbatus2[,c('decimalLongitude','decimalLatitude')], cex = 0.5, pch = 3)
points(cobia2[,c('decimalLongitude','decimalLatitude')], cex = 0.5, pch = 3)

#to add a basic coastline, you can use the internal map
world <- rangeBuilder:::loadWorldMap()

#add map
plot(world, add = TRUE, lwd = 0.5)
class(sps1Poly[[1]])

#Save as sf object
c_limbatus_poly <- st_as_sf(sps1Poly[[1]])