#If you need the installs.
#install.packages('readr')
#install.packages('tidyverse')
#install.packages('sf')
#install.packages('sp')
#install.packages('raster')
#install.packages('stars')
#devtools::install_github('ocean-tracking-network/remora@get_data_qc', force=TRUE)

library(readr)
library(tidyverse)
library(remora)
library(sf)
library(sp)
library(raster)
library(stars)
library(glatos)
library(utils)

setwd('YOUR/PATH/TO/remora')

download.file("https://members.oceantrack.org/data/share/testdataotn.zip/@@download/file/testDataOTN.zip", "./testDataOTN.zip")
unzip("testDataOTN.zip")

#IMOS test data, as something to reference against if you want it.
imos_files <- list(det = system.file(file.path("test_data","IMOS_detections.csv"), package = "remora"),
                   rmeta = system.file(file.path("test_data","IMOS_receiver_deployment_metadata.csv"),
                                       package = "remora"),
                   tmeta = system.file(file.path("test_data","IMOS_transmitter_deployment_metadata.csv"),
                                       package = "remora"),
                   meas = system.file(file.path("test_data","IMOS_animal_measurements.csv"),
                                      package = "remora"))

otn_test_data <- readr::read_csv("./testDataOTN/qc_princess.csv") #Put your path to your test file here. 
otn_mapped_test <- otn_imos_column_map(otn_test_data)
#If you want to check your work. 
View(otn_mapped_test)

#The above code isn't meant to be run on its own just yet, the ideal is that you can pass it to QC without having to manually map it. 
otn_files <- list(det = "./testDataOTN/qc_princess.csv") #Put your path to your files here

#The QC functions rely on having shapefiles for distributions and study areas to calculate distances. 
#We've got to get a shapefile for the Blue Shark test data, one is included here for sharks but for alternative data you will need your own appropriate one.
#We got ours from IUCN so maybe start there!
shark_shp <- sf::st_read("./testDataOTN/SHARKS_RAYS_CHIMAERAS/SHARKS_RAYS_CHIMAERAS.shp")
#We're using the binomial name and bounding box that befits our species and area but feel free to sub in your own when you work with other datasets.
blue_shark_shp <- shark_shp[shark_shp$binomial == 'Prionace glauca',]
blue_shark_crop <- sf::st_crop(blue_shark_shp,  xmin=-68.4, ymin=42.82, xmax=-60.53, ymax=45.0)

#Make a transition layer for later...
shark_transition <- glatos::make_transition2(blue_shark_crop)
shark_tr <- shark_transition$transition

#And also a spatial polygon that we can use later. 
blue_shark_spatial <- sf::as_Spatial(blue_shark_crop)

#We also need a raster for the ocean. We'll load this from a mid-resolution tif file, for testing purposes. 
world_raster <- raster::raster("./testDataOTN/NE2_50M_SR.tif")
#And crop it based on our cropped blue shark extent. 
world_raster_sub <- raster::crop(world_raster, blue_shark_crop)

#These are the available tests at time of writing. Detection Distribution isn't working yet and so we have commented it out. 
tests_vector <-  c("FDA_QC",
                   "Velocity_QC",
                   "Distance_QC",
#                   "DetectionDistribution_QC", #
                   "DistanceRelease_QC",
                   "ReleaseDate_QC",
                   "ReleaseLocation_QC",
                   "Detection_QC")

#In a perfect world, when you run this code, you will get output with QC attached. 
otn_test_tag_qc <- remora::runQC(otn_files, data_format = "otn", tests_vector = tests_vector, .parallel = FALSE, .progress = TRUE)

qc_shapes_test <- get_qc_shapes(blue_shark_shp, otn_test_data)
