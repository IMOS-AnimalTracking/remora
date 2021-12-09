## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
# Set workspace
library(data.table); library(dplyr); library(sf); library(leaflet); library(RColorBrewer); library(classInt); library(ncdf4); library(lubridate)


## ---- warning=FALSE,message=FALSE---------------------------------------------
library(tidyverse)
library(remora)

imos_variables()

## ---- eval = TRUE, message=FALSE, warning=FALSE-------------------------------
## Example dataset that has undergone quality control using the `runQC()` function
data("TownsvilleReefQC")

## Un-nest the output and retain detections flagged as 'valid' and 'likely valid' (Detection_QC 1 and 2)
qc_data <- TownsvilleReefQC %>% 
  tidyr::unnest(QC) %>% 
  dplyr::filter(Detection_QC %in% c(1,2)) 


## ---- eval=FALSE, message=FALSE, warning=FALSE, fig.align='center'------------
#  library(ggplot2)
#  library(ggspatial)
#  
#  qc_data %>%
#    group_by(transmitter_id, station_name, installation_name, receiver_deployment_longitude, receiver_deployment_latitude) %>%
#    dplyr::summarise(num_det = n()) %>%
#    ggplot() +
#    annotation_map_tile('cartolight') +
#    geom_spatial_point(aes(x = receiver_deployment_longitude,
#                           y = receiver_deployment_latitude,
#                           size = num_det,
#                           color = installation_name),
#                       crs = 4326) +
#    facet_wrap(~transmitter_id,nrow=3) +
#    theme(legend.position="bottom")

## ---- eval=FALSE, message=FALSE, warning=FALSE, fig.align='center'------------
#  qc_data %>%
#    mutate(date = as.Date(detection_datetime)) %>%
#    group_by(transmitter_id, date, installation_name) %>%
#    dplyr::summarise(num_det = n()) %>%
#    ggplot(aes(x = date, y = transmitter_id, color = installation_name, size = num_det)) +
#    geom_point() +
#    theme_bw()

## ---- eval=FALSE, message=TRUE, warning=FALSE---------------------------------
#  # Generate a table containing IMOS moorings metadata for only those moorings with temperature data
#  moorT <- mooringTable(sensorType="temperature")
#  
#  # PLot the location of the moorings with associated metadata
#  library(leaflet)
#  leaflet() %>%
#    addProviderTiles("CartoDB.Positron") %>%
#    addMarkers(lng = moorT$longitude, lat = moorT$latitude,
#               popup = paste("Site code", moorT$site_code,"<br>",
#                             "URL:", moorT$url, "<br>",
#                             "Standard names:", moorT$standard_names, "<br>",
#                             "Coverage start:", moorT$time_coverage_start, "<br>",
#                             "Coverage end:", moorT$time_coverage_end))

## ---- eval=FALSE, message=TRUE, warning=FALSE---------------------------------
#  #Extract the nearest mooring to each station where a tag was detected
#  det_dist <- getDistance(trackingData = qc_data,
#                          moorLocations = moorT,
#                          X = "receiver_deployment_longitude",
#                          Y = "receiver_deployment_latitude",
#                          datetime = "detection_datetime")

## ---- eval=FALSE, message=FALSE, warning=FALSE--------------------------------
#  # Which moorings have overlapping data with detections?
#  mooring_overlap <- getOverlap(det_dist)
#  mooring_overlap

## ---- eval=FALSE, message=FALSE, warning=FALSE--------------------------------
#  # Filter for only those moorings which are closest to receivers and also have overlapping detection intervals
#  moorT_new <- moorT %>%
#    filter(site_code %in% mooring_overlap$moor_site_code)
#  
#  #Summarise dataset so only a single row per receiver station
#  qs_stat <- qc_data %>%
#    group_by(station_name, installation_name,
#             receiver_deployment_longitude,
#             receiver_deployment_latitude) %>%
#    dplyr::summarise(num_det = n(),
#              first_detection = min(detection_datetime),
#              last_detection = max(detection_datetime))
#  
#  # make palette for number of detections at stations
#  library(viridisLite)
#  domain <- range(qs_stat$num_det) # get domain of numeric data for colour scalse
#  pal <- colorNumeric(palette = viridis(100), domain = domain)
#  
#  # Draw the map
#  leaflet() %>%
#    addProviderTiles("CartoDB.Positron") %>%
#    addMarkers(lng = moorT_new$longitude, lat = moorT_new$latitude,
#               popup = paste("Site code", moorT_new$site_code,"<br>",
#                             "URL:", moorT_new$url, "<br>",
#                             "Standard names:", moorT_new$standard_names, "<br>",
#                             "Start:", moorT_new$time_coverage_start, "<br>",
#                             "End:", moorT_new$time_coverage_end)) %>%
#    addCircleMarkers(data=qs_stat,
#                     lng = qs_stat$receiver_deployment_longitude,
#                     lat = qs_stat$receiver_deployment_latitude,
#                     radius = 6,
#                     color=~pal(num_det),
#                     stroke=FALSE,
#                     fillOpacity=0.5,
#                     popup = paste("Station name", qs_stat$station_name,"<br>",
#                                   "Installation name:", qs_stat$installation_name, "<br>",
#                                   "Tag detections:", qs_stat$num_det, "<br>",
#                                   "First tag detection:",qs_stat$first_detection, "<br>",
#                                   "Last tag detection:",qs_stat$last_detection))

## ---- eval=FALSE, message=TRUE, warning=FALSE---------------------------------
#  ## Creates vector of moorings that we want to download
#  moorIDs <- unique(mooring_overlap$moor_site_code)
#  
#  ## Download each net cdf from the Thredds server to a specified folder
#  ## set fromWeb = FALSE if loading from an existing folder within the working directory
#  moorDat.l <- mooringDownload(moor_site_codes = moorIDs,
#                               sensorType="temperature",
#                               fromWeb = TRUE,
#                               file_loc="imos.cache/moor/temperature",
#                               itimeout=240)
#  names(moorDat.l) # What moorings datasets do we have in memory
#  #[1] "GBRMYR" "GBRPPS" "NRSYON"

## ---- eval=FALSE, message=FALSE, warning=FALSE, fig.width=10, fig.height=10----
#  plotDT(moorData=moorDat.l$GBRPPS, # have GBRMYR, GBRPPS and NRSYON available
#         moorName="GBRPPS",
#         dateStart="2013-11-01",
#         dateEnd="2014-02-01",
#         varName="temperature",
#         trackingData=det_dist,
#         speciesID="Carcharhinus leucas",
#         IDtype="species_scientific_name",
#         detStart="2013-11-01",
#         detEnd="2014-02-01")
#  

## ---- eval=FALSE, message=FALSE, warning=FALSE, fig.width=10, fig.height=10----
#  plotDat.l <- mooringDownload(moor_site_codes = "GBRPPS",
#                               fromWeb = TRUE, # Download the netCDF. If already downloaded, set as FALSE
#                               sensorType="velocity", # sensor data to download
#                               file_loc="imos.cache/moor/velocity", # Location where the netCDF is stored/saved
#                               itimeout=240)
#  
#  # plot the moorings data
#  plotDT(moorData=plotDat.l$GBRPPS,
#         moorName="GBRPPS",
#         dateStart="2013-11-01",
#         dateEnd="2014-02-01",
#         varName = "vcur",
#         trackingData=det_dist,
#         speciesID="Carcharhinus leucas",
#         IDtype="species_scientific_name",
#         detStart="2013-11-01",
#         detEnd="2014-02-01")

## ---- eval=FALSE, message=FALSE, warning=FALSE--------------------------------
#  data_with_mooring_sst_all <- extractMoor(trackingData = det_dist,
#                                           file_loc="imos.cache/moor/temperature",
#                                           sensorType = "temperature",
#                                           timeMaxh = Inf,
#                                           distMaxkm = Inf)

## ---- eval = FALSE, warning=FALSE, message=FALSE------------------------------
#  data_with_mooring_sst_all # By default the output is a nested tibble
#  
#  # Unnest tibble to reveal the first 10 rows of the data
#  data_with_mooring_sst_all %>%
#    tidyr::unnest(cols = c(data))

## ---- eval=FALSE, message=TRUE, warning=FALSE---------------------------------
#  # Run the same function again with time and distance thresholds
#  # and when multiple sensors are available return the shallowest value at that time stamp
#  data_with_mooring_sst_shallow <- extractMoor(trackingData = det_dist,
#                                               file_loc="imos.cache/moor/temperature",
#                                               sensorType = "temperature",
#                                               timeMaxh = 24,
#                                               distMaxkm = 50,
#                                               targetDepthm=0,
#                                               scalc="min")
#  
#  data_with_mooring_sst_shallow

## ---- eval = FALSE, message=FALSE, warning=FALSE------------------------------
#  summarised_data_id <-
#    data_with_mooring_sst_shallow %>%
#    tidyr::unnest(cols = c(data)) %>%
#    mutate(date = as.Date(detection_datetime)) %>%
#    group_by(transmitter_id, date) %>%
#    dplyr::summarise(num_det = n(),
#              mean_temperature = mean(moor_sea_temp, na.rm = T))
#  
#  library(ggplot2)
#  ggplot(summarised_data_id, aes(x = date, y = transmitter_id, size = num_det, color = mean_temperature)) +
#    geom_point() +
#    scale_color_viridis_c() +
#    labs(subtitle = "In-situ sea water temperature (°C)") +
#    theme_bw()

## ---- eval = FALSE, message=FALSE, warning=FALSE------------------------------
#  summarised_data_id <-
#    data_with_mooring_sst_shallow %>%
#    tidyr::unnest(cols = c(data)) %>%
#    mutate(date = as.Date(detection_datetime)) %>%
#    group_by(installation_name, date) %>%
#    dplyr::summarise(num_det = n(),
#              mean_temperature = mean(moor_sea_temp, na.rm = T)) #%>% drop_na(mean_temperature)
#  
#  
#  ggplot(summarised_data_id, aes(x=date,y=installation_name,
#                                 size=num_det,color=mean_temperature)) +
#    geom_point() +
#    scale_color_viridis_c() +
#    labs(subtitle = "In-situ sea water temperature (°C)") +
#    theme_bw()

## ---- eval = FALSE, message=FALSE, warning=FALSE------------------------------
#  library(hrbrthemes)
#  # First plot the temperature data
#  data_with_mooring_sst_shallow %>%
#    tidyr::unnest(cols = c(data)) %>%
#    ggplot(aes(x=moor_sea_temp)) +
#    geom_histogram(binwidth=1,fill="#69b3a2",color="#e9ecef",alpha=0.9) +
#    facet_wrap(~installation_name,scales="free_y",drop=TRUE) +
#    ggtitle("In-situ sea water temperatures when bull sharks present (°C)") +
#    #theme_ipsum() +
#    theme(
#      plot.title = element_text(size=15)
#    )

## ---- eval = FALSE, message=FALSE, warning=FALSE------------------------------
#  # Next plot the depth data
#  data_with_mooring_sst_shallow %>%
#    tidyr::unnest(cols = c(data)) %>%
#    drop_na(moor_sea_temp) %>%
#    ggplot(aes(x=moor_depth)) +
#    geom_histogram(binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
#    ggtitle("Depths of sea water temperature readings (m)") +
#    theme_ipsum() +
#    theme(
#      plot.title = element_text(size=15)
#    )

