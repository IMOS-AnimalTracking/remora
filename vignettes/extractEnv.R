## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
## working when developing vignette
require(tidyverse, quietly = TRUE)
require(remora, quietly = TRUE)

## load extracted dataset for faster running of vignette
data("data_with_sst_and_current", envir = environment())

## ---- warning=FALSE-----------------------------------------------------------
library(remora)

imos_variables()

## ---- eval = FALSE, message=FALSE, warning=FALSE------------------------------
#  library(tidyverse)
#  library(raster)
#  library(ggspatial)
#  
#  ## Example dataset that has undergone quality control using the `runQC()` function
#  data("TownsvilleReefQC")
#  
#  ## Only retain detections flagged as 'valid' and 'likely valid' (Detection_QC 1 and 2)
#  qc_data <-
#    TownsvilleReefQC %>%
#    tidyr::unnest(cols = QC) %>%
#    dplyr::ungroup() %>%
#    filter(Detection_QC %in% c(1,2))
#  

## ---- eval=FALSE, message=FALSE, warning=FALSE, fig.align='center'------------
#  
#  qc_data %>%
#    group_by(transmitter_id, station_name, installation_name, receiver_deployment_longitude, receiver_deployment_latitude) %>%
#    summarise(num_det = n()) %>%
#    ggplot() +
#    annotation_map_tile('cartolight') +
#    geom_spatial_point(aes(x = receiver_deployment_longitude, y = receiver_deployment_latitude,
#                           size = num_det, color = installation_name), crs = 4326) +
#    facet_wrap(~transmitter_id, ncol = 2) +
#    labs(x = "Longitude", y = "Latitude", color = "Installation Name" , size = "Number of\nDetections") +
#    theme_bw()
#  

## ---- eval=FALSE, message=FALSE, warning=FALSE, fig.align='center'------------
#  
#  qc_data %>%
#    mutate(date = as.Date(detection_datetime)) %>%
#    group_by(transmitter_id, date, installation_name) %>%
#    summarise(num_det = n()) %>%
#    ggplot(aes(x = date, y = transmitter_id, color = installation_name, size = num_det)) +
#    labs(x = "Date", y = NULL, color = "Installation Name", size = "Number of\nDetections") +
#    geom_point() +
#    theme_bw()
#  

## ---- eval=FALSE, message=TRUE, warning=FALSE---------------------------------
#  
#  data_with_sst <-
#    extractEnv(df = qc_data,
#               X = "receiver_deployment_longitude",
#               Y = "receiver_deployment_latitude",
#               datetime = "detection_datetime",
#               env_var = "rs_sst_interpolated",  ## Currently only a single variable can be called at a time
#               cache_layers = TRUE,
#               crop_layers = TRUE,
#               fill_gaps = TRUE,
#               full_timeperiod = FALSE,
#               folder_name = "test",
#               .parallel = TRUE)
#  

## ---- eval=FALSE, message=TRUE, warning=FALSE---------------------------------
#  
#  data_with_sst_and_current <-
#    data_with_sst %>%
#    extractEnv(X = "receiver_deployment_longitude",
#               Y = "receiver_deployment_latitude",
#               datetime = "detection_datetime",
#               env_var = "rs_current",
#               cache_layers = TRUE,
#               crop_layers = TRUE,
#               fill_gaps = TRUE,
#               full_timeperiod = FALSE,
#               folder_name = "test",
#               .parallel = TRUE)
#  

## ---- warning=FALSE, message=FALSE--------------------------------------------
data_with_sst_and_current

## ---- eval = FALSE, message=FALSE, warning=FALSE------------------------------
#  
#  summarised_data <-
#    data_with_sst_and_current %>%
#    mutate(date = as.Date(detection_datetime)) %>%
#    group_by(transmitter_id, date) %>%
#    summarise(num_det = n(),
#              mean_sst = mean(rs_sst_interpolated, na.rm = T),
#              mean_current_velocity = mean(rs_current_velocity, na.rm = T),
#              mean_current_bearing = mean(rs_current_bearing, na.rm = T))
#  
#  ggplot(summarised_data, aes(x = date, y = transmitter_id, size = num_det, color = mean_sst)) +
#    geom_point() +
#    scale_color_viridis_c() +
#    labs(subtitle = "Interpolated sea surface temperature", x = "Date",
#         y = NULL, color = "SST (˚C)", size = "Number of\nDetections") +
#    theme_bw()
#  

## ---- eval=FALSE, message=FALSE, warning=FALSE--------------------------------
#  
#  ggplot(summarised_data) +
#    geom_point(aes(x = date, y = transmitter_id, color = mean_current_velocity,
#                   size = num_det)) +
#    geom_spoke(aes(x = date, y = transmitter_id, angle = mean_current_bearing, radius = mean_current_velocity),
#               arrow = arrow(length = unit(0.05, 'inches')), color = "grey", lwd = 0.1) +
#    labs(subtitle = "Direction and velocity of modeled surface currents", x = "Date",
#         y = NULL, color = "Current\nVelocity (ms-1)", size = "Number of\nDetections") +
#    scale_color_viridis_c(option = "B") +
#    theme_bw()
#  

## ---- eval=FALSE, message=FALSE, warning=FALSE--------------------------------
#  
#  ## Detection data summary
#  det_summary <-
#    qc_data %>%
#    group_by(station_name, lon = receiver_deployment_longitude, lat = receiver_deployment_latitude) %>%
#    summarise()
#  
#  ## Sea surface temperature
#  sst_raster <- stack("imos.cache/rs variables/test/rs_sst_interpolated.grd")
#  
#  sst_df <-
#    as.data.frame(sst_raster[[1:12]], xy = T) %>%
#    pivot_longer(-c(1,2))
#  
#  ggplot(sst_df) +
#    geom_tile(aes(x, y, fill = value)) +
#    scale_fill_viridis_c() +
#    geom_point(data = det_summary, aes(x = lon, y = lat), size = 0.5, color = "red", inherit.aes = F) +
#    labs(fill = "Interpolated\nSST (˚C)", x = "Longitude", y = "Latitude") +
#    coord_equal() +
#    facet_wrap(~name, nrow = 4) +
#    theme_bw()
#  

## ---- eval=FALSE, message=FALSE, warning=FALSE--------------------------------
#  ## Current
#  library(metR)
#  library(sf)
#  
#  ## read in vcur and ucur spatial data
#  vcur_raster <- stack("imos.cache/rs variables/test/rs_vcur.grd")[[1:4]]
#  vcur_raster[is.na(values(vcur_raster))] <- 0
#  ucur_raster <- stack("imos.cache/rs variables/test/rs_ucur.grd")[[1:4]]
#  ucur_raster[is.na(values(ucur_raster))] <- 0
#  
#  ## calculate velocity (ms-1)
#  vel_raster <- sqrt(vcur_raster^2 + ucur_raster^2)
#  
#  ## extract values from raster stacks
#  uv <-
#    as.data.frame(vcur_raster, xy = T) %>%
#    pivot_longer(-c(1,2), names_to = "date", values_to = "v") %>%
#    left_join(as.data.frame(ucur_raster, xy = T) %>%
#                pivot_longer(-c(1,2), names_to = "date", values_to = "u")) %>%
#    left_join(as.data.frame(vel_raster, xy = T) %>%
#                pivot_longer(-c(1,2), names_to = "date", values_to = "vel")) %>%
#    mutate(date = str_replace_all(str_sub(date, start = 2, end = 11), pattern = "[.]", replacement = "-"))
#  
#  ggplot() +
#    geom_contour_fill(data = uv, aes(x = x, y = y, z = vel, fill = stat(level)),
#                      binwidth = 0.05, alpha = 0.7, inherit.aes = F) +
#    geom_streamline(data = uv, skip = 0, size = 0.05, L = 1, res = 5, jitter = 5, lineend = "round",
#                    aes(x = x, y = y, dx = u, dy = v), color = "white") +
#    scale_fill_viridis_d(option = "B", name = "Current Velocity (m/s)") +
#    geom_point(data = det_summary, aes(x = lon, y = lat), size = 0.5, color = "red", inherit.aes = F) +
#    facet_wrap(~ date, nrow = 2) +
#    labs(subtitle = "Modeled surface currents", x = "Longitude", y = "Latitude") +
#    theme_bw()
#  

