##' @title produce interactive leaflet maps showing the occurrence of QC'd detections per species
##'
##' @description \code{plotQCint()} QC'd detections colour-coded by their
##' assessed validity status, overlaid on species expert distribution extent
##'
##' @param x a remora output object with \code{class(remora_QC)}.
##' @param path path to save map(s). Options are: `NULL` (default) - renders to 
##' a viewer window (RStudio only); `wb` - renders in default web browser; 
##' or a valid file path - map saved as a self-contained .html file.
##' @param pal a `brewer.pal` palette name as a quoted character string. Use 
##' `RColorBrewer::display.brewer.all()` to see choices.
##' @param revpal reverse order of colour palette.
##' 
##' @return produces interactive leaflet maps of species expert distribution and
##' location of QC'd detections
##'
##' @examples
##' ## example QC'd data
##' data(TownsvilleReefQC)
##' 
##' ## save plot as an .html file to the working directory
##' plotQC(TownsvilleReefQC, path = ".")
##' 
##' ## clean up
##' system("rm *_QCmap.html")
##' 
##' 
##' @importFrom leaflet leaflet addMarkers addCircleMarkers fitBounds addLegend 
##' @importFrom leaflet addProviderTiles addLayersControl addPolygons layersControlOptions
##' @importFrom leaflet markerClusterOptions
##' @importFrom RColorBrewer brewer.pal
##' @importFrom htmlwidgets saveWidget
##' @importFrom dplyr '%>%' summarise left_join group_by bind_rows distinct count
##' @importFrom grDevices extendrange
##' @importFrom utils browseURL
##'
##' @export

plotQC <- function(x, path = NULL, pal = "PuOr", revpal = TRUE) {

  if(!inherits(x, "remora_QC")) 
    stop("\033[31;1mx must be a nested tibble with class `remora_QC`\033[0m")
  
  QCdata <- bind_rows(x$QC)

  species <- QCdata %>% select(CAAB_species_id, 
                               species_scientific_name, 
                               species_common_name) %>%
    distinct(.keep_all = TRUE)
	
	for (i in 1:nrow(species)){
		expert_shp <-
		  try(get_expert_distribution_shp_CAAB(CAAB_species_id = species$CAAB_species_id[i], 
		                                       spe = species$species_scientific_name[i]), 
		      silent = TRUE)

		if(is.null(class(expert_shp))) {
			print(paste('No expert distribution shapefile available for species ',
			            caab_dump$COMMON_NAME[which(caab_dump$SPCODE == CAAB_id)],
			            ' (',
			            caab_dump$SCIENTIFIC_NAME[which(caab_dump$SPCODE == CAAB_id)],
			            ', ',
			            caab_dump$AUTHORITY[which(caab_dump$SPCODE == CAAB_id)], ')',
			            sep = ''))
		} else if (inherits(expert_shp, "try-error")) {
		  cat("\033[0;34mCould not download shapefile, mapping without species expert distribution\033[0m")
		  expert_shp <- NULL
		}

		data <- subset(QCdata, CAAB_species_id == species$CAAB_species_id[i])
		releases <- with(data, unique(data.frame(transmitter_id, 
		                                         tag_id,
		                                         transmitter_deployment_id,
		                                         transmitter_deployment_longitude,
		                                         transmitter_deployment_latitude,
		                                         ReleaseLocation_QC)))
		
		 data <- data[, c('transmitter_id',
		                  'tag_id',
		                  'transmitter_deployment_id',
		                  'detection_datetime',
		                 'receiver_name',
		                 'station_name',
		                 'installation_name',
		                 'receiver_deployment_longitude',
		                 'receiver_deployment_latitude',
		                 'Detection_QC')]
	 
		nT <- data %>% group_by(station_name, 
		                         installation_name, 
		                         receiver_deployment_longitude, 
		                         receiver_deployment_latitude, 
		                         Detection_QC, 
		                        transmitter_id) %>%
		  summarise(start_dt = min(detection_datetime),
		            end_dt = max(detection_datetime)) %>%
		  ungroup() %>%
		  group_by(station_name, 
		           installation_name, 
		           receiver_deployment_longitude, 
		           receiver_deployment_latitude, 
		           Detection_QC) %>%
		  summarise(nT = length(unique(transmitter_id)),
		            transmitter_ids = paste(unique(transmitter_id), collapse = ", "),
		            start_dt = paste(start_dt, collapse = ", "),
		            end_dt = paste(end_dt, collapse = ", "))
		  
		dataC <- count(data, 
		               station_name,
		               installation_name,
		               receiver_deployment_longitude,
		               receiver_deployment_latitude,
		               Detection_QC)

		binned_detects <- data.frame(dataC$n,
		                             bin=cut(dataC$n,
		                                     c(0, 10, 100, 1000, 10000, 100000),
		                                     include.lowest=TRUE))
		
		dataC$binned_detections <- as.numeric(binned_detects$bin)
		
		dataC <- left_join(dataC, nT, by = c("station_name", 
		                            "installation_name", 
		                            "receiver_deployment_longitude", 
		                            "receiver_deployment_latitude", 
		                            "Detection_QC"))
    
		bb <- with(data, c(extendrange(c(receiver_deployment_longitude,releases$transmitter_deployment_longitude), f=0.1),
		                   extendrange(c(receiver_deployment_latitude, releases$transmitter_deployment_latitude), f=0.1)
		))

		if (!is.null(class(expert_shp))) {
		  map <- leaflet(data = expert_shp) %>%
		    fitBounds(lng1 = bb[1], lat1 = bb[3], lng2 = bb[2], lat2 = bb[4]) %>%
		    addPolygons(weight = 0.5, 
		                group = "Species distribution",
		                color = "#000000",
		                fillColor = "#f7fbff",
		                fillOpacity = 0.6) %>%
		    addProviderTiles("CartoDB.Positron", group = "Default") %>%
		    addProviderTiles("Esri.WorldImagery", group = "ESRI Satellite") %>%
		    addProviderTiles("Esri.OceanBasemap", group = "ESRI Bathymetry")
		  
		} else if (any(is.null(expert_shp), is.null(class(expert_shp)))) {
		  map <- leaflet() %>%
		    fitBounds(lng1 = bb[1], lat1 = bb[3], lng2 = bb[2], lat2 = bb[4]) %>%
		    addProviderTiles("CartoDB.Positron", group = "Default")
		}
    if(revpal) cpal <- rev(brewer.pal(n=4, pal))
		else cpal <- brewer.pal(n=4, pal)
    
		## Plot likely valid detections - render first so valid overlay these
		if (sum(data$Detection_QC == 2, na.rm = TRUE) > 0) {
		  dsub <- subset(dataC, Detection_QC == 2)
		  map <- map %>% 
		    addCircleMarkers(lng = dsub$receiver_deployment_longitude, 
		                     lat = dsub$receiver_deployment_latitude,
		                     group  = "Likely valid",
		                     radius = dsub$binned_detections * 5,
		                     weight = 0.25,
		                     color = cpal[2],
		                     opacity = 1,
		                     fillColor = cpal[2],
		                     fillOpacity = 0.65,
		                     popup = paste("<b>Quality Controlled Detections</b>", "<br>",
		                                   "Station name:", dsub$station_name, "<br>",
		                                   "Installation name:", dsub$installation_name, "<br>",
		                                   "Longitude:", dsub$receiver_deployment_longitude, "<br>",
		                                   "Latitude:", dsub$receiver_deployment_latitude, "<br>",
		                                   "Number of detections:", dsub$n, "<br>",
		                                   "Number of tags detected:", dsub$nT, "<br>",
		                                   "Transmitter ID's:", dsub$transmitter_ids, "<br>",
		                                   "First date by ID:", dsub$start_dt, "<br>",
		                                   "Last date by ID:", dsub$end_dt, "<br>",
		                                   "QC flag:", "<b>Likely valid</b>"))
		  
		}
		
		## Plot valid detections
		if (sum(data$Detection_QC == 1, na.rm = TRUE) > 0) {
		  dsub <- subset(dataC, Detection_QC == 1)
		  map <- map %>% 
		    addCircleMarkers(lng = dsub$receiver_deployment_longitude, 
		               lat = dsub$receiver_deployment_latitude,
		               group = "Valid",
		               radius = dsub$binned_detections * 5,
		               weight = 0.25,
		               color = cpal[1],
		               opacity = 1,
		               fillColor = cpal[1],
		               fillOpacity = 0.65,
		               popup = paste("<b>Quality Controlled Detections</b>", "<br>",
		                             "Station name:", dsub$station_name, "<br>",
		                             "Installation name:", dsub$installation_name, "<br>",
		                             "Longitude:", dsub$receiver_deployment_longitude, "<br>",
		                             "Latitude:", dsub$receiver_deployment_latitude, "<br>",
		                             "Number of detections:", dsub$n, "<br>",
		                             "Number of tags detected:", dsub$nT, "<br>",
		                             "Transmitter ID's:", dsub$transmitter_ids, "<br>",
		                             "First date by ID:", dsub$start_dt, "<br>",
		                             "Last date by ID:", dsub$end_dt, "<br>",
		                             "QC flag:", "<b>Valid</b>"))
		}

			## Plot valid detections
			if (sum(data$Detection_QC == 3, na.rm = TRUE) > 0) {
			  dsub <- subset(dataC, Detection_QC == 3)
			  map <- map %>% 
			    addCircleMarkers(lng = dsub$receiver_deployment_longitude, 
			                     lat = dsub$receiver_deployment_latitude,
			                     group = "Likely invalid",
			                     radius = dsub$binned_detections * 5,
			                     weight = 0.25,
			                     color = cpal[3],
			                     opacity = 1,
			                     fillColor = cpal[3],
			                     fillOpacity = 0.65,
			                     popup = paste("<b>Quality Controlled Detections</b>", "<br>",
			                                   "Station name:", dsub$station_name, "<br>",
			                                   "Installation name:", dsub$installation_name, "<br>",
			                                   "Longitude:", dsub$receiver_deployment_longitude, "<br>",
			                                   "Latitude:", dsub$receiver_deployment_latitude, "<br>",
			                                   "Number of detections:", dsub$freq, "<br>",
			                                   "Number of tags detected:", dsub$nT, "<br>",
			                                   "Transmitter ID's:", dsub$transmitter_ids, "<br>",
			                                   "First date by ID:", dsub$start_dt, "<br>",
			                                   "Last date by ID:", dsub$end_dt, "<br>",
			                                   "QC flag:", "<b>Likely invalid</b>"))
			  }
			## Plot invalid detections
			if (sum(data$Detection_QC == 4, na.rm = TRUE) > 0) {
			  dsub <- subset(dataC, Detection_QC == 4)
			  map <- map %>% 
			    addCircleMarkers(lng = dsub$receiver_deployment_longitude, 
			                     lat = dsub$receiver_deployment_latitude,
			                     group = "Invalid",
			                     radius = dsub$binned_detections * 5,
			                     weight = 0.25,
			                     color = cpal[4],
			                     opacity = 1,
			                     fillColor = cpal[4],
			                     fillOpacity = 0.65,
			                     popup = paste("<b>Quality Controlled Detections</b>", "<br>",
			                                   "Station name:", dsub$station_name, "<br>",
			                                   "Installation name:", dsub$installation_name, "<br>",
			                                   "Longitude:", dsub$receiver_deployment_longitude, "<br>",
			                                   "Latitude:", dsub$receiver_deployment_latitude, "<br>",
			                                   "Number of detections:", dsub$n, "<br>",
			                                   "Number of tags detected:", dsub$nT, "<br>",
			                                   "Transmitter ID's:", dsub$transmitter_ids, "<br>",
			                                   "First date by ID:", dsub$start_dt, "<br>",
			                                   "Last date by ID:", dsub$end_dt, "<br>",
			                                   "QC flag:", "<b>Invalid</b>"))
			}

			## Plot release locations
		map <- map %>%
		  addMarkers(lng = releases$transmitter_deployment_longitude, 
		             lat = releases$transmitter_deployment_latitude,
		             group = "Deployment locations",
		             popup = paste("<b>Tag Deployment</b>", "<br>", 
		                          "Filename:", 
		                           paste(releases$transmitter_id, 
		                                 releases$tag_id, 
		                                 releases$transmitter_deployment_id, 
		                                 sep = "_")),
		             clusterOptions = markerClusterOptions()
		             ) %>%
		  addLegend(position = "topright",
		            title = paste(species$species_common_name[i], "QC flags"),
		            values = dataC$Detection_QC,
		            colors = cpal,
		              #c("#1A9641", "#A6D96A", "#FDAE61", "#D7191C"),
		            opacity = 0.85,
		            labels = c("Valid", "Likely valid", "Likely invalid", "Invalid"),
		            layerId = "QCflags") %>%
		  addLayersControl(
		    baseGroups = c("Default", "ESRI Satellite", "ESRI Bathymetry"),
		    overlayGroups = c("Species distribution", "Valid", "Likely valid", "Likely invalid", "Invalid", "Deployment locations"),
		    options = layersControlOptions(collapsed = FALSE),
		    position = "bottomleft"
		  )

		fs <- tempfile(pattern = paste0(gsub(' ', '_', species$species_common_name[i]),
		                                "_QCmap"),
		               fileext = ".html")
		saveWidget(map, file = fs, selfcontained = TRUE)
		
		if (all(!is.null(path), path != "wb")) {
		  fs2 <- file.path(path, paste0(gsub(' ', '_', species$species_common_name[i]),
		                                "_QCmap.html"))
		  system2("cp", args = c(fs, fs2))
		    
		} else if (all(!is.null(path), path == "wb")) {
		  browseURL(fs)
		  
		} else if (is.null(path)) {
		  print(map)
		}
	}
}