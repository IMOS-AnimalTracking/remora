##' @title produce interactive leaflet maps showing the occurrence of QC'd detections per species
##'
##' @description \code{plotQCint()} QC'd detections colour-coded by their
##' assessed validity status, overlaid on species expert distribution extent
##'
##' @param x a remora output object with \code{class(remora_QC)}
##' @param path path to save map(s) as an .html file. Options are: `NULL` (default);
##' `wb` (render map in default web browser); or a valid file path (map saved as a
##' self-contained .html file)
##' @return produces interactive leaflet maps of species expert distribution and
##' location of QC'd detections
##'
##' @examples
##' ## example QC'd data
##' data(TownsvilleReefQC)
##' 
##' ## render output to viewer
##' plotQCint(TownsvilleReefQC)
##' 
##' 
##' @importFrom data.table fread rbindlist
##' @importFrom leaflet leaflet addMarkers addCircleMarkers fitBounds addLegend 
##' @importFrom leaflet addProviderTiles addLayersControl addPolygons layersControlOptions
##' @importFrom htmlwidgets saveWidget
##' @importFrom dplyr '%>%' summarise left_join group_by
##' @importFrom plyr ldply '.' ddply count
##' @importFrom grDevices extendrange
##'
##' @export

plotQCint <- function(x, path = NULL) {

  if(!inherits(x, "remora_QC")) 
    stop("\033[31;1mx must be a nested tibble with class `remora_QC`\033[0m")
  
  QCdata <- rbindlist(x$QC)

	species <- ddply(QCdata, '.'(QCdata$CAAB_species_id,
	                             QCdata$species_scientific_name,
	                             QCdata$species_common_name),
	                 nrow)

	## Retrieve species list
	colnames(species) <- c('CAAB_species_id',
	                       'species_scientific_name',
	                       'species_common_name',
	                       'freq')
	
	for (i in 1:nrow(species)){
		expert_shp <-
		  try(get_expert_distribution_shp_CAAB(CAAB_species_id = species$CAAB_species_id[i], spe = species$species_scientific_name[i]), 
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
		                 'receiver_name',
		                 'station_name',
		                 'installation_name',
		                 'receiver_deployment_longitude',
		                 'receiver_deployment_latitude',
		                 'Detection_QC')]
		 
		nT <- data %>% group_by(receiver_name, 
		                         station_name, 
		                         installation_name, 
		                         receiver_deployment_longitude, 
		                         receiver_deployment_latitude, 
		                         Detection_QC) %>%
		  summarise(nT = length(unique(transmitter_id)))
		 
		dataC <- count(data, vars = c('receiver_name',
		                              'station_name',
		                              'installation_name',
		                              'receiver_deployment_longitude',
		                              'receiver_deployment_latitude',
		                              'Detection_QC'))
		
		binned_detects <- data.frame(dataC$freq,
		                             bin=cut(dataC$freq,
		                                     c(0, 10, 100, 1000, 10000, 100000),
		                                     include.lowest=TRUE))
		
		dataC$binned_detections <- as.numeric(binned_detects$bin)
		
		dataC <- left_join(dataC, nT, by = c("receiver_name", 
		                            "station_name", 
		                            "installation_name", 
		                            "receiver_deployment_longitude", 
		                            "receiver_deployment_latitude", 
		                            "Detection_QC"))

		bb <- with(data, c(extendrange(receiver_deployment_longitude, f=0.15),
		                   extendrange(receiver_deployment_latitude, f=0.15)
		))

		if (!is.null(class(expert_shp))) {
		  map <- leaflet(expert_shp) %>%
		    fitBounds(lng1 = bb[1], lat1 = bb[3], lng2 = bb[2], lat2 = bb[4]) %>%
		    addPolygons(weight = 0.25, group = "Species distribution") %>%
		    addProviderTiles("CartoDB.Positron", group = "Default") %>%
		    addProviderTiles("Esri.OceanBasemap", group = "ESRI Bathymetry")
		  
		} else {
		  map <- leaflet() %>%
		    fitBounds(lng1 = bb[1], lat1 = bb[3], lng2 = bb[2], lat2 = bb[4]) %>%
		    addProviderTiles("CartoDB.Positron", group = "CartoDB")
		}

		## Plot invalid detections - render first so valid overlay these
		if (sum(data$Detection_QC == 2, na.rm = TRUE) > 0) {
		  dsub <- subset(dataC, Detection_QC == 2)
		  map <- map %>% 
		    addCircleMarkers(lng = dsub$receiver_deployment_longitude, 
		                     lat = dsub$receiver_deployment_latitude,
		                     group  = "Likely valid",
		                     radius = dsub$binned_detections * 5,
		                     weight = 0.25,
		                     color = "#89D9CF",
		                     fillColor = "#89D9CF",
		                     fillOpacity = 0.5,
		                     popup = paste("<b>Quality Controlled Detections</b>", "<br>",
		                                   "Receiver name:", dsub$receiver_name,"<br>",
		                                   "Station name:", dsub$station_name, "<br>",
		                                   "Installation name:", dsub$installation_name, "<br>",
		                                   "Longitude:", dsub$receiver_deployment_longitude, "<br>",
		                                   "Latitude:", dsub$receiver_deployment_latitude, "<br>",
		                                   "Number of detections:", dsub$freq, "<br>",
		                                   "Number of tags detected:", dsub$nT, "<br>",
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
		               color = "#004B40",
		               fillColor = "#004B40",
		               fillOpacity = 0.5,
		               popup = paste("<b>Quality Controlled Detections</b>", "<br>",
		                             "Receiver name:", dsub$receiver_name,"<br>",
		                             "Station name:", dsub$station_name, "<br>",
		                             "Installation name:", dsub$installation_name, "<br>",
		                             "Longitude:", dsub$receiver_deployment_longitude, "<br>",
		                             "Latitude:", dsub$receiver_deployment_latitude, "<br>",
		                             "Number of detections:", dsub$freq, "<br>",
		                             "Number of tags detected:", dsub$nT, "<br>",
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
			                     color = "#E4C6A1",
			                     fillColor = "#E4C6A1",
			                     fillOpacity = 0.5,
			                     popup = paste("<b>Quality Controlled Detections</b>", "<br>",
			                                   "Receiver name:", dsub$receiver_name,"<br>",
			                                   "Station name:", dsub$station_name, "<br>",
			                                   "Installation name:", dsub$installation_name, "<br>",
			                                   "Longitude:", dsub$receiver_deployment_longitude, "<br>",
			                                   "Latitude:", dsub$receiver_deployment_latitude, "<br>",
			                                   "Number of detections:", dsub$freq, "<br>",
			                                   "Number of tags detected:", dsub$nT, "<br>",
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
			                     color = "#533600",
			                     fillColor = "#533600",
			                     fillOpacity = 0.5,
			                     popup = paste("<b>Quality Controlled Detections</b>", "<br>",
			                                   "Receiver name:", dsub$receiver_name,"<br>",
			                                   "Station name:", dsub$station_name, "<br>",
			                                   "Installation name:", dsub$installation_name, "<br>",
			                                   "Longitude:", dsub$receiver_deployment_longitude, "<br>",
			                                   "Latitude:", dsub$receiver_deployment_latitude, "<br>",
			                                   "Number of detections:", dsub$freq, "<br>",
			                                   "Number of tags detected:", dsub$nT, "<br>",
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
		                                 sep = "_"))
		             ) %>%
		  addLegend(position = "bottomleft",
		            values = dataC$Detection_QC,
		            colors = hcl.colors(n=4, palette = "Green-Brown"),
		              #c("#1A9641", "#A6D96A", "#FDAE61", "#D7191C"),
		            opacity = 0.75,
		            labels = c("Valid", "Likely valid", "Likely invalid", "Invalid")) %>%
		  addLayersControl(
		    baseGroups = c("Default", "ESRI Bathymetry"),
		    overlayGroups = c("Species distribution", "Valid", "Likely valid", "Likely invalid", "Invalid", "Deployment locations"),
		    options = layersControlOptions(collapsed = FALSE)
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