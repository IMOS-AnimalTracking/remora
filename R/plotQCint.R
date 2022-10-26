##' @title produce interactive leaflet maps showing the occurrence of QC'd detections per species
##'
##' @description \code{plotQCint()} QC'd detections colour-coded by their
##' assessed validity status, overlaid on species expert distribution extent
##'
##' @param x a remora output object with \code{class(remora_QC)}
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
##' @importFrom tools file_ext
##' @importFrom data.table fread rbindlist
##' @importFrom leaflet leaflet addMarkers addCircleMarkers fitBounds
##' @importFrom dplyr '%>%' summarise left_join group_by
##' @importFrom plyr ldply '.' ddply count
##' @importFrom scales alpha
##' @importFrom maps map.axes map
##' @importFrom sp plot
##' @importFrom grDevices png dev.off extendrange
##' @importFrom graphics par points legend mtext 
##'
##' @export

plotQCint <- function(x) {

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
		    addPolygons(weight = 0.25) %>%
		    addProviderTiles("CartoDB.Positron")
		} else {
		  map <- leaflet() %>%
		    fitBounds(lng1 = bb[1], lat1 = bb[3], lng2 = bb[2], lat2 = bb[4]) %>%
		    addProviderTiles("CartoDB.Positron")
		}

		## Plot valid detections
		if (sum(data$Detection_QC == 1, na.rm = TRUE) > 0) {
		  dsub <- subset(dataC, Detection_QC == 1)
		  map <- map %>% 
		    addCircleMarkers(lng = dsub$receiver_deployment_longitude, 
		               lat = dsub$receiver_deployment_latitude,
		               radius = dsub$binned_detections * 5,
		               weight = 0.25,
		               color = "#1A9641",
		               fillColor = "#1A9641",
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

			## Plot invalid detections
		if (sum(data$Detection_QC == 2, na.rm = TRUE) > 0) {
		  dsub <- subset(dataC, Detection_QC == 2)
		  map <- map %>% 
		    addCircleMarkers(lng = dsub$receiver_deployment_longitude, 
		                     lat = dsub$receiver_deployment_latitude,
		                     radius = dsub$binned_detections * 5,
		                     weight = 0.25,
		                     color = "#A6D96A",
		                     fillColor = "#A6D96A",
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
			if (sum(data$Detection_QC == 3, na.rm = TRUE) > 0) {
			  dsub <- subset(dataC, Detection_QC == 3)
			  map <- map %>% 
			    addCircleMarkers(lng = dsub$receiver_deployment_longitude, 
			                     lat = dsub$receiver_deployment_latitude,
			                     radius = dsub$binned_detections * 5,
			                     weight = 0.25,
			                     color = "#FDAE61",
			                     fillColor = "#FDAE61",
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
			                     radius = dsub$binned_detections * 5,
			                     weight = 0.25,
			                     color = "#D7191C",
			                     fillColor = "#D7191C",
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
		             popup = paste("<b>Tag Deployment</b>", "<br>", 
		                          "Filename:", 
		                           paste(releases$transmitter_id, 
		                                 releases$tag_id, 
		                                 releases$transmitter_deployment_id, 
		                                 sep = "_"))
		             )

print(map)
	}
}
