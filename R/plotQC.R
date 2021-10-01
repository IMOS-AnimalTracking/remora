##' @title produce maps showing the occurrence of QC'd detections per species
##'
##' @description \code{plotQC()} QC'd detections colour-coded by their
##' assessed validity status, overlaid on species expert distribution extent
##'
##' @param x a remora output object with \code{class(remora_QC)}
##' @param path string; path where maps will be saved. Default is to write plot
##' to the working directory as a \code{.png} file. 
##'
##' @return produces maps showing species expert distribution and
##' location of QC'd detections
##'
##' @examples
##' ## example QC'd data
##' data(test_data)
##' ## Plot QC output to graphics window
##' plotQC(test_data, path = NULL)
##'
##' @importFrom tools file_ext
##' @importFrom data.table fread rbindlist
##' @importFrom plyr ldply '.' ddply count
##' @importFrom scales alpha
##' @importFrom maps map.axes
##' @importFrom sp plot
##'
##' @export

plotQC <- function(x, path = getwd()) {

  if(!inherits(x, "remora_QC")) stop("x must be a nested tibble with class `remora_QC`")
  
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
		  get_expert_distribution_shp_CAAB(CAAB_species_id = species$CAAB_species_id[i])
		if(class(expert_shp) == 'NULL') {
			print(paste('No expert distribution shapefile available for species ',
			            caab_dump$COMMON_NAME[which(caab_dump$SPCODE == CAAB_id)],
			            ' (',
			            caab_dump$SCIENTIFIC_NAME[which(caab_dump$SPCODE == CAAB_id)],
			            ', ',
			            caab_dump$AUTHORITY[which(caab_dump$SPCODE == CAAB_id)], ')',
			            sep = ''))
		}

		data <- QCdata[which(QCdata$CAAB_species_id == species$CAAB_species_id[i]), ]
		releases <- unique(data.frame(data$transmitter_deployment_longitude,
		                              data$transmitter_deployment_latitude,
		                              data$ReleaseLocation_QC))
			colnames(releases) <- c('transmitter_deployment_longitude',
			                        'transmitter_deployment_latitude',
			                        'ReleaseLocation_QC')
		data <- data[, c('receiver_name',
		                 'station_name',
		                 'installation_name',
		                 'receiver_deployment_longitude',
		                 'receiver_deployment_latitude',
		                 'Detection_QC')]
		data <- count(data)
		binned_detects <- data.frame(data$freq,
		                             bin=cut(data$freq,
		                                     c(0, 10, 100, 1000, 10000, 100000),
		                                     include.lowest=TRUE))
		data$binned_detections <- as.numeric(binned_detects$bin)

		png(file = paste(path,
		                 '/',
		                 gsub(' ', '_', species$species_common_name[i]), ".png", sep= ""),
		    width = 1920,
		    height = 800,
		    units = "px",
		    res=92,
		    bg = "white")


		par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
		## First panel - Australia's spatial extent

		if(requireNamespace("rnaturalearth", quietly = TRUE)) {
		wm <- rnaturalearth::ne_countries(scale=50, returnclass = "sp")
		## use maps:: to avoid conflict with purrr::map
		maps::map(wm,
		    xlim=c(100, 165),
		    ylim = c(-45, -5),
		    fill = TRUE,
		    col = "grey")
		} else {
		  maps::map("world",
		      xlim=c(100, 165),
		      ylim = c(-45, -5),
		      fill = TRUE,
		      col = "grey")
		}
			map.axes()
			if (!is.null(class(expert_shp))) {
			  plot(expert_shp,
			       add = T,
			       col = alpha('dark blue', 0.25)
			       )
			}

		## Plot receiver locations
		## points(rec$deployment_longitude[which(!rec$station_name %in% unique(data$station_name))],
		##	rec$deployment_latitude[which(!rec$station_name %in% unique(data$station_name))],
		##	col = 'dark red',
		##	pch = 4,
		##	cex = .5)

		## Plot valid detections
		if (sum(data$Detection_QC == 1, na.rm = TRUE) > 0) {
		  points(data$receiver_deployment_longitude[which(data$Detection_QC == 1)],
		         data$receiver_deployment_latitude[which(data$Detection_QC == 1)],
		         col = alpha('#1A9641', 0.65),
		         cex = data$binned_detections[which(data$Detection_QC == 1)],
		         pch = 19)
		}

			## Plot invalid detections
		if (sum(data$Detection_QC == 2, na.rm = TRUE) > 0) {
		  points(data$receiver_deployment_longitude[which(data$Detection_QC == 2)],
		         data$receiver_deployment_latitude[which(data$Detection_QC == 2)],
		         col = alpha('#A6D96A', 0.65),
		         cex = data$binned_detections[which(data$Detection_QC == 2)],
		         pch = 19)
		}
			## Plot valid detections
			if (sum(data$Detection_QC == 3, na.rm = TRUE) > 0) {
			  points(data$receiver_deployment_longitude[which(data$Detection_QC == 3)],
			         data$receiver_deployment_latitude[which(data$Detection_QC == 3)],
			         col = alpha('#FDAE61', 0.65),
			         cex = data$binned_detections[which(data$Detection_QC == 3)],
			         pch = 19)
			  }
			## Plot invalid detections
			if (sum(data$Detection_QC == 4, na.rm = TRUE) > 0) {
			  points(data$receiver_deployment_longitude[which(data$Detection_QC == 4)],
			         data$receiver_deployment_latitude[which(data$Detection_QC == 4)],
			         col = alpha('#D7191C', 0.65),
			         cex = data$binned_detections[which(data$Detection_QC == 4)],
			         pch = 19)
			}
			## Plot release locations
			points(releases[, c('transmitter_deployment_longitude',
			                    'transmitter_deployment_latitude')],
			       col = 'blue',
			       pch = 4,
			       cex = 2,
			       lwd = 2)

			legend(x = 135, y = -25, #centre on Australia landmass
			       legend = c('Valid detections',
			                  'Likely valid detections',
			                  'Likely invalid detections',
			                  'Invalid detections',
			                  'Tag release location',
			                  'Species expert distribution extent'),
					col = c('#1A9641',
					        '#A6D96A',
					        '#FDAE61',
					        '#D7191C',
					        'blue',
					        alpha('dark blue', 0.5)),
					pch = c(19, 19, 19, 19, 4, 15),
					cex = 1,
					bg="transparent",
					bty = 'n',
					xjust = 0.5,
					yjust = 0.5)

		## Second panel - data's spatial extent + 10 %
			diff_long <- .1 * (max(c(releases$transmitter_deployment_longitude,
			                         data$receiver_deployment_longitude),
			                       na.rm=T) -
			                     min(c(releases$transmitter_deployment_longitude,
			                           data$receiver_deployment_longitude),
			                         na.rm=T))
			diff_lat <- .1 * (max(c(releases$transmitter_deployment_latitude,
			                        data$receiver_deployment_latitude),
			                      na.rm=T) -
			                    min(c(releases$transmitter_deployment_latitude,
			                          data$receiver_deployment_latitude),
			                        na.rm=T))
			xr <- c(min(c(releases$transmitter_deployment_longitude,
			              data$receiver_deployment_longitude),
			            na.rm=T) -
			          diff_long,
			        max(c(releases$transmitter_deployment_longitude,
			              data$receiver_deployment_longitude),
			            na.rm=T) +
			          diff_long)
			yr <- c(min(c(releases$transmitter_deployment_latitude,
			              data$receiver_deployment_latitude),
			            na.rm=T) -
			          diff_lat,
			        max(c(releases$transmitter_deployment_latitude,
			              data$receiver_deployment_latitude),
			            na.rm=T) +
			          diff_lat)
			if (class(expert_shp) != 'NULL') {
			  plot(expert_shp,
			       xlim=xr,
			       ylim=yr,
			       col = alpha('dark blue', 0.25))
			}
      if(exists("wm")) maps::map(wm, add = TRUE, fill = TRUE, col = 'grey')
			else maps::map("world", add = TRUE, fill = TRUE, col = 'grey')
			map.axes()
			## Plot receiver locations
			## points(rec$deployment_longitude[which(!rec$station_name %in% unique(data$station_name))],
			##  rec$deployment_latitude[which(!rec$station_name %in% unique(data$station_name))],
			##  col = 'dark red',
			##  pch = 4,
			##  cex = .5)

			## Plot valid detections
			if (sum(data$Detection_QC == 1, na.rm = TRUE) > 0) {
			  points(data$receiver_deployment_longitude[which(data$Detection_QC == 1)],
			         data$receiver_deployment_latitude[which(data$Detection_QC == 1)],
			         col = alpha('#1A9641', 0.65),
			         cex = data$binned_detections[which(data$Detection_QC == 1)],
			         pch = 19)
			}
			## Plot invalid detections
			if (sum(data$Detection_QC == 2, na.rm = TRUE) > 0) {
			  points(data$receiver_deployment_longitude[which(data$Detection_QC == 2)],
			         data$receiver_deployment_latitude[which(data$Detection_QC == 2)],
			         col = alpha('#A6D96A', 0.65),
			cex = data$binned_detections[which(data$Detection_QC == 2)],
			pch = 19)
			}
			## Plot valid detections
			if (sum(data$Detection_QC == 3, na.rm = TRUE) > 0) {
			  points(data$receiver_deployment_longitude[which(data$Detection_QC == 3)],
			         data$receiver_deployment_latitude[which(data$Detection_QC == 3)],
			         col = alpha('#FDAE61', 0.65),
			         cex = data$binned_detections[which(data$Detection_QC == 3)],
			         pch = 19)
			}
			## Plot invalid detections
			if (sum(data$Detection_QC == 4, na.rm = TRUE) > 0) {
			  points(data$receiver_deployment_longitude[which(data$Detection_QC == 4)],
			         data$receiver_deployment_latitude[which(data$Detection_QC == 4)],
			         col = alpha('#D7191C', 0.65),
			         cex = data$binned_detections[which(data$Detection_QC == 4)],
			         pch = 19)
			}
			## Plot release locations
			points(releases[, c('transmitter_deployment_longitude',
			                    'transmitter_deployment_latitude')],
			       col = 'blue',
			       pch = 4,
			       cex = 2,
			       lwd = 2)

		mtext(paste(species$species_common_name[i],
		            ', ',
		            length(unique(QCdata$transmitter_id[which(QCdata$CAAB_species_id == species$CAAB_species_id[i])])),
		            ' tags.', sep = ''),
		      outer = TRUE,
		      cex = 1.5)

		mtext(paste('Total # detections = ',
		            sum(data$freq),
		            ', # invalid detections = ',
		            sum(data$freq[which(data$Detection_QC > 2)]), sep = ''),
		      outer = TRUE,
		      cex = 1.5,
		      line = -2)
		if(!is.null(path)) dev.off()
	}
}
