##' @title produce maps showing the occurrence of QC'd detections per species
##'
##' @description \code{plotQC()} QC'd detections colour-coded by their
##' assessed validity status, overlaid on species expert distribution extent
##'
##' @param x a remora output object with \code{class(remora_QC)}
##' @param path string; path where maps will be saved. Default is to write plot
##' to the working directory as a \code{.png} file. 
##' @param suffix text to be added to plot filename (default is NULL)
##' @return produces maps showing species expert distribution and
##' location of QC'd detections
##'
##' @examples
##' ## example QC'd data
##' data(TownsvilleReefQC)
##' 
##' ## Plot QC output to graphics window
##' plotQC(TownsvilleReefQC, path = NULL)
##' 
##' \dontrun{
##' ## Plot QC output, saved to the working directory as .png file(s) 
##' ##   using species common name(s)
##' plotQC(TownsvilleReefQC)
##' }
##' 
##' @importFrom tools file_ext
##' @importFrom data.table fread rbindlist
##' @importFrom plyr ldply '.' ddply count
##' @importFrom scales alpha
##' @importFrom maps map.axes map
##' @importFrom sp plot
##' @importFrom grDevices png dev.off extendrange
##' @importFrom graphics par points legend mtext 
##'
##' @export

plotQC <- function(x, path = getwd(), suffix = NULL) {

  if(!inherits(x, "remora_QC")) 
    stop("\033[31;1mx must be a nested tibble with class `remora_QC`\033[0m")
  if(!is.null(path)) {
    if(!dir.exists(path)) 
      stop("\033[31;1mPlot cannot be saved to the specified path, make sure the directory exists!\033[0m")
  }
  
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

		## turn warnings off
		options(warn = -1)
		if (!is.null(path)) {
		  png(
		    filename = paste0(
		      file.path(
		        path,
		        gsub(' ', '_', species$species_common_name[i])
		        ),
		      ifelse(!is.null(suffix), paste0("_", suffix), ""),
		      ".png"),
		    width = 1920,
		    height = 800,
		    units = "px",
		    res = 92,
		    bg = "white"
		  )
		  
		} 

		par(mfrow = c(1,2), oma = c(0, 0, 2, 0))
		
		## First panel - Australia's spatial extent
		if (!is.null(class(expert_shp))) {
		  plot(expert_shp,
		       xlim = c(100, 165),
		       ylim = c(-45, -4),
		       col = alpha('dark blue', 0.25),
		       border = alpha('dark blue', 0.25)
		  )
		  
		  map("world",
		            xlim = c(100, 165),
		            ylim = c(-45, -5),
		            fill = TRUE,
		            col = "grey",
		            border = "grey",
		            add = TRUE)
		  map.axes()
		} else {
		  map("world",
		            xlim = c(100, 165),
		            ylim = c(-45, -5),
		            fill = TRUE,
		            col = "grey",
		            border = "grey")
		  map.axes()
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

			leg <- c(
			  'Valid detections',
			  'Likely valid detections',
			  'Likely invalid detections',
			  'Invalid detections',
			  'Tag release location',
			  'Species expert distribution extent'
			)
			col <- c('#1A9641',
			         '#A6D96A',
			         '#FDAE61',
			         '#D7191C',
			         'blue',
			         alpha('dark blue', 0.5))
			pch <- c(19, 19, 19, 19, 4, 15)
			
			if (is.null(expert_shp)) {
			  leg <- leg[1:5]
			  col <- col[1:5]
			  pch <- pch[1:5]
			}
			
			legend(
			  x = 135,
			  y = -25,
			  #centre on Australia landmass
			  legend = leg,
			  col = col,
			  pch = pch,
			  cex = ifelse(!is.null(path), 1, 0.5),
			  bg = "transparent",
			  bty = 'n',
			  xjust = 0.5,
			  yjust = 0.5
			)
			
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
			
			if (!is.null(class(expert_shp))) {
			  plot(expert_shp,
			       xlim = xr,
			       ylim = yr,
			       col = alpha('dark blue', 0.25),
			       border = alpha('dark blue', 0.25)
			       )
			  
			  map("world", 
			            add = TRUE, 
			            fill = TRUE, 
			            col = "grey", 
			            border = "grey")
			  map.axes()
			} else {
			  map("world",
			            xlim = extendrange(r=xr, f=0.05),
			            ylim = extendrange(r=yr, f=0.05),
			            fill = TRUE,
			            col = "grey",
			            border = "grey")
			  map.axes()
			}
			
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
	## turn warnings back on
	options(warn = 0)
	
}
