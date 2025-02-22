##' @title test the validity of individual detections for tags detected more than once
##'
##' @description Subjects tag detections to 8 quality control tests and appends results to each detection record
##'
##' @param x a list of un-QC'd data for each tag deployment
##' @param Lcheck (logical; default TRUE) test for receiver_deployment_latitudes
##' in N hemisphere at correct to S hemisphere. Set to FALSE for QC on N hemisphere data
##' @param logfile path to logfile; default is the working directory
##'
##' @details ...
##'
##' @return temporal_outcome is a list with each element corresponding to a QC'd tag detection file
##'
##' @importFrom dplyr %>% bind_cols
##' @importFrom sf st_as_sf st_distance st_crs st_intersects st_coordinates
##'
##' @keywords internal
##'


qc <- function(x, Lcheck = TRUE, logfile) {
  if(!is.data.frame(x)) stop("x must be a data.frame")
  
  ## Initial tests to identify & correct obvious errors in data
  ## first check for NA's in detection_datetime & remove and flag in logfile
  rn <- which(is.na(x$detection_datetime))
  if(length(rn) > 0) {
    lapply(1:length(rn), function(i) {
      write(paste0(x$filename[1],
                   ":  ", length(rn), " NA's found in detection_datetime; records removed from QC'd output"),
            file = logfile,
            append = TRUE)
    })
    ## remove records with NA's in the above variables so QC can proceed
    x <- x[-rn,]
  }
  
  ## check for NA's in (receiver_deployment) longitude/latitude & remove and flag in logfile
  rn <- which(is.na(x$longitude) | is.na(x$latitude))
  if(length(rn) > 0) {
    lapply(1:length(rn), function(i) {
      write(paste0(x$filename[1],
                   ":  ", length(rn), " NA's found in receiver_deployment_longitude &/or latitude; records removed from QC'd output"),
            file = logfile,
            append = TRUE)
    })
    ## remove records with NA's in the above variables so QC can proceed
    x <- x[-rn,]
  }
  
  ## check for & correct any lat's incorrectly in N hemisphere
  if(any(x$latitude > 0) & Lcheck) {
    ## how many incorrect records
    n <- sum(x$latitude > 0)
    ## write to logfile
    write(paste0(x$filename[1],
                ":  ", n, " receiver_deployment_latitude(s) incorrectly entered in N hemisphere; corrected in QC output"),
          file = logfile,
          append = TRUE)

      x <- x %>% mutate(latitude = ifelse(latitude > 0, -1 * latitude, latitude))
  }


  ## Configure output processed data file
  temporal_outcome <- data.frame(matrix(ncol = 8, nrow = nrow(x)))
  colnames(temporal_outcome) <- c("FDA_QC",
                                  "Velocity_QC",
                                  "Distance_QC",
                                  "DetectionDistribution_QC",
                                  "DistanceRelease_QC",
                                  "ReleaseDate_QC",
                                  "ReleaseLocation_QC",
                                  "Detection_QC")
  
  ## FIXME: these 2 sections now redundant - IDJ 25/01/2022
  ## check for missing detection coordinates
  if(any(is.na(x$longitude)) | any(is.na(x$latitude))) {
    ## how many NA lon and/or lat records
    n <- sum(is.na(x$longitude) | is.na(x$latitude))
    ## write to logfile
    write(paste0(x$filename[1],
                 ":  ", n, " receiver_deployment_longitudes &/or latitudes are missing; file not QC'd"),
          file = logfile,
          append = TRUE)
    
    x <- x %>%
      rename(receiver_deployment_longitude = longitude,
             receiver_deployment_latitude = latitude)
    return(bind_cols(x, temporal_outcome))
    stop("NA's found in detection locations - check logfile for details")
  }

  ## check for missing transmitter deployment coordinates
  if(any(is.na(x$transmitter_deployment_longitude),
         is.na(x$transmitter_deployment_latitude))) {

    ## write to logfile
    write(paste0(x$filename[1],
                 ":  transmitter_deployment_longitude &/or latitude are missing; file not QC'd"),
          file = logfile,
          append = TRUE)
    
    x <- x %>%
      rename(receiver_deployment_longitude = longitude,
             receiver_deployment_latitude = latitude)
    return(bind_cols(x, temporal_outcome))
    stop("NA's found in transmitter deployment locations - check logfile for details")
  }

  spe <- unique(x$species_scientific_name)
  CAAB_species_id <- unique(x$CAAB_species_id)

  ## Find corresponding ALA shapefile based on species name
  shp_b <- NULL
  if (!is.na(spe) & !is.na(CAAB_species_id))
    shp_b <- try(get_expert_distribution_shp(CAAB_species_id, spe), silent = TRUE)

  ## if no shape file or spe or CAAB_species_id is missing then append to logfile & continue
  if(is.null(shp_b)) {
    ## write to logfile
    write(paste0(x$filename[1],
                ": shapefile not available for ", spe, "; Dectection distribution not tested"),
          file = logfile,
          append = TRUE)
  } else if(inherits(shp_b, "try-error")) {
    ## write to logfile
    write(paste0(x$filename[1],
                 ": shapefile could not be downloaded for ", spe, "; Dectection distribution not tested"),
          file = logfile,
          append = TRUE)
    shp_b <- NULL
  }
  
  ## Converts unique sets of lat/lon detection coordinates and release lat/lon 
  ##  coordinates to SpatialPoints to test subsequently whether or not detections 
  ##  are in distribution range
  if (!is.null(shp_b)) {
    ll <- unique(data.frame(x$longitude, x$latitude)) %>%
      st_as_sf(coords = c("x.longitude", "x.latitude"), crs = st_crs(shp_b))

    # coordinates(ll) <- ~ x.longitude + x.latitude
    # proj4string(ll) <- suppressWarnings(proj4string(shp_b))

    if (!is.na(x$transmitter_deployment_longitude[1])) {
      ll_r <- data.frame(lon = x$transmitter_deployment_longitude[1], 
                         lat = x$transmitter_deployment_latitude[1]) %>%
        st_as_sf(coords = c("lon", "lat"), crs = st_crs(shp_b))
      
      # coordinates(ll_r) <-
      #   ~ x.transmitter_deployment_longitude.1. + x.transmitter_deployment_latitude.1.
      # proj4string(ll_r) <- suppressWarnings(proj4string(shp_b))
    }
  }

		## False Detection Algorithm test
		sta_rec <- unique(x$installation_name)
		sta_rec <- sta_rec[order(sta_rec)]

		for (j in 1:length(sta_rec)){
			sel <- which(x$installation_name == sta_rec[j])
			sub <- x[sel, ]

			## Calculate time differences between detections (in minutes)
			time_diff <- as.numeric(difftime(sub$detection_datetime[2:nrow(sub)],
			                                 sub$detection_datetime[1:(nrow(sub)-1)],
			                                 tz = "UTC", units = "mins"))
			temporal_outcome[sel, 1] <-
			  ifelse(sum(time_diff <= 30) > sum(time_diff >= 720) & nrow(sub) > 1, 1, 2)
		}

		## Distance and Velocity tests
		position <- data.frame(longitude = c(x$transmitter_deployment_longitude[1], x$longitude),
		                       latitude = c(x$transmitter_deployment_latitude[1], x$latitude))
		
		dist <- shortest_dist2(position,
		                      x$installation_name,
		                      raster = NULL,
		                      tr = tr)
		
		if (length(dist) == 1) {
		  timediff <- as.numeric(
		    difftime(
		      x$transmitter_deployment_datetime,
		      x$detection_datetime,
		      tz = "UTC",
		      units = "secs"
		    )
		  )
		  velocity <- (dist * 1000) / timediff

		  temporal_outcome[2] <- ifelse(velocity <= 10, 1, 2)
		  temporal_outcome[3] <- ifelse(dist <= 1000, 1, 2)

		} else if (length(dist) > 1) {
		  dist_next <- c(dist[2:nrow(dist)], NA)

		  time <-
		    c(x$transmitter_deployment_datetime[1],
		      x$detection_datetime)
		  timediff <-
		    abs(as.numeric(difftime(
		      time[1:(length(time) - 1)], time[2:length(time)],
		      tz = "UTC", units = "secs"
		    )))
		  timediff_next <- c(timediff[2:length(timediff)], NA)

		  ## Exception to overcome the fact that a same tag may be detected by
		  ##  two neighbouring stations at the exact same time, thus creating
		  ##  infinite velocity values
		  timediff[which(timediff == 0)] <- 1
		  timediff_next[which(timediff_next == 0)] <- 1
		  velocity <- (dist * 1000) / timediff
		  velocity_next <- (dist_next * 1000) / timediff_next

		  ## Velocity test
		  temporal_outcome[, 2] <-
		    ifelse(velocity > 10 & velocity_next > 10, 2, 1)
		  temporal_outcome[1, 2] <- ifelse(velocity[1] > 10, 2, 1)
		  temporal_outcome[nrow(x), 2] <-
		    ifelse(velocity[nrow(x)] > 10, 2, 1)
		  
		  ## Distance test
		  temporal_outcome[, 3] <-
		    ifelse(dist > 1000 & dist_next > 1000, 2, 1)
		  temporal_outcome[1, 3] <- ifelse(dist[1] > 1000, 2, 1)
		  temporal_outcome[nrow(x), 3] <-
		    ifelse(dist[nrow(x)] > 1000, 2, 1)

		}

		## Detection distribution test
		temporal_outcome[, 4] <- ifelse(is.null(shp_b), 3, 1)
		if(!is.null(shp_b)) {
			out <- suppressWarnings(which(is.na(st_intersects(ll, shp_b))))
			if(length(out) > 0) {
			  ll.xy <- st_coordinates(ll)
			  temporal_outcome[all(x$longitude %in% ll.xy[out, 1],
			                     x$latitude %in% ll.xy[out, 2]), 4] <- 2
			}
		}

		## Distance from release
		rel.pts <- data.frame(lon = x$transmitter_deployment_longitude[rep(1, nrow(x))],
		                     lat = x$transmitter_deployment_latitude[rep(1, nrow(x))]) %>%
		  st_as_sf(coords = c("lon", "lat"), crs = 4326)
		
		det.pts <- data.frame(lon = x$longitude, lat = x$latitude) %>%
		  st_as_sf(coords = c("lon", "lat"), crs = 4326)
		## return distance in km
		dist_r <- as.numeric(st_distance(rel.pts, det.pts, by_element = TRUE) / 1000)
		temporal_outcome[, 5] <- ifelse(dist_r > 500, 2, 1)

		## Release date before detection date
		release_timediff <- as.numeric(difftime(x$detection_datetime,
		                                        x$transmitter_deployment_datetime, tz = "UTC",
		                                        units = "mins"))
		## -720 minutes (12 h) to take into account potential time zone differences
		temporal_outcome[which(release_timediff >= (-720)), 6] <- 1
		temporal_outcome[which(release_timediff < (-720)), 6] <- 2

		## Release location test
		if(!is.null(shp_b)) {
			temporal_outcome[, 7] <- ifelse(all(dist[1] > 500,
			                                   sum(is.na(st_intersects(ll_r, shp_b))) > 0), 2, 1)
		} else {
			temporal_outcome[, 7] <- ifelse(dist[1] > 500, 2, 1)
		}
		
		## Detection QC
		ones <- as.numeric(rowSums(temporal_outcome[, c(1:5)] == 1))
		temporal_outcome[which(ones <= 2), 8] <- 4
		temporal_outcome[which(ones == 3), 8] <- 3
		temporal_outcome[which(ones == 4), 8] <- 2
		temporal_outcome[which(ones == 5), 8] <- 1
		temporal_outcome$Velocity_QC <- as.numeric(temporal_outcome$Velocity_QC)
		temporal_outcome$Distance_QC <- as.numeric(temporal_outcome$Distance_QC)

	x <- x %>%
	  rename(receiver_deployment_longitude = longitude,
	         receiver_deployment_latitude = latitude)
	
	return(bind_cols(x, temporal_outcome))

}
