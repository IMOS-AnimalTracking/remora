##' @title test the validity of individual detections for tags detected more than once
##'
##' @description Subjects tag detections to 8 quality control tests and appends results to each detection record
##'
##' @param x a list of un-QC'd data for each tag deployment
##' @param Lcheck (logical; default TRUE) test for receiver_deployment_latitudes
##' in N hemisphere at correct to S hemisphere. Set to FALSE for QC on N hemisphere data
##' @param datecolumn The name of the column in which the date information is held. 
##' @param logfile path to logfile; default is the working directory
##'
##' @details ...
##'
##' @return temporal_outcome is a list with each element corresponding to a QC'd tag detection file
##'
##' @importFrom dplyr '%>%' bind_cols
##' @importFrom sp 'coordinates<-' 'proj4string<-' 'proj4string' over
##' @importFrom geosphere distGeo
##'
##' @keywords internal
##'


qc_updated <- function(x, Lcheck = TRUE, datecolumn = 'datecollected', speciescolumn = 'scientificname',
                       installationcolumn = 'station', logfile) {
  message("Doing dataframe check")
  if(!is.data.frame(x)) stop("x must be a data.frame")
  
  ## Initial tests to identify & correct obvious errors in data
  ## first check for NA's in detection_datetime & remove and flag in logfile
  rn <- which(is.na(x[datecolumn]))
  message("Checking for NAs in datetime")
  message(rn)
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
  message("Checking for NAs in lat/lon")
  message(rn)
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
  message("Checking for incorrect N hemisphere lats")
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
  message("Setting up temporal outcome variable")
  temporal_outcome <- data.frame(matrix(ncol = 8, nrow = nrow(x)))
  colnames(temporal_outcome) <- c("FDA_QC",
                                  "Velocity_QC",
                                  "Distance_QC",
                                  "DetectionDistribution_QC",
                                  "DistanceRelease_QC",
                                  "ReleaseDate_QC",
                                  "ReleaseLocation_QC",
                                  "Detection_QC")
  
  # #Commented out pending figuring out what we use instead of CAAB. 
  # # spe <- unique(x[speciescolumn])
  # # CAAB_species_id <- unique(x$CAAB_species_id)
  # # 
  # # ## Find corresponding ALA shapefile based on species name
  # # shp_b <- NULL
  # # if (!is.na(spe) & !is.na(CAAB_species_id))
  # #   shp_b <- try(get_expert_distribution_shp(CAAB_species_id, spe), silent = TRUE)
  # # 
  # # ## if no shape file or spe or CAAB_species_id is missing then append to logfile & continue
  # # if(is.null(shp_b)) {
  # #   ## write to logfile
  # #   write(paste0(x$filename[1],
  # #                ": shapefile not available for ", spe, "; Dectection distribution not tested"),
  # #         file = logfile,
  # #         append = TRUE)
  # # } else if(inherits(shp_b, "try-error")) {
  # #   ## write to logfile
  # #   write(paste0(x$filename[1],
  # #                ": shapefile could not be downloaded for ", spe, "; Dectection distribution not tested"),
  # #         file = logfile,
  # #         append = TRUE)
  # #   shp_b <- NULL
  # # }
  # # 
  # # ## Converts unique sets of lat/lon detection coordinates and release lat/lon 
  # # ##  coordinates to SpatialPoints to test subsequently whether or not detections 
  # # ##  are in distribution range
  # # if (!is.null(shp_b)) {
  # #   ll <- unique(data.frame(x$longitude, x$latitude))
  # #   coordinates(ll) <- ~ x.longitude + x.latitude
  # #   proj4string(ll) <- suppressWarnings(proj4string(shp_b))
  # #   
  # #   if (!is.na(x$transmitter_deployment_longitude[1])) {
  # #     ll_r <-
  # #       data.frame(x$transmitter_deployment_longitude[1], x$transmitter_deployment_latitude[1])
  # #     coordinates(ll_r) <-
  # #       ~ x.transmitter_deployment_longitude.1. + x.transmitter_deployment_latitude.1.
  # #     proj4string(ll_r) <- suppressWarnings(proj4string(shp_b))
  # #   }
  # # }
  # 
  # # Commenting this out as I try to get other checks working. 
  ## False Detection Algorithm test
  sta_rec <- unique(x[installationcolumn])
  #sta_rec <- sta_rec[order(sta_rec),]
  
  j <- 1
  for (j in 1:length(sta_rec)){
    View(sta_rec[j,])
    #sel <- which(x[installationcolumn] == sta_rec[j])
    #sub <- x[sel, ]
    
    #Gotta do this bit of R jiggery-pokery to make filter recognize installationcolumn as a variable
    #containing a column name and not a column name in and of itself.
    sub <- filter(x, !!as.name(installationcolumn) %in% !!sta_rec[j,])
    View(sub)

    # Calculate time differences between detections (in minutes)
    time_diff <- as.numeric(difftime(sub[datecolumn][2:nrow(sub)],
                                     sub[datecolumn][1:(nrow(sub)-1)],
                                     tz = "UTC", units = "mins"))
    temporal_outcome[sel, 1] <-
      ifelse(sum(time_diff <= 30) > sum(time_diff >= 720) & nrow(sub) > 1, 1, 2)
  }
  
  return(temporal_outcome)
  
  # # 
  # # 
  # # ## Distance and Velocity tests
  # # position <- data.frame(longitude = c(x$transmitter_deployment_longitude[1], x$longitude),
  # #                        latitude = c(x$transmitter_deployment_latitude[1], x$latitude))
  # # 
  # # dist <-
  # #   shortest_dist(position,
  # #                 x[installationcolumn],
  # #                 rast = Aust_raster,
  # #                 tr = tr)
  # # 
  # # if (length(dist) == 1) {
  # #   timediff <- as.numeric(
  # #     difftime(
  # #       x$transmitter_deployment_datetime,
  # #       x[datecolumn],
  # #       tz = "UTC",
  # #       units = "secs"
  # #     )
  # #   )
  # #   velocity <- (dist * 1000) / timediff
  # #   
  # #   temporal_outcome[2] <- ifelse(velocity <= 10, 1, 2)
  # #   temporal_outcome[3] <- ifelse(dist <= 1000, 1, 2)
  # #   
  # # } else if (length(dist) > 1) {
  # #   dist_next <- c(dist[2:nrow(dist)], NA)
  # #   
  # #   time <-
  # #     c(x$transmitter_deployment_datetime[1],
  # #       x[datecolumn])
  # #   timediff <-
  # #     abs(as.numeric(difftime(
  # #       time[1:(length(time) - 1)], time[2:length(time)],
  # #       tz = "UTC", units = "secs"
  # #     )))
  # #   timediff_next <- c(timediff[2:length(timediff)], NA)
  # #   
  # #   ## Exception to overcome the fact that a same tag may be detected by
  # #   ##  two neighbouring stations at the exact same time, thus creating
  # #   ##  infinite velocity values
  # #   timediff[which(timediff == 0)] <- 1
  # #   timediff_next[which(timediff_next == 0)] <- 1
  # #   velocity <- (dist * 1000) / timediff
  # #   velocity_next <- (dist_next * 1000) / timediff_next
  # #   
  # #   ## Velocity test
  # #   temporal_outcome[, 2] <-
  # #     ifelse(velocity > 10 & velocity_next > 10, 2, 1)
  # #   temporal_outcome[1, 2] <- ifelse(velocity[1] > 10, 2, 1)
  # #   temporal_outcome[nrow(x), 2] <-
  # #     ifelse(velocity[nrow(x)] > 10, 2, 1)
  # #   
  # #   ## Distance test
  # #   temporal_outcome[, 3] <-
  # #     ifelse(dist > 1000 & dist_next > 1000, 2, 1)
  # #   temporal_outcome[1, 3] <- ifelse(dist[1] > 1000, 2, 1)
  # #   temporal_outcome[nrow(x), 3] <-
  # #     ifelse(dist[nrow(x)] > 1000, 2, 1)
  # #   
  # # }
  # 
  # # ## Detection distribution test
  # # temporal_outcome[, 4] <- ifelse(is.null(shp_b), 3, 1)
  # # if(!is.null(shp_b)) {
  # #   out <- which(is.na(over(ll, shp_b)))
  # #   if(length(out) > 0) {
  # #     temporal_outcome[x$longitude %in% ll@coords[out, 1] &
  # #                        x$latitude %in% ll@coords[out, 2], 4] <- 2
  # #   }
  # # }
  # 
  # ## Distance from release
  # dist_r <- distGeo(cbind(x$transmitter_deployment_longitude[rep(1, nrow(x))],
  #                         x$transmitter_deployment_latitude[rep(1, nrow(x))]),
  #                   cbind(x$longitude, x$latitude)) / 1000 ## return in km
  # temporal_outcome[, 5] <- ifelse(dist_r > 500, 2, 1)
  # 
  # ## Release date before detection date
  # release_timediff <- as.numeric(difftime(x[datecolumn],
  #                                         x$transmitter_deployment_datetime, tz = "UTC",
  #                                         units = "mins"))
  # ## -720 minutes (12 h) to take into account potential time zone differences
  # temporal_outcome[which(release_timediff >= (-720)), 6] <- 1
  # temporal_outcome[which(release_timediff < (-720)), 6] <- 2
  # 
  # ## Release location test
  # if(!is.null(shp_b)) {
  #   temporal_outcome[, 7] <- ifelse(dist[1] > 500 &
  #                                     sum(is.na(over(ll_r, shp_b))) > 0, 2, 1)
  # } else {
  #   temporal_outcome[, 7] <- ifelse(dist[1] > 500, 2, 1)
  # }
  # 
  # ## Detection QC
  # ones <- as.numeric(rowSums(temporal_outcome[, c(1:5)] == 1))
  # temporal_outcome[which(ones <= 2), 8] <- 4
  # temporal_outcome[which(ones == 3), 8] <- 3
  # temporal_outcome[which(ones == 4), 8] <- 2
  # temporal_outcome[which(ones == 5), 8] <- 1
  # temporal_outcome$Velocity_QC <- as.numeric(temporal_outcome$Velocity_QC)
  # temporal_outcome$Distance_QC <- as.numeric(temporal_outcome$Distance_QC)
  # 
  # x <- x %>%
  #   rename(receiver_deployment_longitude = longitude,
  #          receiver_deployment_latitude = latitude)
  # 
  # return(bind_cols(x, temporal_outcome))
  
}