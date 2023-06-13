##' @title test the validity of individual detections for tags detected more than once
##'
##' @description Subjects tag detections to 8 quality control tests and appends results to each detection record
##'
##' @param x a list of un-QC'd data for each tag deployment
##' @param Lcheck (logical; default TRUE) test for receiver_deployment_latitudes
##' in N hemisphere at correct to S hemisphere. Set to FALSE for QC on N hemisphere data
##' @param logfile path to logfile; default is the working directory
##' @param tests_vector ...
##' @param data_format currently, "imos" (default) or "otn"
##' @param shapefile A shapefile for species home ranges. Can be left null but some tests may not run. 
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


qc <- function(x, Lcheck = TRUE, logfile, tests_vector = c("FDA_QC",
                                                           "Velocity_QC",
                                                           "Distance_QC",
                                                           "DetectionDistribution_QC",
                                                           "DistanceRelease_QC",
                                                           "ReleaseDate_QC",
                                                           "ReleaseLocation_QC",
                                                           "Detection_QC"),
               data_format = "imos",
               shapefile = NULL,
               fda_type = "time-diff") {
  if(!is.data.frame(x)) stop("x must be a data.frame")
  ## Configure output processed data file
  temporal_outcome <- data.frame(matrix(ncol = length(tests_vector), nrow = nrow(x)))
  colnames(temporal_outcome) <- tests_vector
  
  #If our dataframe is only one entry long, quit.
  if(nrow(x) == 1) {
    message("Dataframe must have more than one row for QC.")
    return(bind_cols(x, temporal_outcome))
  }
  
  #Start by removing any rows that have NAs in the datetime, lat, or long columns. I'd like to return to this function and make something
  #a little more comprehensive but for now I've just sliced out the code and hived it off into its own function for cleanliness' sake.
  #message("Removing NAs")
  x <- qc_remove_nas(x)
  #message("NAs removed.")
  
  #I've commented out this check for now, I think we're not going to want this what with our intended global scope. 
  ## IDJ - uncommented as latitude check is useful for IMOS data. I've added to the conditional so this only gets 
  ##  implemented if the data_format = "imos"
  ## check for & correct any lat's incorrectly in N hemisphere
  if(any(x$latitude > 0) & Lcheck & data_format == "imos") {
   ## how many incorrect records
   n <- sum(x$latitude > 0)
   ## write to logfile
   write(paste0(x$filename[1],
               ":  ", n, " receiver_deployment_latitude(s) incorrectly entered in N hemisphere; corrected in QC output"),
         file = logfile,
         append = TRUE)

     x <- x %>% mutate(latitude = ifelse(latitude > 0, -1 * latitude, latitude))
  }
  
  #Removed sections flagged as redundant. - BD 30/06/2022
  message(x$filename[1])
  write(paste0(x$filename[1],":  ", " Grabbing species shapefile."),
        file = logfile,
        append = TRUE)
  if(data_format == "imos") {
    spe <- unique(x$species_scientific_name)
    CAAB_species_id <- unique(x$CAAB_species_id)
  
    ## Find corresponding ALA shapefile based on species name
    shp_b <- NULL
    if (!is.na(spe) & !is.na(CAAB_species_id))
      shp_b <- try(get_expert_distribution_shp(CAAB_species_id, spe))
  
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
  
  } else if (data_format == "otn") {
    if(is.null(shapefile)) {
      message("WARNING: No shapefile supplied. Some tests may not run.")
    }
    shp_b <- shapefile
  }
  write(paste0(x$filename[1],
               ":  "," Shapefile Grab done."),
        file = logfile,
        append = TRUE)
  
  ## Converts unique sets of lat/lon detection coordinates and release lat/lon 
  ##  coordinates to SpatialPoints to test subsequently whether or not detections 
  ##  are in distribution range
  if (!is.null(shp_b)) {
    #message("shapefile not null, starting lat/lon conversion to SpatialPoints")

    ll <- unique(data.frame(x$longitude, x$latitude))
    
    SpatialPoints(ll, proj4string = CRS("EPSG:4326"))
    #coordinates(ll) <- ~ x.longitude + x.latitude
    #message("Coordinates set")
    #proj4string(ll) <- proj4string(shp_b)
    #st_crs(ll) <- CRS("EPSG:4326")
    #message("projection string set") 
    
    ll_r <- NULL
    if (!is.na(x$transmitter_deployment_longitude[1])) {
      ll_r <-
        data.frame(x$transmitter_deployment_longitude[1], x$transmitter_deployment_latitude[1])
      #message("Step two")
      ll_r <- SpatialPoints(ll_r, proj4string = CRS("EPSG:4326"))
      #coordinates(ll_r) <-
      #  ~ x.transmitter_deployment_longitude.1. + x.transmitter_deployment_latitude.1.
      #message("Step three")
      #proj4string(ll_r) <- suppressWarnings(proj4string(shp_b))
      #st_crs(ll_r) <- CRS("EPSG:4326")
    }
  }
  message("Conversion done.")

  
	## False Detection Algorithm test
  if("FDA_QC" %in% colnames(temporal_outcome))
  {
    write(paste0(x$filename[1],
                 ":  ", " Starting false detection test"),
          file = logfile,
          append = TRUE)
    
    #Trying to force pincock into working as a stopgap.
    x$transmitter_codespace <- x$transmitter_id
    x$receiver_sn <- x$receiver_id
    x$detection_timestamp_utc <- x$detection_datetime
    
    write(paste0(x$filename[1],
                 ":  ", " Stopgap columns set"),
          file = logfile,
          append = TRUE)
    
    temporal_outcome <- qc_false_detection_test(x, temporal_outcome, type = fda_type)
    write(paste0(x$filename[1],
                 ":  ", " False detection test done."),
          file = logfile,
          append = TRUE)
  }
	
  
	#bathyUrl = "https://upwell.pfeg.noaa.gov/erddap/griddap/etopo5.geotif?ROSE%5B(40):1:(50)%5D%5B(280):1:(320)%5D"
  #message("Starting dist/velocity tests")
  #Commented out to test if I can get the rest of this running
	## Distance and Velocity tests
  dist <- NULL
  if(any(is.na(x$transmitter_deployment_longitude)) | any(is.na(x$transmitter_deployment_longitude))) {
    write(paste0(x$filename[1],
                 ":  ", " Not enough data for some QC checks."),
          file = logfile,
          append = TRUE)
  }
  else
  {
  	position <- data.frame(longitude = c(x$transmitter_deployment_longitude[1], x$longitude),
  		                       latitude = c(x$transmitter_deployment_latitude[1], x$latitude))
  	
    #message("position set")

    #Distance temporarily commented out. We're going to reimplement a lot of this and that includes the shortest_dist calculation, which
    #right now chokes out the rest of the code. So this blows away most of the checks, but it lets the code run so that we can see what
    #happens when the OTN data goes thru it. 
    #dist <- NULL
    # tr is included in the sysdata, but if someone brings their own shapefile then we have to create our own. 
    ## IDJ: add conditional on data_format
    ## BD: added a check to not run the OTN version of this if shp_b is null
    if(data_format == "otn" & !is.null(shp_b))
    {
      dist <- switch(data_format,
                     imos = {
                       shortest_dist(position,
                                     x$installation_name,
                                     rast = Aust_raster,
                                     tr = tr)
                     },
                     otn = {
                       transition_layer <- glatos::make_transition2(shp_b)
                       tr <- transition_layer$transition
                       #dist <- NULL
                       shortest_dist(position,
                                     x$installation_name,
                                     rast = world_raster_sub,
                                     tr = tr) 
                     })
      #message("shortest dist calculated")
    }
  }
    if("Velocity_QC" %in% colnames(temporal_outcome) & !is.null(dist)) {
      write(paste0(x$filename[1],
                   ":  ", " Running velocity check"),
            file = logfile,
            append = TRUE)
    	temporal_outcome <- qc_test_velocity(x, temporal_outcome, dist)
    }
  
    if("Distance_QC" %in% colnames(temporal_outcome) & !is.null(dist)) {
      write(paste0(x$filename[1],
                   ":  ", " Running distance check"),
            file = logfile,
            append = TRUE)
      temporal_outcome <- qc_test_distance(x, temporal_outcome, dist)
    }

		message("Dist/velocity tests done.")
		## Detection distribution test
    if("DetectionDistribution_QC" %in% colnames(temporal_outcome) & !is.null(dist) & !is.null(shp_b)) {
      write(paste0(x$filename[1],
                   ":  ", " Running detection distribution check."),
            file = logfile,
            append = TRUE)
      temporal_outcome <- qc_test_det_distro(x, ll, temporal_outcome, shp_b)
    }

    if("DistanceRelease_QC" %in% colnames(temporal_outcome))
    {
      write(paste0(x$filename[1],
                   ":  ", " Running distance from release check."),
            file = logfile,
            append = TRUE)
      temporal_outcome <- qc_test_dist_release(x, temporal_outcome)
    }
		
    if("ReleaseDate_QC" %in% colnames(temporal_outcome)) {
      ## Release date before detection date
      write(paste0(x$filename[1],
                   ":  ", " Running release date check."),
            file = logfile,
            append = TRUE)
      temporal_outcome <- qc_test_release_time_diff(x, temporal_outcome)
    }

    if("ReleaseLocation_QC" %in% colnames(temporal_outcome) & !is.null(dist) & !is.null(shp_b)) {
      write(paste0(x$filename[1],
                   ":  ", " Running release location check."),
            file = logfile,
            append = TRUE)
      temporal_outcome <- qc_release_location_test(x, temporal_outcome, shp_b, dist, ll_r)
    }
		## it might be better to keep all tests in temporal_outcome & just ensure
		##  tests that are turned off return NA values, that way output QC object always
		##  has same dims - otherwise this will cause IMOS AODN incoming server checks to
		##  reject QC'd data.
		
		## Detection QC
    temporal_outcome <- qc_detection_qc(temporal_outcome)
    
	x <- x %>%
	  dplyr::rename(receiver_deployment_longitude = longitude,
	         receiver_deployment_latitude = latitude)
	
	message("Done and returning")
	return(bind_cols(x, temporal_outcome))

}
