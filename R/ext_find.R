##' @title Identify spatial and temporal extents of data
##'
##' @description Determines spatial and temporal ranges for data extraction
##'
##' @param .df Dataframe given as input to extract environmental data for
##' @param .X Column containing logitudes
##' @param .Y Column containing latitudes
##' @param .datetime Column containing timestamps
##' @param .full_timeperiod Whether data extraction should be performed for the entire period or not
##'
##' @details Internal function to determine spatial and temporal ranges for data extraction
##'
##' @return List of objects 
##'
##' @keywords internal

ext_find <- function(.df, .X, .Y, .datetime, .full_timeperiod, verbose) {
	## define date range
	unique_dates <- 
	    .df %>%
	    dplyr::mutate(date = date(!!as.name(.datetime))) %>%
	    dplyr::distinct(date) %>%
	    dplyr::pull(date) 	  
	date_range <- range(unique_dates)
	  
	## define spatial extent and extend by 40%
	study_extent <- terra::ext(c(min(.df[[.X]]), max(.df[[.X]]), min(.df[[.Y]]), max(.df[[.Y]]))) * 1.4
	 
	## define unique positions (for quicker environmental variable extraction)
	unique_positions <-
		dplyr::ungroup(.df) %>% 
		dplyr::mutate(date = date(!!as.name(.datetime))) %>%
		dplyr::distinct(!!as.name(.X), !!as.name(.Y), date) %>% 
	  dplyr::select(!!as.name(.X), !!as.name(.Y), date)
	  
	## define dates of detection and date range and catalog all dates between start and end if .full_timeperiod = TRUE
	if(.full_timeperiod){
		if(verbose){
	    	message("Extracting environmental data for each day between ", 
	        	date_range[1], " and ", date_range[2], " (", 
	        	difftime(date_range[2], date_range[1], units = "days"), " days)",
	            "\nThis may take a little while...") 
	    }
	    dates <- seq(date_range[1], date_range[2], by = 1)
	    # update unique_positions to include all days
	    all_positions <- expand.grid(date = dates, locs = paste(unique_positions$receiver_deployment_longitude,
	    	unique_positions$receiver_deployment_latitude, sep = "_"))
	    all_positions$receiver_deployment_longitude <- as.numeric(stringr::str_split_fixed(all_positions$locs, pattern = "_", n = 2)[,1])
	    all_positions$receiver_deployment_latitude <- as.numeric(stringr::str_split_fixed(all_positions$locs, pattern = "_", n = 2)[,2])
	    all_positions <- all_positions[,c(3,4,1)]
	    unique_positions <- all_positions
	 } else {
		if(verbose){
	    	message("Extracting environmental data only on days detections were present; between ", 
	        	date_range[1], " and ", date_range[2], " (", 
	            length(unique_dates), " days)",
	            "\nThis may take a little while...")
	    }
	    dates <- unique_dates
	}
	return(list(ext = study_extent, dates = dates, unique_positions = unique_positions))
}