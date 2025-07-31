##' @title Initiates IMOS variable extraction process
##'
##' @description Help funtion to process data either in parallel or not
##'
##' @param input IMOS netCDF detail dataframe created with remote_urls
##' @param data_details Spatial and temporal details of data extraction, created with ext_find
##' @param .fill_gaps Should NAs be filled? 
##' @param .buffer Size of the buffer area to be user to fill NAs
##' @param env_buffer distance (in decimal degrees) to expand the study area beyond the coordinates to extract environmental data. Default value is 1°.
##' @param .parallel Should data extraction be performed in parallel?
##'
##' @details Internal help function to initiate IMOS data extraction
##'
##' @return Dataframe containing environmental data
##'
##' @keywords internal

remote_open <- function(input, data_details, env_buffer,
	.fill_gaps, .buffer, .parallel, .ncores, var_name) {

	# Check if area is too small for data extraction: add 1° each side otherwise
	xrange <- as.numeric(data_details$ext[1] - data_details$ext[2])
		if (xrange < 0)
			xrange <- xrange * -1
	yrange <- as.numeric(data_details$ext[3] - data_details$ext[4])
		if (yrange < 0)
			yrange <- yrange * -1
	if (xrange < 1 & yrange < 1) {
		data_details$ext[1] <- data_details$ext[1] - 1
		data_details$ext[2] <- data_details$ext[2] + 1
		data_details$ext[3] <- data_details$ext[3] - 1
		data_details$ext[4] <- data_details$ext[4] + 1
	}
	if (var_name %in% c("bathy", "dist_to_land")) {
		pos_sf <- 
			data_details$unique_positions %>%
			sf::st_as_sf(coords = c(1,2), crs = 4326, remove = FALSE)# %>% 
		ext_matrix <- suppressWarnings(terra::extract(input, pos_sf))
	    df_output <- data_details$unique_positions
	    df_output$var <- ext_matrix[,2]
	    names(df_output)[ncol(df_output)] <- var_name	 
	} else {
		if (.parallel) {		
			# Set up parallel processing
			`%dopar%` <- foreach::`%dopar%`
		    if (is.null(.ncores)) {
		      .ncores = parallel::detectCores()
		      cl <- parallel::makeCluster(.ncores[1]-1)
		      doParallel::registerDoParallel(cl)
		    } else {
		      cl <- parallel::makeCluster(.ncores)
		      doParallel::registerDoParallel(cl)
		    }
		    # Process data by year: avoid memory kill!
		    years <- unique(format(input$date, "%Y"))
		    input$year <- format(input$date, "%Y")
		    pb <- txtProgressBar(min = 0, max = length(years), initial = 0, style = 3, width = 50)
		    df_output <- NULL
		    for (i in 1:length(years)) {
		    	input_run <- input[which(input$year == years[i]),]
		    	lon.min <- as.numeric(data_details$ext[1]) - env_buffer
				lon.max <- as.numeric(data_details$ext[2]) + env_buffer
				lat.min <- as.numeric(data_details$ext[3]) - env_buffer
	    		lat.max <- as.numeric(data_details$ext[4]) + env_buffer
			    df_save <- foreach::foreach(ii = 1:nrow(input_run),
			    	.combine = rbind,
			    	.packages = c("remora", "dplyr", "tidync")) %dopar% {
			            extract_func(dataset = input_run[ii,],
			            	data_details = data_details,
			            	lon.min = lon.min,
							lon.max = lon.max,
					    	lat.min = lat.min,
	    					lat.max = lat.max,  
			            	var_name = var_name,
			            	.fill_gaps = .fill_gaps, 
			            	.buffer = .buffer)
			    }
			    df_output <- rbind(df_output, df_save)
			   	setTxtProgressBar(pb, i) 
			   	gc()
			}
			close(pb)
		parallel::stopCluster(cl)
		} else {
			# Load environmental data and extract 
			df_output <- NULL
			pb <- txtProgressBar(min = 0, max = nrow(input), initial = 0, style = 3, width = 50)
			for (i in 1:nrow(input)) {
				df_exp <- extract_func(dataset = input[i,],
					data_details = data_details,
					lon.min = as.numeric(data_details$ext[1]) - env_buffer,
					lon.max = as.numeric(data_details$ext[2]) + env_buffer,
				    lat.min = as.numeric(data_details$ext[3]) - env_buffer,
    				lat.max = as.numeric(data_details$ext[4]) + env_buffer,  
			        var_name = var_name,
			        .fill_gaps = .fill_gaps,
			        .buffer = .buffer)	
				df_output <- rbind(df_output, df_exp)
				setTxtProgressBar(pb, i) 
			}
			close(pb)
		}
	}
	return(df_output)
}

