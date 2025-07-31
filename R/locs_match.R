##' @title Match extracted IMOS environmental data with detections
##'
##' @description Help funtion to match environmental and detection datasets. Different workflows if data is extracted for full timeperiods.
##'
##' @param aux_df Dataset containing detections
##' @param aux_env Dateset contatining environmental data
##' @param X name of column with X coordinate or longitude (EPSG 4326)
##' @param Y name of column with Y coordinate or latitude (EPSG 4326)
##' @param datetime name of column with date time stamp (Coordinated Universal 
##' @param full_timeperiod should environmental variables be extracted for all days
##' @param station_name if full_timeperiod = TRUE, please provide the column that identifies the name of the 
##' acoustic stations
##' @param var_name Name of the variable of interest
##'
##' @details Internal help function to match datasets
##'
##' @return Dataframe containing extracted environmental data
##'
##' @keywords internal

locs_match <- function(aux_df, aux_env, X, Y, datetime, full_timeperiod, station_name, var_name) {
	# Aux indexing objects
	# Locations file
	index.lon <- which(names(aux_df) == X)
	index.lat <- which(names(aux_df) == Y)
	index.time <- which(names(aux_df) == datetime)
	aux_index <- aux_df[,c(index.time, index.lon, index.lat)]
	names(aux_index) <- c("time", "x", "y")
	aux_index$time <- as.Date(aux_index$time)
	aux_index$aux <- paste(aux_index$time, aux_index$x, aux_index$y, sep = "_")
	aux_df$aux <- aux_index$aux
	# Environmental file
	index.lon <- which(names(aux_env) == X)
	index.lat <- which(names(aux_env) == Y)
	index.time <- which(names(aux_env) == "date")
	aux_index <- aux_env[,c(index.time, index.lon, index.lat)]
	names(aux_index) <- c("time", "x", "y")
	aux_index$aux <- paste(aux_index$time, aux_index$x, aux_index$y, sep = "_")
	aux_env$aux <- aux_index$aux
	# Match data
	if (full_timeperiod) {
		aux_detecs <- aux_df %>%
			dplyr::group_by(aux) %>%
			dplyr::summarise(number_detections = n())
		aux_env$number_detections <- aux_detecs$number_detections[match(aux_env$aux, aux_detecs$aux)]
		aux_env$number_detections[is.na(aux_env$number_detections)] <- 0
		# Match station names
		aux_env$aux <- stringr::str_split_fixed(aux_env$aux, pattern = "_", n = 2)[,2]
		aux_df$aux <- stringr::str_split_fixed(aux_df$aux, pattern = "_", n = 2)[,2]
		names(aux_df)[which(names(aux_df) == station_name)] <- "station_name"
		aux_env$station_name <- aux_df$station_name[match(aux_env$aux, aux_df$aux)]
		if (var_name == "rs_current") {
			aux_env <- aux_env[,c('date', X, Y, "station_name", 
				"rs_gsla", "rs_vcur", "rs_ucur", "rs_current_velocity", "rs_current_bearing", 
				"number_detections")]
			names(aux_env)[which(names(aux_env) == "station_name")] <- station_name
		} else {
			aux_env <- aux_env[,c('date', X, Y, "station_name", var_name, "number_detections")]
			names(aux_env)[which(names(aux_env) == "station_name")] <- station_name
		}
		df_save <- tibble::as_tibble(aux_env)
	} else {
		if (var_name == "rs_current") {
			aux_df$date <- aux_env$date[match(aux_df$aux, aux_env$aux)]
			aux_df$rs_gsla <- aux_env$rs_gsla[match(aux_df$aux, aux_env$aux)]
			aux_df$rs_vcur <- aux_env$rs_vcur[match(aux_df$aux, aux_env$aux)]
			aux_df$rs_ucur <- aux_env$rs_ucur[match(aux_df$aux, aux_env$aux)]
			aux_df$rs_current_velocity <- aux_env$rs_current_velocity[match(aux_df$aux, aux_env$aux)]
			aux_df$rs_current_bearing <- aux_env$rs_current_bearing[match(aux_df$aux, aux_env$aux)]
		} else {
			names(aux_env)[4] <- "var"
			aux_df$date <- aux_env$date[match(aux_df$aux, aux_env$aux)]
			aux_df$var <- aux_env$var[match(aux_df$aux, aux_env$aux)]
			names(aux_df)[ncol(aux_df)] <- var_name
		}
		aux_df <- aux_df[,-which(names(aux_df) == "aux")]
		df_save <- aux_df
	}
	return(df_save)
}