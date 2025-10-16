#' @title Extract IMOS environmental data remotely
#'
#' @description Remotely open and crop IMOS netCDF files and extract data
#'
#' @param dataset Environmental data details (URL and netCDF variable names)
#' @param depth For Bluelink variables, the depth of interest to extract data for
#' @param data_details Spatial and temporal details of data extraction, created with ext_find
#' @param lon.min Minimum longitude for data extraction
#' @param lon.max Maximum longitude for data extraction
#' @param lat.min Minimum latitude for data extraction
#' @param lat.max Maximum latitude for data extraction
#' @param var_name Name of environmental variable of interest
#' @param .fill_gaps Should NAs be filled? 
#' @param .buffer Size of the buffer area to be user to fill NAs
#'
#' @details Internal function to open and extract IMOS environmental data
#'
#' @importFrom dplyr %>%
#'
#' @keywords internal

extract_func <- function(dataset, depth, data_details, lon.min, lon.max, lat.min, lat.max,
  var_name, .fill_gaps, .buffer) {

	# Open netcdf remotely and crop for area of interest
  remote_nc <- tryCatch({suppressMessages(tidync::tidync(dataset$url_name))},
      error = function(msg){
        remote_nc <- ("File not found")
      }
  )
  if (length(remote_nc) == 1) {
    dataset$url_name <- stringr::str_replace_all(dataset$url_name, 
      pattern = "000000",
      replacement = "060000")
  remote_nc <-  tryCatch({suppressMessages(tidync::tidync(dataset$url_name))},
      error = function(msg){
        remote_nc <- ("File not found")
      }
    )
  }

  # Add NAs when netCDF file is not found
  if (inherits(remote_nc, what = "character")) {
    # Environmental data not available
    if (var_name %in% c("rs_current", "BRAN_wind", "BRAN_cur")) {
        if (var_name == "rs_current") {
          df_exp <- data_details$unique_positions %>%
          dplyr::filter(date == dataset$date) %>%
          dplyr::mutate(rs_gsla = NaN,
            rs_vcur = NaN,
            rs_ucur = NaN,
            rs_current_velocity = NaN,
            rs_current_bearing = NaN)
        }
        if (var_name == "BRAN_wind") {
          df_exp <- data_details$unique_positions %>%
          dplyr::filter(date == dataset$date) %>%
          dplyr::mutate(BRAN_vwind = NaN,
            BRAN_uwind = NaN,
            BRAN_wind_velocity = NaN,
            BRAN_wind_bearing = NaN)
        }
        if (var_name == "BRAN_cur") {
          df_exp <- data_details$unique_positions %>%
          dplyr::filter(date == dataset$date) %>%
          dplyr::mutate(BRAN_vcur = NaN,
            BRAN_ucur = NaN,
            BRAN_current_velocity = NaN,
            BRAN_current_bearing = NaN)
        }
      } else {
        df_exp <- data_details$unique_positions %>%
          dplyr::filter(date == dataset$date) %>%
          dplyr::mutate(var = NaN)
        names(df_exp)[ncol(df_exp)] <- var_name
      }
  } else { # Extract info when netCDF file is found    
    # Different variables have different lat/lon names:
    if (var_name %in% c("rs_sst", "rs_sst_interpolated")) {
      remote_df <- remote_nc %>%
        tidync::activate(dataset$layer) %>%
        tidync::hyper_filter(lon = lon > lon.min & lon < lon.max,
          lat = lat > lat.min & lat < lat.max) %>% 
        tidync::hyper_tibble()
      names(remote_df)[1] <- "var"
      remote_df$var <- remote_df$var - 273.15
      names(remote_df)[1] <- var_name     
    }
    if (var_name %in% c("rs_chl", "rs_turbidity", "rs_npp")) {
      remote_df <- remote_nc %>%
        tidync::activate(dataset$layer) %>%
        tidync::hyper_filter(longitude = longitude > lon.min & longitude < lon.max,
          latitude = latitude > lat.min & latitude < lat.max) %>% 
        tidync::hyper_tibble()
      names(remote_df)[1] <- var_name     
    }
    if (var_name == "rs_current") {
      remote_df <- remote_nc %>%
        tidync::activate("UCUR") %>%
        tidync::hyper_filter(LONGITUDE = LONGITUDE > lon.min & LONGITUDE < lon.max,
          LATITUDE = LATITUDE > lat.min & LATITUDE < lat.max) %>% 
        tidync::hyper_tibble()
      remote_df2 <- remote_nc %>%
        tidync::activate("VCUR") %>%
        tidync::hyper_filter(LONGITUDE = LONGITUDE > lon.min & LONGITUDE < lon.max,
          LATITUDE = LATITUDE > lat.min & LATITUDE < lat.max) %>% 
        tidync::hyper_tibble()
      remote_df3 <- remote_nc %>%
        tidync::activate("GSLA") %>%
        tidync::hyper_filter(LONGITUDE = LONGITUDE > lon.min & LONGITUDE < lon.max,
          LATITUDE = LATITUDE > lat.min & LATITUDE < lat.max) %>% 
        tidync::hyper_tibble()
      # Match values by location (NAs sometimes present in GSLA)
      remote_df$aux <- paste(remote_df$LATITUDE, remote_df$LONGITUDE, sep = "_")
      remote_df2$aux <- paste(remote_df2$LATITUDE, remote_df2$LONGITUDE, sep = "_")
      remote_df3$aux <- paste(remote_df3$LATITUDE, remote_df3$LONGITUDE, sep = "_")
      remote_df$VCUR <- remote_df2$VCUR[match(remote_df$aux, remote_df2$aux)]      
      remote_df$GSLA <- remote_df3$GSLA[match(remote_df$aux, remote_df3$aux)]      
      remote_df <- remote_df[,-which(names(remote_df) == "aux")]
      remote_df$Dir <- windDir(u = remote_df$UCUR, v = remote_df$VCUR)
      remote_df$Spd <- windSpd(u = remote_df$UCUR, v = remote_df$VCUR)
    }

    if (var_name %in% c('BRAN_temp', 'BRAN_salt', 'BRAN_ssh', 'BRAN_mld', 'BRAN_cur', 'BRAN_wcur', 'BRAN_wind')) {
      if (var_name == "BRAN_wind") {
        remote_df <- remote_nc %>%
          tidync::activate("u_atm") %>%        
          tidync::hyper_filter(lon = lon > lon.min & lon < lon.max,
          lat = lat > lat.min & lat < lat.max) %>% 
          tidync::hyper_tibble()
        remote_df2 <- remote_nc %>%
          tidync::activate("v_atm") %>%
          tidync::hyper_filter(lon = lon > lon.min & lon < lon.max,
          lat = lat > lat.min & lat < lat.max) %>% 
          tidync::hyper_tibble()
        # Match values by location 
        remote_df$aux <- paste(remote_df$lat, remote_df$lon, remote_df$Time, sep = "_")
        remote_df2$aux <- paste(remote_df2$lat, remote_df2$lon, remote_df2$Time, sep = "_")
        remote_df$v_atm <- remote_df2$v_atm[match(remote_df$aux, remote_df2$aux)]      
        remote_df$Dir <- windDir(u = remote_df$u_atm, v = remote_df$v_atm)
        remote_df$Spd <- windSpd(u = remote_df$u_atm, v = remote_df$v_atm)
        remote_df$Time <- as.Date(substr(remote_df$Time, 1, 10), tz = "UTC")
        remote_df <- subset(remote_df, Time == as.Date(dataset$date, tz = "UTC"))
        remote_df <- remote_df[,-which(names(remote_df) %in% c("aux", "Time"))]
      }
      if (var_name == "BRAN_cur") {
        # Open also VCUR
        remote_nc2 <- suppressMessages(tidync::tidync(stringr::str_replace_all(dataset$url_name,
          pattern = "ocean_u", replacement = "ocean_v")))
        # Find nearest depth layer of interest
        bran_depth <- remote_nc %>% 
          tidync::activate("st_ocean") %>% 
          tidync::hyper_tibble()
        bran_depth$st_ocean <- as.numeric(bran_depth$st_ocean) * -1
        if (depth > 0)
          depth <- depth * -1
        depth_layer <- which.min(abs(bran_depth$st_ocean - depth))
        # Subset data for only depth layer of interest
        remote_nc <- remote_nc %>% 
          tidync::hyper_filter(st_ocean = index == depth_layer)
        remote_nc2 <- remote_nc2 %>% 
          tidync::hyper_filter(st_ocean = index == depth_layer)
        # Crop for study area
        remote_df <- remote_nc %>%
          tidync::activate("u") %>%
          tidync::hyper_filter(xu_ocean = xu_ocean > lon.min & xu_ocean < lon.max,
            yu_ocean = yu_ocean > lat.min & yu_ocean < lat.max) %>% 
          tidync::hyper_tibble()
        remote_df2 <- remote_nc2 %>%
          tidync::activate("v") %>%
          tidync::hyper_filter(xu_ocean = xu_ocean > lon.min & xu_ocean < lon.max,
            yu_ocean = yu_ocean > lat.min & yu_ocean < lat.max) %>% 
          tidync::hyper_tibble()
        # Match values by location (NAs sometimes present in GSLA)
        remote_df$aux <- paste(remote_df$yu_ocean, remote_df$xu_ocean, remote_df$Time, sep = "_")
        remote_df2$aux <- paste(remote_df2$yu_ocean, remote_df2$xu_ocean, remote_df2$Time, sep = "_")
        remote_df$v <- remote_df2$v[match(remote_df$aux, remote_df2$aux)]      
        remote_df$Dir <- windDir(u = remote_df$u, v = remote_df$v)
        remote_df$Spd <- windSpd(u = remote_df$u, v = remote_df$v)
        remote_df$Time <- as.Date(substr(remote_df$Time, 1, 10), tz = "UTC")
        remote_df <- subset(remote_df, Time == as.Date(dataset$date, tz = "UTC"))
        remote_df <- remote_df[,-which(names(remote_df) %in% c("aux", "Time"))]
      }

      if (var_name == 'BRAN_wcur') {
        # Find nearest depth layer of interest
        bran_depth <- remote_nc %>% 
          tidync::activate("sw_ocean") %>% 
          tidync::hyper_tibble()
        bran_depth$sw_ocean <- as.numeric(bran_depth$sw_ocean)
        if (depth > 0)
          depth <- depth * -1
        depth_layer <- which.min(abs(bran_depth$sw_ocean - depth))
        # Subset data for only depth layer of interest
        remote_nc <- remote_nc %>% 
          tidync::hyper_filter(sw_ocean = index == depth_layer)
        # Crop for study area
        remote_df <- remote_nc %>%
          tidync::activate(dataset$layer) %>%
          tidync::hyper_filter(xt_ocean = xt_ocean > lon.min & xt_ocean < lon.max,
            yt_ocean = yt_ocean > lat.min & yt_ocean < lat.max) %>% 
          tidync::hyper_tibble()
        names(remote_df)[1] <- var_name    
        remote_df$Time <- as.Date(substr(remote_df$Time, 1, 10), tz = "UTC")
        remote_df <- subset(remote_df, Time == as.Date(dataset$date, tz = "UTC"))
        remote_df <- remote_df[,1:3]
      }

      if (var_name %in% c('BRAN_temp', 'BRAN_salt')) {
        # Find nearest depth layer of interest
        bran_depth <- remote_nc %>% 
          tidync::activate("st_ocean") %>% 
          tidync::hyper_tibble()
        bran_depth$st_ocean <- as.numeric(bran_depth$st_ocean) * -1
        if (depth > 0)
          depth <- depth * -1
        depth_layer <- which.min(abs(bran_depth$st_ocean - depth))
        # Subset data for only depth layer of interest
        remote_nc <- remote_nc %>% 
          tidync::hyper_filter(st_ocean = index == depth_layer)
        # Crop for study area
        remote_df <- remote_nc %>%
          tidync::activate(dataset$layer) %>%
          tidync::hyper_filter(xt_ocean = xt_ocean > lon.min & xt_ocean < lon.max,
            yt_ocean = yt_ocean > lat.min & yt_ocean < lat.max) %>% 
          tidync::hyper_tibble()
        names(remote_df)[1] <- var_name    
        remote_df$Time <- as.Date(substr(remote_df$Time, 1, 10), tz = "UTC")
        remote_df <- subset(remote_df, Time == as.Date(dataset$date, tz = "UTC"))
        remote_df <- remote_df[,1:3]
      }

      if (var_name %in% c('BRAN_ssh', 'BRAN_mld')) {
        # Crop for study area
        remote_df <- remote_nc %>%
          tidync::activate(dataset$layer) %>%
          tidync::hyper_filter(xt_ocean = xt_ocean > lon.min & xt_ocean < lon.max,
            yt_ocean = yt_ocean > lat.min & yt_ocean < lat.max) %>% 
          tidync::hyper_tibble()
        names(remote_df)[1] <- var_name    
        remote_df$Time <- as.Date(substr(remote_df$Time, 1, 10), tz = "UTC")
        remote_df <- subset(remote_df, Time == as.Date(dataset$date, tz = "UTC"))
        remote_df <- remote_df[,1:3]
      }

    }

    # If empty raster = cloud cover (add NAs)
    if (nrow(remote_df) == 0) {
      if (var_name %in% c("rs_current", "BRAN_wind", "BRAN_cur")) {
        if (var_name == "rs_current") {
          df_exp <- data_details$unique_positions %>%
          dplyr::filter(date == dataset$date) %>%
          dplyr::mutate(rs_gsla = NaN,
            rs_vcur = NaN,
            rs_ucur = NaN,
            rs_current_velocity = NaN,
            rs_current_bearing = NaN)
        }
        if (var_name == "BRAN_wind") {
          df_exp <- data_details$unique_positions %>%
          dplyr::filter(date == dataset$date) %>%
          dplyr::mutate(BRAN_vwind = NaN,
            BRAN_uwind = NaN,
            BRAN_wind_velocity = NaN,
            BRAN_wind_bearing = NaN)
        } 
        if (var_name == "BRAN_cur") {
          df_exp <- data_details$unique_positions %>%
          dplyr::filter(date == dataset$date) %>%
          dplyr::mutate(BRAN_vcur = NaN,
            BRAN_ucur = NaN,
            BRAN_current_velocity = NaN,
            BRAN_current_bearing = NaN)
        }    
      } else {
        df_exp <- data_details$unique_positions %>%
          dplyr::filter(date == dataset$date) %>%
          dplyr::mutate(var = NaN)
        names(df_exp)[ncol(df_exp)] <- var_name
      }
    } else {
      # Convert to raster for data extraction
      names(remote_df)[2:3] <- c("LON", "LAT")
      remote_df$LON <- as.numeric(remote_df$LON)
      remote_df$LAT <- as.numeric(remote_df$LAT)
      # Check enough number of dimensions are available
      lats <- length(unique(remote_df$LAT))
      lons <- length(unique(remote_df$LON))
      if (lats == 1 | lons == 1) {
        if (var_name %in% c("rs_current", "BRAN_wind", "BRAN_cur")) {
          if (var_name == "rs_current") {
            df_exp <- data_details$unique_positions %>%
            dplyr::filter(date == dataset$date) %>%
            dplyr::mutate(rs_gsla = NaN,
              rs_vcur = NaN,
              rs_ucur = NaN,
              rs_current_velocity = NaN,
              rs_current_bearing = NaN)
          }
          if (var_name == "BRAN_wind") {
            df_exp <- data_details$unique_positions %>%
            dplyr::filter(date == dataset$date) %>%
            dplyr::mutate(BRAN_vwind = NaN,
              BRAN_uwind = NaN,
              BRAN_wind_velocity = NaN,
              BRAN_wind_bearing = NaN)
          }
          if (var_name == "BRAN_cur") {
            df_exp <- data_details$unique_positions %>%
            dplyr::filter(date == dataset$date) %>%
            dplyr::mutate(BRAN_vcur = NaN,
              BRAN_ucur = NaN,
              BRAN_current_velocity = NaN,
              BRAN_current_bearing = NaN)
          } 
      } else {
          df_exp <- data_details$unique_positions %>%
            dplyr::filter(date == dataset$date) %>%
            dplyr::mutate(var = NaN)
          names(df_exp)[ncol(df_exp)] <- var_name
        }
      } else {
        # Create data raster for data extraction
        if (var_name %in% c("rs_current", "BRAN_wind")) { # Multi-variables
          if (var_name == "rs_current") {
            remote_df <- remote_df[,c("LON", "LAT", "GSLA", "VCUR", 
            "UCUR", "Spd", "Dir")]
            remote_rast <- tidyterra::as_spatraster(x = remote_df,
              xycols = 1:2, crs = 4326, digits = 1)
          }
          if (var_name == "BRAN_wind") {
            remote_df <- remote_df[,c("LON", "LAT", "v_atm", 
            "u_atm", "Spd", "Dir")]
            names(remote_df)[3:4] <- c("VWIND", "UWIND")
            remote_rast <- tidyterra::as_spatraster(x = remote_df,
              xycols = 1:2, crs = 4326, digits = 1)
          }
          if (var_name == "BRAN_cur") {
            remote_df <- remote_df[,c("LON", "LAT", "v", 
            "u", "Spd", "Dir")]
            remote_rast <- tidyterra::as_spatraster(x = remote_df,
              xycols = 1:2, crs = 4326, digits = 1)
          }
        } else { # Single variables
          remote_rast <- tryCatch({suppressMessages(tidyterra::as_spatraster(x = remote_df,
            xycols = 2:3, crs = 4326, digits = 1))},
              error = function(msg){
                remote_rast <- ("Non-regular grid!")
              }
          )
          if (inherits(remote_rast, what = "character")) {
            # Use interpolation when cell sizes are not even
            aux_df <- remote_df[,c("LON", "LAT", var_name)]
            names(aux_df)[3] <- "var" 
            pts <- terra::vect(data.frame(x = aux_df$LON, 
              y = aux_df$LAT, 
              z = aux_df$var), 
              geom = c("x", "y"))
            spdf <- methods::as(pts, "Spatial")
            # Create gstat object for IDW
            gs <- gstat::gstat(formula = z~1, locations = spdf, nmax = 7, set = list(idp = 2.0))
            # Create raster grid
            remote_rast <- terra::rast(terra::ext(pts), resolution = 0.1)
            grd <- as.data.frame(terra::xyFromCell(remote_rast, 1:terra::ncell(remote_rast)))
            names(grd) <- c("x", "y")
            sp::coordinates(grd) <- ~x + y
            # Predict using gstat model
            invisible(utils::capture.output({
              idw_result <- terra::predict(gs, newdata = grd)
            }))
            # Convert to raster
            remote_rast[] <- idw_result$var1.pred
            names(remote_rast) <- var_name
          }
        }
        # Create vector of daily locations and extract data
        pos_sf <- 
          data_details$unique_positions %>%
            dplyr::filter(date == dataset$date) %>% 
              sf::st_as_sf(coords = c(1,2), crs = 4326, remove = FALSE)# %>% 
            ext_vals <- suppressWarnings(terra::extract(remote_rast, pos_sf))
            # Fill gaps
            if (.fill_gaps) {
              if (var_name %in% c("rs_current", "BRAN_wind", "BRAN_cur")) {
                index <- which(is.na(ext_vals[,2]))
                if (length(index) > 0) { # Only if gaps are present
                  pos_sf_buffer <- sf::st_buffer(pos_sf[index,], dist = .buffer)
                  ext_fills <- suppressWarnings(terra::extract(remote_rast, pos_sf_buffer, fun = median, na.rm = TRUE))
                  if (var_name == "rs_current") {
                    ext_vals[index, "GSLA"] <- ext_fills[, "GSLA"]
                    ext_vals[index, "VCUR"] <- ext_fills[, "VCUR"]
                    ext_vals[index, "UCUR"] <- ext_fills[, "UCUR"]
                    ext_vals[index, "Spd"] <- ext_fills[, "Spd"]
                    ext_vals[index, "Dir"] <- ext_fills[, "Dir"]
                    df_exp <- data_details$unique_positions %>%
                    dplyr::filter(date == dataset$date) %>%
                    dplyr::mutate(rs_gsla = ext_vals[,"GSLA"],
                      rs_vcur = ext_vals[,"VCUR"],
                      rs_ucur = ext_vals[,"UCUR"],
                      rs_current_velocity = ext_vals[,"Spd"],
                      rs_current_bearing = ext_vals[,"Dir"])      
                  }
                  if (var_name == "BRAN_wind") {
                    ext_vals[index, "VWIND"] <- ext_fills[, "VWIND"]
                    ext_vals[index, "UWIND"] <- ext_fills[, "UWIND"]
                    ext_vals[index, "Spd"] <- ext_fills[, "Spd"]
                    ext_vals[index, "Dir"] <- ext_fills[, "Dir"]
                    df_exp <- data_details$unique_positions %>%
                    dplyr::filter(date == dataset$date) %>%
                    dplyr::mutate(BRAN_vwind = ext_vals[,"VWIND"],
                      BRAN_uwind = ext_vals[,"UWIND"],
                      BRAN_wind_velocity = ext_vals[,"Spd"],
                      BRAN_wind_bearing = ext_vals[,"Dir"])      
                  }
                  if (var_name == "BRAN_cur") {
                    ext_vals[index, "v"] <- ext_fills[, "v"]
                    ext_vals[index, "u"] <- ext_fills[, "u"]
                    ext_vals[index, "Spd"] <- ext_fills[, "Spd"]
                    ext_vals[index, "Dir"] <- ext_fills[, "Dir"]
                    df_exp <- data_details$unique_positions %>%
                    dplyr::filter(date == dataset$date) %>%
                    dplyr::mutate(BRAN_vcur = ext_vals[,"v"],
                      BRAN_ucur = ext_vals[,"u"],
                      BRAN_current_velocity = ext_vals[,"Spd"],
                      BRAN_current_bearing = ext_vals[,"Dir"])      
                  }                                               
                } else {
                  if (var_name == "rs_current") {
                    df_exp <- data_details$unique_positions %>%
                    dplyr::filter(date == dataset$date) %>%
                    dplyr::mutate(rs_gsla = ext_vals[,"GSLA"],
                      rs_vcur = ext_vals[,"VCUR"],
                      rs_ucur = ext_vals[,"UCUR"],
                      rs_current_velocity = ext_vals[,"Spd"],
                      rs_current_bearing = ext_vals[,"Dir"])      
                  }
                  if (var_name == "BRAN_wind") {
                    df_exp <- data_details$unique_positions %>%
                    dplyr::filter(date == dataset$date) %>%
                    dplyr::mutate(BRAN_vwind = ext_vals[,"VWIND"],
                      BRAN_uwind = ext_vals[,"UWIND"],
                      BRAN_wind_velocity = ext_vals[,"Spd"],
                      BRAN_wind_bearing = ext_vals[,"Dir"])      
                  }
                  if (var_name == "BRAN_cur") {
                    df_exp <- data_details$unique_positions %>%
                    dplyr::filter(date == dataset$date) %>%
                    dplyr::mutate(BRAN_vcur = ext_vals[,"v"],
                      BRAN_ucur = ext_vals[,"u"],
                      BRAN_current_velocity = ext_vals[,"Spd"],
                      BRAN_current_bearing = ext_vals[,"Dir"])      
                  }                                                        
                }
              } else {
                index <- which(is.na(ext_vals[,2]))
                if (length(index) > 0) { # Only if gaps are present
                  pos_sf_buffer <- sf::st_buffer(pos_sf[index,], dist = .buffer)
                  ext_fills <- suppressWarnings(terra::extract(remote_rast, pos_sf_buffer, fun = median, na.rm = TRUE))
                  ext_vals[index,2] <- ext_fills[,2]
                  ext_vals <- as.numeric(ext_vals[,2])
                } else {
                  ext_vals <- as.numeric(ext_vals[,2])
                }
                df_exp <- data_details$unique_positions %>%
                  dplyr::filter(date == dataset$date) %>%
                  dplyr::mutate(var = ext_vals)
                names(df_exp)[ncol(df_exp)] <- var_name       
              }
            # If no fill gaps
            } else {
              if (var_name %in% c("rs_current", "BRAN_wind", "BRAN_cur")) {
                if (var_name == "rs_current") {
                  df_exp <- data_details$unique_positions %>%
                  dplyr::filter(date == dataset$date) %>%
                  dplyr::mutate(rs_gsla = ext_vals[,"GSLA"],
                    rs_vcur = ext_vals[,"VCUR"],
                    rs_ucur = ext_vals[,"UCUR"],
                    rs_current_velocity = ext_vals[,"Spd"],
                    rs_current_bearing = ext_vals[,"Dir"])
                }
                if (var_name == "BRAN_wind") {
                  df_exp <- data_details$unique_positions %>%
                  dplyr::filter(date == dataset$date) %>%
                  dplyr::mutate(BRAN_vwind = ext_vals[,"VWIND"],
                    BRAN_uwind = ext_vals[,"UWIND"],
                    BRAN_wind_velocity = ext_vals[,"Spd"],
                    BRAN_wind_bearing = ext_vals[,"Dir"])
                }
                if (var_name == "BRAN_cur") {
                  df_exp <- data_details$unique_positions %>%
                  dplyr::filter(date == dataset$date) %>%
                  dplyr::mutate(BRAN_vcur = ext_vals[,"v"],
                    BRAN_ucur = ext_vals[,"u"],
                    BRAN_current_velocity = ext_vals[,"Spd"],
                    BRAN_current_bearing = ext_vals[,"Dir"])
                }
              } else {
                ext_vals <- as.numeric(ext_vals[,2])
                df_exp <- data_details$unique_positions %>%
                  dplyr::filter(date == dataset$date) %>%
                  dplyr::mutate(var = ext_vals)
                names(df_exp)[ncol(df_exp)] <- var_name
              }
            }
        }
    }   
  }    
	return(df_exp)
}