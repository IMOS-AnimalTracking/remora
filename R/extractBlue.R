##' @title Extract and append Bluelink Reanalysis (BRAN) environmental data to detection data
##'
##' @description Accesses and download environmental data from the Bluelink (CSIRO)
##' server and append variables to detection data based on date of detection
##'
##' @param df detection data source in data frame with at the minimum a X, Y and
##' date time field
##' @param X name of column with X coordinate or longitude (EPSG 4326)
##' @param Y name of column with Y coordinate or latitude (EPSG 4326)
##' @param datetime name of column with date time stamp (Coordinated Universal 
##' Time; UTC)
##' @param env_var variable needed from Bluelink. Options include ('BRAN_temp', 'BRAN_salt', 
##' BRAN_ssh', 'BRAN_mld', 'BRAN_cur', 'BRAN_wind').
##' @param extract_depth Bluelink data is 3D, so data can be obtained either at the water surface or at depth. Please
##' provide the depth of interest (between 0 and 4,509 m) as numeric and the function will automatically obtain the data at 
##' the nearest available layer. By default the data will be extracted at the water surface. 
##' @param var_name name for the column including the extracted environmental data. Can be usefull if the user wants to 
##' download the same environmental data at different depths. If not specified, it will be chosen based on the env_var and
##' extract_depth arguments.
##' @param folder_name name of folder within the working directory where the downloaded and processed 
##' netCDF files should be saved. Default (NULL) produces automatic folder names based on study extent and deletes 
##' processed files after processing. 
##' @param verbose should function provide details of what operation is being conducted. 
##' Set to `FALSE` to keep it quiet
##' @param cache_layers should the downloaded and processed environmental data be cached within
##' the working directory? If FALSE (default), the Bluelink data will be stored in a temporary folder 
##' and discarded after environmental extraction. Otherwise, it will be saved in the "cached" folder
##' within folder_name. 
##' @param env_buffer distance (in decimal degrees) to expand the study area beyond the coordinates to extract environmental data. Default value is 1°.
##' @param full_timeperiod should environmental variables be extracted for each day
##' across full monitoring period? This option is time and memory consuming for long projects. If this option is selected, 
##' the returned dataset will be standardized for the days with/without detections across all stations (station_name column) where animals were 
##' detected. For more details please see the package vignettes.
##' @param station_name if full_timeperiod = TRUE, please provide the column that identifies the name of the 
##' acoustic stations
##' @param export_step should the processed dataset be exported to file? This is particularly usefull for large 
##' datasets, to avoid function failure due to issues with internet connexion. The rows with missing data will be 
##' exported as NAs, and only these will be completed if the function is rerun providing the exported dataset as input (df) 
##' @param export_path path and name of to export the dataset with appended environmnetal data
##' @param .parallel should the function be run in parallel 
##' @param .ncores number of cores to use if set to parallel. If none provided, 
##' uses \code{\link[parallel]{detectCores}} to determine number.
##'
##' @details The `extractBlue` function allows the user to download, process and 
##' append a range of 3D environmental variables (between the water surface to 4,509 m depth) 
##' to each detection within a telemetry data set. 
##' We advocate for users to first undertake a quality control step using
##' the \code{\link{runQC}} function before further analysis, however the 
##' functionality to append environmental data will work on any dataset that has
##' at the minimum spatial coordinates (i.e., latitude, longitude; in EPSG 4326)
##' and a timestamp (in UTC) for each detection event. Quality controlled 
##' environmental variables housed in the Bluelink (BRAN) CSIRO server will be extracted 
##' for each specific coordinate at the specific timestamp where available. A 
##' summary table of the full range of environmental variables currently 
##' available can be accessed using the \code{\link{imos_variables}} function.
##' 
##'
##' @return a dataframe with the environmental variable appended as an extra 
##' column based on date of each detection
##'
##' @examples
##' ## Input example detection dataset that have run through the quality control
##' ##   workflow (see 'runQC' function)
##' 
##' library(tidyverse)
##' data("TownsvilleReefQC")
##' 
##' ## simplify & subset data for speed
##' qc_data <- 
##'   TownsvilleReefQC %>% 
##'   unnest(cols = c(QC)) %>% 
##'   ungroup() %>% 
##'   filter(Detection_QC %in% c(1,2)) %>%
##'   filter(filename == unique(filename)[1]) %>%
##'   slice(1:20)
##' 
##' ## Extract daily interpolated sea surface temperature
##' ## cache_layers & fill_gaps args set to FALSE for speed
##' data_with_temp <- 
##'    extractBlue(df = qc_data,
##'                X = "receiver_deployment_longitude", 
##'                Y = "receiver_deployment_latitude", 
##'                datetime = "detection_datetime", 
##'                env_var = "BRAN_temp",
##'                extract_depth = 0,
##'                verbose = TRUE)
##'
##' @importFrom foreach %dopar%
##'
##' @export

extractBlue <- function(df, X, Y, datetime, 
                        env_var, extract_depth = 0, var_name = paste(env_var, extract_depth, sep = "_"), 
                        folder_name = "Bluelink", env_buffer = 1, cache_layers = FALSE,
                        full_timeperiod = FALSE, station_name = NULL, 
                        export_step = FALSE, export_path = "Processed_data",
                        .parallel = FALSE, .ncores = NULL, verbose = TRUE) {
  
  # Initial checks of parameters
  if(!X %in% colnames(df)){stop("Cannot find X coordinate in dataset, provide column name where variable can be found")}
  if(!Y %in% colnames(df)){stop("Cannot find Y coordinate in dataset, provide column name where variable can be found")}
  if(!datetime %in% colnames(df)){stop("Cannot find datetime in dataset, provide column name where variable can be found")}
  if(!datetime %in% colnames(df)){stop("Cannot find date timestamp column in dataset, provide column name where variable can be found")}
  if(!env_var %in% c('BRAN_temp', 'BRAN_salt', 'BRAN_ssh', 'BRAN_mld', 'BRAN_cur', 'BRAN_wcur', 'BRAN_wind')){
    stop("Environmental variable not recognised, options include:\n'BRAN_temp', 'BRAN_salt', 'BRAN_ssh', 'BRAN_mld', 'BRAN_cur', 'BRAN_wcur', 'BRAN_wind'")}
  if(length(env_var) > 1){stop("This function currently only supports extracting a single variable at a time. Please only select one of:\n'ocean_temp', 'ocean_salt', 'ocean_u', 'ocean_v', 'ocean_w', 'ocean_eta_t', 'ocean_mld', 'air_wind")}
  if(is.character(extract_depth)) {
    stop("Please provide the extract_depth argument as numeric")
  }
  # Check if data needs to be download for full timeperiod or not
  df <- as.data.frame(df)
  if (full_timeperiod) {
    if (is.null(station_name)) {
      stop("Please provide column with station names in the 'station_name' argument.")
    } else {
      # Create empty dataset to be filled
      stations <- unique(df[ ,station_name])
      all.dates <- as.Date(range(unique(df[,datetime])), tz = "UTC")
      all.dates <- seq(all.dates[1], all.dates[2], 1)
      df.all <- expand.grid(all.dates, stations)
      names(df.all) <- c("date", station_name)
      df.all$lon <- df[,X][match(df.all[,station_name], df[,station_name])]
      df.all$lat <- df[,Y][match(df.all[,station_name], df[,station_name])]
      df.all$Var <- NA 
      names(df.all)[length(names(df.all))] <- var_name
      # For data download!
      df.all$aux.date <- substr(df.all$date, 1, 7) 
      dates <- unique(df.all$aux.date)
      if (cache_layers) {
        if (dir.exists(folder_name) == FALSE)  
          dir.create(folder_name)
      }
    }
  } else {
    # Check for previous function runs
    if (dir.exists(folder_name) == FALSE & cache_layers) 
      dir.create(folder_name)
    df$aux.date <- substr(df[,which(names(df) == datetime)], 1, 7)
    if (env_var %in% names(df)) {
      message(paste("The", env_var, "variable was found. Continuing data download."))
      dates <- unique(df$aux.date[is.na(df[which(names(df) == var_name)])])  
    } else {
      if (env_var %in% c("BRAN_cur", "BRAN_wind")) {
        if (env_var == "BRAN_cur") {
          df$BRAN_dir <- NA            
          df$BRAN_spd <- NA
        } else {
          df$BRAN_wind_dir <- NA
          df$BRAN_wind_spd <- NA
        }        
      } else {
        df$Var <- NA
        names(df)[length(names(df))] <- var_name  
      }
      dates <- unique(df$aux.date)
    }   
  }
  # Change variable names for data download
  if (env_var == "BRAN_temp") {
    download.var <- "ocean_temp"
  }
  if (env_var == "BRAN_salt") {
    download.var <- "ocean_salt"
  }
  if (env_var == "BRAN_ssh") {
    download.var <- "ocean_eta_t"
  }
  if (env_var == "BRAN_mld") {
    download.var <- "ocean_mld"
  }
  if (env_var == "BRAN_wind") {
    download.var <- "atm_flux_diag"
  }   
  if (env_var == "BRAN_wcur") {
    download.var <- "ocean_w"
  }   
  # Processing begins
  if (.parallel) { # Run in parallel
    `%dopar%` <- foreach::`%dopar%`
    if (is.null(.ncores)) {
      .ncores = parallel::detectCores()
      cl <- parallel::makeCluster(.ncores[1]-1)
      doParallel::registerDoParallel(cl)
    } else {
      cl <- parallel::makeCluster(.ncores)
      doParallel::registerDoParallel(cl)
    }
    if (verbose) {
      message(paste("Downloading", env_var, "data:",
                    min(dates), "|", max(dates), "in parallel..."))
    }
    nc.bran <- foreach::foreach(i = 1:length(dates),
                                .combine = rbind, .packages = c('remora', 'dplyr')) %dopar% {
                                  if (env_var %in% c("BRAN_cur", "BRAN_wind")) {
                                    if (env_var == "BRAN_cur") {
                                      # Open netCDF files
                                      save.bran_u <- remoteNCDF(
                                        year = as.numeric(substr(dates[i], 1, 4)),
                                        month = as.numeric(substr(dates[i], 6, 7)),
                                        var = "ocean_u",      
                                        depth = extract_depth,
                                        lon.min = min(df[,X]) - env_buffer,
                                        lon.max = max(df[,X]) + env_buffer,
                                        lat.min = min(df[,Y]) - env_buffer,
                                        lat.max = max(df[,Y]) + env_buffer
                                      )
                                      save.bran_v <- remoteNCDF(
                                        year = as.numeric(substr(dates[i], 1, 4)),
                                        month = as.numeric(substr(dates[i], 6, 7)),
                                        var = "ocean_v",      
                                        depth = extract_depth,
                                        lon.min = min(df[,X]) - env_buffer,
                                        lon.max = max(df[,X]) + env_buffer,
                                        lat.min = min(df[,Y]) - env_buffer,
                                        lat.max = max(df[,Y]) + env_buffer
                                      )
                                      save.bran <- save.bran_u
                                      save.bran$ocean_v <- save.bran_v$ocean_v
                                      save.bran$BRAN_dir <- windDir(u = save.bran$ocean_u, v = save.bran$ocean_v)
                                      save.bran$BRAN_spd <- windSpd(u = save.bran$ocean_u, v = save.bran$ocean_v)
                                      save.bran <- save.bran[,c(7,8,2:5)]
                                    } else {
                                      save.bran <- remoteNCDF(
                                        year = as.numeric(substr(dates[i], 1, 4)),
                                        month = as.numeric(substr(dates[i], 6, 7)),
                                        var = download.var,      
                                        depth = extract_depth,
                                        lon.min = min(df[,X]) - env_buffer,
                                        lon.max = max(df[,X]) + env_buffer,
                                        lat.min = min(df[,Y]) - env_buffer,
                                        lat.max = max(df[,Y]) + env_buffer
                                      )
                                      save.bran$BRAN_wind_dir <- windDir(u = save.bran$u, v = save.bran$v)
                                      save.bran$BRAN_wind_spd <- windSpd(u = save.bran$u, v = save.bran$v)
                                      save.bran <- save.bran[,c(6,7,3:5)]
                                    }
                                  } else {
                                    # Open netCDF files
                                    save.bran <- remoteNCDF(
                                      year = as.numeric(substr(dates[i], 1, 4)),
                                      month = as.numeric(substr(dates[i], 6, 7)),
                                      var = download.var,      
                                      depth = extract_depth,
                                      lon.min = min(df[,X]) - env_buffer,
                                      lon.max = max(df[,X]) + env_buffer,
                                      lat.min = min(df[,Y]) - env_buffer,
                                      lat.max = max(df[,Y]) + env_buffer
                                    )
                                  }
                                }  
    parallel::stopCluster(cl)
    nc.bran$x <- as.numeric(nc.bran$x)
    nc.bran$y <- as.numeric(nc.bran$y)
  } else { # Not in parallel
    if (verbose) {
      message(paste("Downloading", env_var, "data:", min(dates), "|", max(dates)))
      pb <- txtProgressBar(min = 0, max = length(dates), initial = 0, style = 3, width = 50)
    }
    nc.bran <- NULL
    for (i in 1:length(dates)) {
      if (env_var %in% c("BRAN_cur", "BRAN_wind")) {
        if (env_var == "BRAN_cur") {
          # Open netCDF files
          aux.date <- dates[i]
          save.bran_u <- remoteNCDF(
            year = as.numeric(substr(aux.date, 1, 4)),
            month = as.numeric(substr(aux.date, 6, 7)),
            var = "ocean_u",      
            depth = extract_depth,
            lon.min = min(df[,X]) - env_buffer,
            lon.max = max(df[,X]) + env_buffer,
            lat.min = min(df[,Y]) - env_buffer,
            lat.max = max(df[,Y]) + env_buffer
          )
          save.bran_v <- remoteNCDF(
            year = as.numeric(substr(aux.date, 1, 4)),
            month = as.numeric(substr(aux.date, 6, 7)),
            var = "ocean_v",      
            depth = extract_depth,
            lon.min = min(df[,X]) - env_buffer,
            lon.max = max(df[,X]) + env_buffer,
            lat.min = min(df[,Y]) - env_buffer,
            lat.max = max(df[,Y]) + env_buffer
          )
          save.bran <- save.bran_u
          save.bran$ocean_v <- save.bran_v$ocean_v
          save.bran$BRAN_dir <- windDir(u = save.bran$ocean_u, v = save.bran$ocean_v)
          save.bran$BRAN_spd <- windSpd(u = save.bran$ocean_u, v = save.bran$ocean_v)
          save.bran <- save.bran[,c(7,8,2:5)]
          nc.bran <- rbind(nc.bran, save.bran)
          gc()
        } else {
          aux.date <- dates[i]
          save.bran <- remoteNCDF(
            year = as.numeric(substr(aux.date, 1, 4)),
            month = as.numeric(substr(aux.date, 6, 7)),
            var = download.var,      
            depth = extract_depth,
            lon.min = min(df[,X]) - env_buffer,
            lon.max = max(df[,X]) + env_buffer,
            lat.min = min(df[,Y]) - env_buffer,
            lat.max = max(df[,Y]) + env_buffer
          )
          save.bran$BRAN_wind_dir <- windDir(u = save.bran$u, v = save.bran$v)
          save.bran$BRAN_wind_spd <- windSpd(u = save.bran$u, v = save.bran$v)
          save.bran <- save.bran[,c(6,7,3:5)]
          nc.bran <- rbind(nc.bran, save.bran)
          gc()
        }
      } else {
        # Open netCDF files
        aux.date <- dates[i]
        save.bran <- remoteNCDF(
          year = as.numeric(substr(aux.date, 1, 4)),
          month = as.numeric(substr(aux.date, 6, 7)),
          var = download.var,      
          depth = extract_depth,
          lon.min = min(df[,X]) - env_buffer,
          lon.max = max(df[,X]) + env_buffer,
          lat.min = min(df[,Y]) - env_buffer,
          lat.max = max(df[,Y]) + env_buffer
        )
        nc.bran <- rbind(nc.bran, save.bran) 
        gc() 
      }
      if (verbose)
        setTxtProgressBar(pb, i)     
    }
    nc.bran$x <- as.numeric(nc.bran$x)
    nc.bran$y <- as.numeric(nc.bran$y)
    if (verbose)
      close(pb)
  }
  # Export environmental data 
  if (cache_layers)
    utils::write.csv(nc.bran, paste0(folder_name, "/", var_name, ".csv"), row.names = FALSE) 
  # Process data  
  # Run processing by year to avoid memory kill
  if (full_timeperiod) {
    df.all$year <- substr(df.all$aux.date, 1, 4)
  } else {
    df$year <- substr(df$aux.date, 1, 4)
  }
  year.data <- unique(substr(dates, 1, 4))   
  df.save <- NULL
  if (verbose) {
    if (.parallel) {
      message("Extracting environmental data in parallel...")
    } else {
      message("Extracting environmental data...")
    }  
    pb <- txtProgressBar(min = 0, max = length(year.data), initial = 0, style = 3, width = 50)
  }
  for (aux.year in 1:length(year.data)) {
    year.run <- year.data[aux.year]
    if (full_timeperiod) {
      df.run <- subset(df.all, year == year.run)
    } else {
      df.run <- subset(df, year == year.run)
    }
    if (.parallel) { # Run in parallel
      if (is.null(.ncores)) {
        .ncores = parallel::detectCores()
        cl <- parallel::makeCluster(.ncores[1]-1)
        doParallel::registerDoParallel(cl)
      } else {
        cl <- parallel::makeCluster(.ncores)
        doParallel::registerDoParallel(cl)
      }
      if (full_timeperiod) {
        index.day <- unique(as.Date(df.run[,"date"]))
        var.save <- foreach::foreach(i = 1:length(index.day),
                                     .combine = 'c', 
                                     .packages = c('foreach', 'geosphere')) %dopar% {  
                                       aux.nc <- subset(nc.bran, as.Date(Time) == index.day[i])
                                       index.locs <- which(as.Date(df.run[,datetime]) == index.day[i]) 
                                       var.run <- foreach::foreach(ii = 1:length(index.locs), 
                                                                   .combine = 'c',
                                                                   .packages = 'geosphere') %dopar% {
                                                                     aux.nc$Distance <- as.numeric(geosphere::distm(y = c(df.run[index.locs[ii], X], df.run[index.locs[ii], Y]),
                                                                                                                    x = aux.nc[,c("x","y")]
                                                                     ))
                                                                     if (env_var %in% c("BRAN_cur", "BRAN_wind")) {
                                                                       var1 <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance)),1]
                                                                       var2 <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance)),2]
                                                                       var.run <- c(var1, var2)
                                                                     } else {
                                                                       var.run <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance)),1]
                                                                     }
                                                                   }
                                     }
      } else { 
        index.day <- unique(as.Date(df.run[,datetime])) 
        var.save <- foreach::foreach(i = 1:length(index.day),
                                     .combine = 'c', 
                                     .packages = c('foreach', 'geosphere')) %dopar% {  
                                       aux.nc <- subset(nc.bran, as.Date(Time) == index.day[i])
                                       index.locs <- which(as.Date(df.run[,datetime]) == index.day[i])      
                                       var.run <- foreach::foreach(ii = 1:length(index.locs), 
                                                                   .combine = 'c', 
                                                                   .packages = 'geosphere') %dopar% {
                                                                     aux.nc$Distance <- as.numeric(geosphere::distm(y = c(df.run[index.locs[ii], X], df.run[index.locs[ii], Y]),
                                                                                                                    x = aux.nc[,c("x","y")]
                                                                     ))
                                                                     if (env_var %in% c("BRAN_cur", "BRAN_wind")) {
                                                                       var1 <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance))[1],1]
                                                                       var2 <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance))[1],2]
                                                                       var.run <- c(var1, var2)
                                                                     } else {
                                                                       var.run <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance))[1],1]
                                                                     } 
                                                                   }     
                                     }      
      }
      parallel::stopCluster(cl)
      if (env_var %in% c("BRAN_cur", "BRAN_wind")) {
        var.mat <- matrix(var.save, ncol = length(var.save) / 2) 
        var1 <- var.mat[1,] 
        var2 <- var.mat[2,] 
        if (env_var == "BRAN_cur") {
          for (rows in 1:nrow(df.run)) {
            df.run[rows, "BRAN_dir"] <- var1[rows]
          }
          for (rows in 1:nrow(df.run)) {
            df.run[rows, "BRAN_spd"] <- var2[rows]
          }
        }
        if (env_var == "BRAN_wind") {
          for (rows in 1:nrow(df.run)) {
            df.run[rows, "BRAN_wind_dir"] <- var1[rows]
          }
          for (rows in 1:nrow(df.run)) {
            df.run[rows, "BRAN_wind_spd"] <- var2[rows]
          }
        }
      } else {
        for (rows in 1:nrow(df.run)) {
          df.run[rows, var_name] <- var.save[rows]
        }
      }   
    } else { # Not in parallel
      if (full_timeperiod) { # Full timeperiod
        index.day <- unique(as.Date(df.run[,"date"]))
        for (i in 1:length(index.day)) {
          aux.nc <- subset(nc.bran, as.Date(Time) == index.day[i])
          index.locs <- which(as.Date(df.run[,"date"]) == index.day[i])
          for (ii in 1:length(index.locs)) {
            aux.nc$Distance <- as.numeric(geosphere::distm(y = c(df.run[index.locs[ii], "lon"], df.run[index.locs[ii], "lat"]),
                                                           x = aux.nc[,c("x","y")]
            ))
            if (env_var %in% c("BRAN_cur", "BRAN_wind")) {
              if(env_var == "BRAN_cur") {
                df.run[index.locs[ii], "BRAN_dir"] <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance))[1],1]
                df.run[index.locs[ii], "BRAN_spd"] <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance))[1],2]
              } else {
                df.run[index.locs[ii], "BRAN_wind_dir"] <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance))[1],1]
                df.run[index.locs[ii], "BRAN_wind_spd"] <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance))[1],2]
              }
            } else {
              df.run[index.locs[ii], var_name] <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance)),1]
            } 
          }   
          # Export extracted data 
          if (export_step) 
            utils::write.csv(df.run, paste0(export_path, ".csv"), row.names = FALSE)
        } 
        df.run <- df.run[,-which(names(df.run) == "aux.date")]
        # Match detection dates to total dataset
        df$aux <- paste(df[,station_name], as.Date(df[,datetime]), sep = "_")
        df$Detection <- 1
        detecs <- unique(df$aux)
        df.run$aux <- paste(df.run[,station_name], as.Date(df.run[,"date"]), sep = "_")
        df.run$Detection <- df$Detection[match(df.run$aux, df$aux)]
        df.run$Detection[is.na(df.run$Detection)] <- 0
        df.run <- df.run[,-which(names(df.run) == "aux")]
      } else {  # Only tracking locations
        index.day <- unique(as.Date(df.run[,datetime]))
        for (i in 1:length(index.day)) {
          aux.nc <- subset(nc.bran, as.Date(Time) == index.day[i])
          index.locs <- which(as.Date(df.run[,datetime]) == index.day[i])
          for (ii in 1:length(index.locs)) {
            aux.nc$Distance <- as.numeric(geosphere::distm(y = c(df.run[index.locs[ii], X], df.run[index.locs[ii], Y]),
                                                           x = aux.nc[,c("x","y")]
            ))
            if (env_var %in% c("BRAN_cur", "BRAN_wind")) {
              if(env_var == "BRAN_cur") {
                df.run[index.locs[ii], "BRAN_dir"] <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance))[1],1]
                df.run[index.locs[ii], "BRAN_spd"] <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance))[1],2]
              } else {
                df.run[index.locs[ii], "BRAN_wind_dir"] <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance))[1],1]
                df.run[index.locs[ii], "BRAN_wind_spd"] <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance))[1],2]
              }
            } else {
              df.run[index.locs[ii], var_name] <- aux.nc[which(aux.nc$Distance == min(aux.nc$Distance))[1],1]
            } 
          }   
        }
      } 
    }
    # Export extracted data 
    df.save <- rbind(df.save, df.run)
    if (export_step) 
      utils::write.csv(df.save, paste0(export_path, ".csv"), row.names = FALSE)  
    if (verbose)
      setTxtProgressBar(pb, aux.year)     
  }
  if (verbose)
    close(pb)
  # Tidy up data
  df.save <- df.save[,-which(names(df.save) %in% c("aux.date", "year"))]
  # Export results
  return(df.save) 
}