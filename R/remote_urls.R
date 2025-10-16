##' @title Create URLs for IMOS netCDF files
##'
##' @description Creates URLs for openning IMOS netCDF files via OPENDAP 
##'
##' @param input Spatial and temporal details of data, generated using ext_find 
##' @param var_name Environmental variable of interest
##' @param .nrt Should near-real-time be used for current data? Default is TRUE
##' @param verbose Defaullt is TRUE
##'
##' @details Internal function to generate URLs for tidync processing
##'
##' @return Dataframe of netCDF URLs
##'
##' @keywords internal

remote_urls <- function(input,
                        var_name,
                        .nrt = TRUE,
                        verbose = TRUE) {
  
  ## Check arguments
  if(!var_name %in% c(
    'bathy',
    'dist_to_land',
    'rs_sst',
    'rs_sst_interpolated',
    'rs_chl',
    'rs_turbidity',
    'rs_npp',
	   'rs_current',
     'BRAN_temp', 
     'BRAN_salt', 
     'BRAN_ssh', 
     'BRAN_mld', 
     'BRAN_cur', 
     'BRAN_wcur', 
     'BRAN_wind'
  	)) {
    	stop("Environmental variable not recognised, options include:\n 'bathy', 'dist_to_land', 'rs_sst', 'rs_sst_interpolated', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 
        \n'BRAN_temp', 'BRAN_salt', 'BRAN_ssh', 'BRAN_mld', 'BRAN_cur', 'BRAN_wcur', 'BRAN_wind'")
	}

  if (var_name %in% c("bathy", "dist_to_land")) {
    # Bathymetry
    if(var_name %in% "bathy"){
      ## Update with IMOS github repo link
      url <- "https://github.com/IMOS-AnimalTracking/environmental_layers/blob/main/bathymetry_AustralianEEZ.tif?raw=true"
      out_brick <-
        try(terra::rast(url) %>%
              {terra::crop(., input$ext)}, silent=TRUE)
      names(out_brick) <- "bathy"
    }
    # Distance to land
    if(var_name %in% "dist_to_land"){
      url <- "https://github.com/IMOS-AnimalTracking/environmental_layers/blob/main/dist_to_land_AustralianEEZ.tif?raw=true"
      out_brick <-
        try(terra::rast(url) %>%
              {terra::crop(., input$ext)}, silent=TRUE)
      names(out_brick) <- "dist_to_land"
    }
  } else {
    ## calculate date range in dataset
    dates <- input$dates
    date_range <- range(input$dates)
    
    ## Check and refine dates where IMOS data is available, and set up components of url creation  
    ## Daily SST interpolated dataset (9km resolution)
    ## 
    if(var_name %in% "rs_sst_interpolated"){
      ## check if IMOS remote sensing data covers detection data range 
      ## RAMSSA: 2006-06-12 - present
      if(date_range[1] < as.Date("2006-06-12")){
        warning("IMOS interpolated sst data is currently only available from 2006-06-12 onwards,\ndetections prior to this date will not have envrionmental data associated")}
      sub_dates <-  dates[dates > as.Date("2006-06-12")]
      fdates <- sub_dates %>% format("%Y%m%d")
      
      ## define start and mid url, and define end of THREDDS based on variable name
      ## example :"http://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/SST/ghrsst/L4/RAMSSA/2006/20060612120000-ABOM-L4_GHRSST-SSTfnd-RAMSSA_09km-AUS-v02.0-fv01.0.nc"
      #start_url <- "http://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/SST/ghrsst/L4/RAMSSA/"
      start_url <- "https://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/SST/ghrsst/L4/RAMSSA/"
      end_url <- "120000-ABOM-L4_GHRSST-SSTfnd-RAMSSA_09km-AUS-v02.0-fv01.0.nc"
      layer <- "analysed_sst"
      
    } 

    ## Daily SST 'raw' dataset (~2km resolution)
    ## 
    if(var_name %in% "rs_sst"){
      ## check if IMOS remote sensing data covers detection data range 
      ## GHRSST AVHRR dataset: 1992-03-21 - present
      if(date_range[1] < as.Date("1992-03-21")){
        warning("IMOS environmental data is currently only available from 1992-03-21 onwards,\ndetections prior to this date will not have envrionmental data associated")}
      sub_dates <- dates[dates > as.Date("1992-03-21")]
      fdates <- sub_dates %>% format("%Y%m%d")
      
      ## define start and mid url, and define end of THREDDS based on variable name
      ## example :"http://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/SST/ghrsst/L3S-1d/dn/2013/20130501092000-ABOM-L3S_GHRSST-SSTfnd-AVHRR_D-1d_dn.nc"
      start_url <- "https://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/SST/ghrsst/L3S-1d/dn/"
      end_url <- "092000-ABOM-L3S_GHRSST-SSTfnd-AVHRR_D-1d_dn.nc"
      layer <- "sea_surface_temperature"
    }
    
    ## Ocean colour datasets (~1km resolution)
    ## 
    if(var_name %in% c("rs_chl", "rs_turbidity", "rs_npp")){
      ## check if IMOS remote sensing data covers detection data range 
      ## ocean color (Aqua Modis): 2002-07-04 - present
      if(date_range[1] < as.Date("2002-07-04")){
        warning("IMOS environmental data is currently only available from 2002-07-04 onwards,\ndetections prior to this date will not have envrionmental data associated")}
      sub_dates <- dates[dates > as.Date("2002-07-04")]
      fdates <- sub_dates %>% format("%Y%m%d")
      
      ## define start and mid url, and define end of THREDDS based on variable name
      ## example :"http://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/OC/gridded/aqua/P1D/2013/05/A.P1D.20130501T053000Z.aust.chl_oc3.nc"
      start_url <- "https://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/OC/gridded/aqua/P1D/"
      mid_url <- "A.P1D."
      layer <- ""
      
      # if(var_name %in% "rs_sst"){end_url <- "T053000Z.aust.sst.nc"}
      if(var_name %in% "rs_chl"){
        end_url <- "T053000Z.aust.chl_oc3.nc"
        layer <- "chl_oc3"
      }
      if(var_name %in% "rs_turbidity"){
        end_url <- "T053000Z.aust.K_490.nc"
        layer <- "K_490"
      }
      if(var_name %in% "rs_npp"){
        end_url <- "T053000Z.aust.npp_vgpm_eppley_oc3.nc"
        layer <- "npp_vgpm_eppley_oc3"
      }
    }

    ## Bluelink BRAN2020 variables
    ## 
    if(var_name %in% c('BRAN_temp', 'BRAN_salt', 'BRAN_ssh', 'BRAN_mld', 'BRAN_cur', 'BRAN_wcur', 'BRAN_wind')){
      
      ## BRAN2020 dataset: 1993-01-01 - 2023-12-31
      if(date_range[1] < as.Date("1993-01-01") |
        date_range[2] > as.Date("2023-12-31")){
        warning("Bluelink data is currently only available from 1993-01-01 to 2023-12-31,\ndetections prior or after these dates will not have envrionmental data associated")}
      
      sub_dates <- dates[dates >= as.Date("1993-01-01") & 
        dates <= as.Date("2023-12-31")]
      fdates <- sub_dates %>% format("%Y%m%d")
      
      ## define start and mid url, and define end of THREDDS based on variable name
      ## example :"http://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/SST/ghrsst/L3S-1d/dn/2013/20130501092000-ABOM-L3S_GHRSST-SSTfnd-AVHRR_D-1d_dn.nc"
      start_url <- "https://thredds.nci.org.au/thredds/dodsC/gb6/BRAN/BRAN2020/daily/"
      end_url <- ".nc"

      # Select BRAN2020 variables
      if (var_name == "BRAN_temp") {
        layer.name <- "ocean_temp"
        layer <- "temp"
      }
      if (var_name == "BRAN_salt") {
        layer.name <- "ocean_salt"
        layer <- "salt"
      }
      if (var_name == "BRAN_mld") {
        layer.name <- "ocean_mld"
        layer <- "mld"
      }
      if (var_name == "BRAN_wcur") {
        layer.name <- "ocean_w"
        layer <- "w"
      }
      if (var_name == "BRAN_ssh") {
        layer.name <- "ocean_eta_t"
        layer <- "eta_t"
      }
      if (var_name == "BRAN_wind") {
        layer.name <- "atm_flux_diag"
        layer <- ""
      }
      if (var_name == "BRAN_cur") {
        layer.name <- "ocean_u"
        layer <- ""
      }  
    
    }
    
    ## Daily Ocean Current
    ## 
    if(var_name %in% "rs_current"){
      ## check if IMOS Ocean Current DM data covers detection data range 
      ## Ocean current (delayed mode): 1993-01-01 - 2020-12-31
      ## IMOS near real time data: 2011-09-01 ongoing
      
      ## example : "http://thredds.aodn.org.au/thredds/catalog/IMOS/OceanCurrent/GSLA/DM/"
      # 
      if(date_range[1] < as.Date("1993-01-01")){
        warning("IMOS ocean current data is currently only available from 1993-01-01 onwards,\ndetections prior to this date will not have current data associated")
      } 
      if(date_range[2] > as.Date("2020-12-31") & !.nrt) {
        warning("IMOS Ocean Current Delayed-Mode data is currently only available from 1993-01-01 to 2020-12-31,\ndetections after this date range will not have current data associated")
      }
      if(date_range[2] > as.Date("2020-12-31") & .nrt) {
        message("IMOS Ocean Current Near-real-time data will be used for locations after 2020-12-31")
      }

      sub_dates <-  dates[dates > as.Date("1993-01-01")]

      ## IDJ - 19/05/2023: directory name on thredds server has changed from: http://thredds.aodn.org.au/thredds/catalog/IMOS/OceanCurrent/GSLA/DM00/ 
      ##                      to http://thredds.aodn.org.au/thredds/catalog/IMOS/OceanCurrent/GSLA/DM/
      catalog <-
        tibble(date = sub_dates, 
          fdates = format(date, "%Y%m%d"),
          year = format(date, "%Y"),
          base_url = paste0("https://thredds.aodn.org.au/thredds/dodsC/IMOS/OceanCurrent/GSLA/DM/", year, "/IMOS_OceanCurrent_HV_"),
          start_url = paste0("https://thredds.aodn.org.au/thredds/dodsC/IMOS/OceanCurrent/GSLA/DM/", year, "/IMOS_OceanCurrent_HV_"),
          end_url = "T000000Z_GSLA_FV02_DM02.nc")

      ## if .nrt == TRUE then substitute NRT data for DM when year > 2020
      if(.nrt) {
        catalog$base_url[catalog$year > 2020] <- paste0(paste0("https://thredds.aodn.org.au/thredds/dodsC/IMOS/OceanCurrent/GSLA/NRT/", catalog$year[catalog$year > 2020], "/IMOS_OceanCurrent_HV_"))
        catalog$start_url[catalog$year > 2020] <- paste0(paste0("https://thredds.aodn.org.au/thredds/dodsC/IMOS/OceanCurrent/GSLA/NRT/", catalog$year[catalog$year > 2020], "/IMOS_OceanCurrent_HV_"))
        catalog$end_url[catalog$year > 2020] <- "T000000Z_GSLA_FV02_NRT.nc"
      } 

      sub_dates <- catalog$date
      fdates <- catalog$fdates
      start_url <- catalog$start_url
      end_url <- catalog$end_url
      layer <- ""

      
      if(verbose){
        message("Finding IMOS Ocean Current data...")
      }
    }
    
    ## build urls from which to download environmental data (current and salinity have different formats)
    if(var_name %in% c("rs_sst_interpolated", "rs_sst")){
      url_name <- paste0(start_url, 
                         substr(fdates, start = 1, stop = 4), "/",
                         fdates, end_url)
    }

    if(var_name %in% c("rs_chl", "rs_turbidity", "rs_npp")){
      url_name <- paste0(start_url, 
                         substr(fdates, start = 1, stop = 4), "/",
                         substr(fdates, start = 5, stop = 6), "/",
                         mid_url,
                         fdates, end_url) 
    }

    if (var_name %in% c('BRAN_temp', 'BRAN_salt', 'BRAN_ssh', 'BRAN_mld', 'BRAN_cur', 'BRAN_wcur', 'BRAN_wind')){
      url_name <- paste0(start_url, 
                         layer.name, "_",
                         substr(fdates, start = 1, stop = 4), "_",
                         substr(fdates, start = 5, stop = 6),
                         end_url)
    }

    if(var_name %in% c("rs_current", "rs_salinity")){
      url_name <- paste0(start_url, 
                         fdates, end_url)     
    }

    url_df <- tibble(date = sub_dates, url_name, layer) 
  }

  if (var_name %in% c("bathy", "dist_to_land")) {
    return(out_brick)
  }  else {
    return(url_df) 
  } 
}
