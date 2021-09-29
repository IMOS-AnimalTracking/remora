##' @title Download the IMOS Mooring Data for a Single Mooring Site
##'
##' @description Downloads all IMOS mooring data for a specified, single mooring site
##' @param moor_site_code character string if the name of the desired mooring (updated from moorID to match download file)
##' @param fromWeb logical string detailing whether or no the netCDF should be downloaded from the web or uploaded from a saved file location
##' @param sensorType character string detailing which sensor value we are interested in. Can be "temperature", "velocity", "salinity or "oxygen"."
##' @param itimeout integer value for number of seconds we are willing to wait before timeout to download netcdf from the web. Defaults to 60
##' @param file_loc character string for the location of the saved files
##' @return The \code{moorData} dataframe with the sensor information time series for a given \code{site_code} 
##' @importFrom ncdf4 nc_open ncvar_get ncatt_get nc_close
##' @export

mooringDownload <- function(moor_site_codes,
                            fromWeb,
                            sensorType = "temperature",
                            itimeout = 240,
                            file_loc){
  
  sensorType <- match.arg(sensorType, choices = c("temperature","velocity","salinity","oxygen"))
  if(sensorType %in% c("temperature","velocity","salinity","oxygen")) 
     stop('Only "temperature", "velocity", "salinity" or "oxygen" can be provided as a valid sensor type')
  
  dir.create(file_loc,showWarnings = F) # Create the directory if it doesnt already exist
  
  # Create function to run on a single mooring
  mooringDownload.1 <- function(moor_site_code,
                                fromWeb,
                                sensorType=c("temperature","velocity","salinity","oxygen"),
                                itimeout = 240,
                                file_loc){
    
    if(fromWeb==TRUE){ #Gets the data from the Web
      
      # Extracts the correct row and URL for a single moorings dataset
      moorTable <-  mooringTable(sensorType) 
      moorTableID <- moorTable %>% filter(site_code == moor_site_code)
      data_url <- paste0("http://thredds.aodn.org.au/thredds/fileServer/", moorTableID$url) 
      
      # Download the data for the instrument
      file_name <- paste0(file_loc, "/", moor_site_code, ".nc") # Define the file name for the netCDF
      options(timeout=itimeout) # To download a file that requires more than 60 seconds using download.file() function
      
      download.file(url = data_url, 
                    method = "libcurl", 
                    destfile = file_name,
                    mode = "wb") # Added as R studio would crash on windows when running this line
      
    }else{ #Gets the data from a local location
      file_name <- paste0(file_loc, "/", moor_site_code, ".nc") 
    }
    
    # Open the saved net cdf file
    nc_ch <- nc_open(file_name)
    
    #Get the time and time units and transform it to a datetime value
    time <- ncvar_get(nc_ch,"TIME") # the times look pretty weird
    tunits <- ncatt_get(nc_ch,"TIME","units") # what units are these times in
    data.time <- as.POSIXct(time*86400, origin = "1950-01-01",tz = "UTC") # Convert these to POSIXct object
    
    depth <- ncvar_get(nc_ch, "DEPTH") #Get the depth 
    fillvalue_depth <- ncatt_get(nc_ch, "DEPTH", "_Fillvalue")   #Get the NA values
    
    if(sensorType == "temperature"){
      temperature <- ncvar_get(nc_ch, "TEMP") #Get the sea water temperature  
      fillvalue_temperature <- ncatt_get(nc_ch, "TEMP", "_Fillvalue") #Get the NA values
    }
    
    if(sensorType == "velocity"){
      ucur <- ncvar_get(nc_ch, "UCUR")
      vcur <- ncvar_get(nc_ch, "VCUR")
      fillvalue_ucur <- ncatt_get(nc_ch, "UCUR", "_FillValue") 
      fillvalue_vcur <- ncatt_get(nc_ch, "VCUR", "_FillValue") 
    }

    if(sensorType == "salinity"){
      psal <- ncvar_get(nc_ch, "sea_water_practical_salinity")
      fillvalue_psal <- ncatt_get(nc_ch, "sea_water_practical_salinity", "_FillValue") 
    }
    
    if(sensorType == "oxygen"){
      mcDMO <- ncvar_get(nc_ch, "mole_concentration_of_dissolved_molecular_oxygen_in_sea_water")
      mOpum <- ncvar_get(nc_ch, "moles_of_oxygen_per_unit_mass_in_sea_water")
      vcDMO <- ncvar_get(nc_ch, "volume_concentration_of_dissolved_molecular_oxygen_in_sea_water")
      fillvalue_mcDMO <- ncatt_get(nc_ch, "mole_concentration_of_dissolved_molecular_oxygen_in_sea_water", "_FillValue") 
      fillvalue_mOpum <- ncatt_get(nc_ch, "moles_of_oxygen_per_unit_mass_in_sea_water, ", "_FillValue") 
      fillvalue_vcDMO <- ncatt_get(nc_ch, "volume_concentration_of_dissolved_molecular_oxygen_in_sea_water, ", "_FillValue") 
    }
    
    # Close the netcdf
    nc_close(nc_ch)
    
    #Fill the NA values
    depth[depth == fillvalue_depth$value] <- NA
    
    if(sensorType == "temperature"){
      temperature[temperature == fillvalue_temperature$value] <- NA 
    }
    
    if(sensorType == "velocity"){
      ucur[ucur == fillvalue_ucur$value] <- NA
      vcur[vcur == fillvalue_vcur$value] <- NA 
    }
    
    if(sensorType == "salinity"){
      psal[psal == fillvalue_psal$value] <- NA
    }
    
    if(sensorType == "oxygen"){
      mcDMO[mcDMO == fillvalue_mcDMO$value] <- NA
      mOpum[mOpum == fillvalue_mOpum$value] <- NA 
      vcDMO[vcDMO == fillvalue_vcDMO$value] <- NA 
    }
    
    # Create a dataframe with the dates, time, current velocity and depth
    
    if(sensorType == "temperature"){
      mooringsDf <- data.frame(moor_site_code,
                               moor_timestamp=data.time, 
                               moor_depth=depth,
                               moor_sea_temp=temperature)
    }
    if(sensorType == "velocity"){
      mooringsDf <- data.frame(moor_site_code,
                               moor_timestamp=data.time, 
                               moor_depth=depth,
                               moor_ucur=ucur,
                               moor_vcur=vcur)
    }
    if(sensorType == "salinity"){
      mooringsDf <- data.frame(moor_site_code,
                               moor_timestamp=data.time, 
                               moor_depth=depth,
                               moor_psal=psal)
    }
    
    if(sensorType == "oxygen"){
      mooringsDf <- data.frame(moor_site_code,
                               moor_timestamp = data.time, 
                               moor_depth = depth,
                               moor_mole_concentration_of_dissolved_molecular_oxygen_in_sea_water = mcDMO,
                               moor_moles_of_oxygen_per_unit_mass_in_sea_water = mOpum,
                               moor_volume_concentration_of_dissolved_molecular_oxygen_in_sea_water = vcDMO)
    }
    
    return(mooringsDf)
  }
  
  # Run this function on all the provided site codes
  moorIDs <- unique(moor_site_codes) # makes sure no duplicates
  lmoorIDs <- as.list(moorIDs) 
  
  moorDat.l <- lapply(lmoorIDs, mooringDownload.1, 
                      file_loc=file_loc, 
                      itimeout=itimeout, 
                      sensorType=sensorType, 
                      fromWeb = fromWeb)
  
  names(moorDat.l) <- moorIDs
  return(moorDat.l)
}
