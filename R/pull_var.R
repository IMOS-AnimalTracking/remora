##' @title Pull environmental data from IMOS THREDDS server and save to local system
##'
##' @description Accesses and download environmental data from the IMOS THREDDS server 
##'
##' @param dates detection data source in data frame with at the minimum a X, Y and date time field
##' @param study_extent raster Extent object defining the study area from which environmental data should be cropped
##' @param var_name variable needed options include current options ('rs_sst', 'rs_sst_interpolated', 
##' 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land')
##' @param folder_name name of folder within 'imos.cache' where downloaded rasters should be saved. 
##' default NULL produces automatic folder names based on study extent
##' @param .cache should the extracted environmental data be cached within the working directory? 
##' @param .crop should the extracted environmental data be cropped to within the study site
##' @param .output_format File type for cached environmental layers. See \code{\link[raster]{writeFormats}}. The default format is 'raster'.
##' @param .parallel should the environmental data download be conducted in parallel to speed up the process?
##' @param .ncores 
##'
##' @details Internal function to download environmental raster layers from IMOS Thredds server (http://thredds.aodn.org.au/)
##'
##' @return a raster stack object with daily spatial environmental layers associated with detection dates
##'
##' @importFrom dplyr '%>%' filter bind_rows n_distinct 
##' @importFrom readr write_csv 
##' @importFrom utils txtProgressBar setTxtProgressBar download.file
##' @importFrom raster raster stack extent crop addLayer setZ writeRaster projection projection<-
##' @importFrom sp CRS
##' @importFrom parallel detectCores
##' @importFrom future plan
##' @importFrom furrr future_map furrr_options
##' @importFrom tibble tibble
##' @importFrom progressr progressor with_progress
##' @importFrom R.utils gunzip
##' @importFrom fs file_temp
##' 
##' @import ncdf4
##'
##' @keywords internal

.pull_env <- function(dates, study_extent, var_name, folder_name = NULL, .cache, .crop, .output_format = "raster", .parallel = TRUE, .ncores = NULL, verbose = TRUE){
  
  ## Check arguments
  if(!var_name %in% c('rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land')){
    stop("Environmental variable not recognised, options include:\n'rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land'")}
  
  ## setup parallelisation
  if(.parallel){
    if(is.null(.ncores)) {
      .ncores <- detectCores()
    } else {
      ## check if n_cores <= detectCores else return warning
      if(.ncores > detectCores()) 
        warning("process to be run across more cores than available, this may not be efficient")
    } 
  }
  
  if(var_name %in% c("bathy", "dist_to_land")){
    
    ## Define urls and download for habitat variables
    ## Non-temporal layers
    # Bathymetry
    if(var_name %in% "bathy"){
      ## Update with IMOS github repo link
      url <- "https://github.com/IMOS-AnimalTracking/environmental_layers/blob/main/bathymetry_AustralianEEZ.tif?raw=true"
      out_brick <-
        try(raster(url, verbose = FALSE) %>%
              {if (.crop) crop(.,study_extent) else .} , silent=TRUE)
      names(out_brick) <- "bathy"
    }
    
    # Distance to land
    if(var_name %in% "dist_to_land"){
      url <- "https://github.com/IMOS-AnimalTracking/environmental_layers/blob/main/dist_to_land_AustralianEEZ.tif?raw=true"
      out_brick <-
        try(raster(url, verbose = FALSE) %>%
              {if (.crop) crop(.,study_extent) else .} , silent=TRUE)
      names(out_brick) <- "dist_to_land"
    }
    
  } 
  
  ## Ocean Color layers
  if(var_name %in% c('rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp')){
    
    ## build urls based on dates and variable names
    urls <- .build_urls(dates, var_name, verbose = verbose)
    
    ## download raster files from built urls
    if(.parallel){
      message("Downloading environmental data in parallel across ", .ncores, " cores...")
      
      plan("multisession", workers = .ncores)
      
      par_function1 <- function(url, var_name, .crop, study_extent){
        p()
        tryCatch({
          out_ras <- 
            try(raster(url$url_name, varname = url$layer, verbose = FALSE) %>%
                  {if (.crop) crop(.,study_extent) else .}, silent=TRUE)
          
          if(var_name %in% c("rs_sst_interpolated", "rs_sst")){
            names(out_ras) <- substr(out_ras@z[[1]], start = 1, stop = 10)
            ## Convert to deg C
            out_ras <- out_ras - 273.15
            
          } else {
            names(out_ras) <- as.character(out_ras@z[[1]]) 
          }
        }, 
        error = function(e) {
          error_log <<- bind_rows(error_log, url)
          out_ras <<- stack()
        }
        )
        return(out_ras)
      }
      
      ## establish a log to store all erroneous urls
      error_log <- tibble(date = as.Date(NULL), url_name = NULL, layer = NULL)
      
      p <- progressor(steps = nrow(urls))
      
      ras_list <- 
        split(urls, urls$date) %>% 
        future_map(.x = ., .f = par_function1, var_name, .crop, study_extent, 
                          .options = furrr_options(seed = TRUE))
      
      out_brick <- stack(ras_list)
      
      plan("sequential")
      
    } else {
      ## Looped version
      ## run through urls to download, crop and stack environmental variables
      
      ## establish a log to store all erroneous urls
      error_log <- tibble(date = as.Date(NULL), url_name = NULL, layer = NULL)
      
      for(i in 1:nrow(urls)){
        if(i %in% 1){
          pb <- txtProgressBar(max = nrow(urls), style = 3)
          tryCatch({
            out_brick <-
              try(raster(urls$url_name[i], varname = urls$layer[i]) %>%
                    {if (.crop) crop(.,study_extent) else .} , silent=TRUE)
            
            if(var_name %in% c("rs_sst_interpolated", "rs_sst")){
              names(out_brick) <- substr(out_brick@z[[1]], start = 1, stop = 10)
              ## Convert to deg C
              out_brick <- out_brick - 273.15
              
            } else {
              names(out_brick) <- as.character(out_brick@z[[1]]) 
            }
          }, error=function(e) {
            error_log <<- bind_rows(error_log, urls[i,])
          })
          
        } else {
          tryCatch({
            out_layer <- 
              try(raster(urls$url_name[i], varname = urls$layer[i]) %>%
                    {if (.crop) crop(.,study_extent) else .}, silent=TRUE)
            
            if(var_name %in% c("rs_sst_interpolated", "rs_sst")){
              names(out_layer) <- substr(out_layer@z[[1]], start = 1, stop = 10)
              ## Convert to deg C
              out_layer <- out_layer - 273.15
              
            } else {
              names(out_layer) <- as.character(out_layer@z[[1]])
            }
            
            out_brick <- addLayer(out_brick, out_layer) 
            
          }, 
          error = function(e) {
            error_log <<- bind_rows(error_log, urls[i,])
          }
          )
        }
        setTxtProgressBar(pb, i)
      }
      
      ## provide log of error prone urls
      if(nrow(error_log) > 0){
        message("Environmental data were not found for ", n_distinct(error_log$date)," dates")
        message("Error log with missing data saved in the working directory")
        write_csv(error_log %>% mutate(variable = var_name), paste0(var_name, "_errorlog.txt"))}
    }
    
    ## Assign a zvalues to raster stack output
    zval <-
      names(out_brick) %>%
      substr(start = 2, stop = 11) %>% 
      gsub("\\.", "-", .) %>% 
      as.Date()
    
    if(is.na(projection(out_brick))){
      projection(out_brick) <- CRS("EPSG:4326")
    }
    out_brick <- setZ(x = out_brick, z = zval, name = "date")
  }

  ## Current layers
  if(var_name %in% "rs_current"){
    
    ## build urls based on dates and variable names
    built_urls <- .build_urls(dates, var_name, verbose = verbose)
    
    ## error log
    error_log <- built_urls %>% filter(is.na(layer))
    
    ## extract urls with data
    urls <- built_urls %>% filter(!is.na(layer))
    
    ## download raster files from built urls
    if(.parallel){
      message("Downloading IMOS Ocean Current data in parallel across ", .ncores, " cores...")
      
      plan("multisession", workers = .ncores)
      
      gsla_stack <- stack()
      vcur_stack <- stack()
      ucur_stack <- stack()
      
      par_function2 <- function(url, .crop, study_extent){
        p()
        temp_nc <- file_temp(ext = ".nc.gz")
        download.file(url$url_name, destfile = temp_nc, quiet = TRUE)
        nc_path <- gunzip(temp_nc)
        
        tryCatch({
          gsla <<- 
            try(raster(nc_path, varname = "GSLA", verbose = FALSE) %>% 
                  {if (.crop) crop(.,study_extent) else .}, silent=TRUE)
          names(gsla) <- url$date[1]
          gsla_stack <<- addLayer(gsla_stack, gsla)
          
          vcur <<- 
            try(raster(nc_path, varname = "VCUR", verbose = FALSE) %>% 
                  {if (.crop) crop(.,study_extent) else .}, silent=TRUE)
          names(vcur) <- url$date[1]
          vcur_stack <<- addLayer(vcur_stack, vcur)
          
          ucur <<- 
            try(raster(nc_path, varname = "UCUR", verbose = FALSE) %>% 
                  {if (.crop) crop(.,study_extent) else .}, silent=TRUE)
          names(ucur) <- url$date[1]
          ucur_stack <<- addLayer(ucur_stack, ucur)
          
          },
          error = function(e) {
            message(e)
          })
        return(tibble(date = url$date, url_name = url$url_name, tempfile = temp_nc))
      }
      
      p <- progressor(steps = nrow(urls))
      
      ras_list <- 
        urls %>% 
        split(., .$date) %>% 
        future_map(.x = ., .f = par_function2, .crop, study_extent, 
                          .options = furrr_options(seed = TRUE))
      
      out_brick <- list(gsla = gsla_stack, vcur = vcur_stack, ucur = ucur_stack)
      
      plan("sequential")
      
    } else {
      ## Looped version
      ## run through urls to download, crop and stack environmental variables
      
      gsla_stack <- stack()
      vcur_stack <- stack()
      ucur_stack <- stack()
      
      pb <- txtProgressBar(max = nrow(urls), style = 3)
      
      for(i in 1:nrow(urls)){
        temp_nc <- file_temp(ext = ".nc.gz")
        download.file(urls$url_name[i], destfile = temp_nc, quiet = TRUE)
        nc_path <- gunzip(temp_nc)
        tryCatch({
          gsla <- 
            try(raster(nc_path, varname = "GSLA") %>% 
                  {if (.crop) crop(.,study_extent) else .}, silent = TRUE)
          names(gsla) <- urls$date[i]
          gsla_stack <- addLayer(gsla_stack, gsla)
          
          vcur <- 
            try(raster(nc_path, varname = "VCUR") %>% 
                  {if (.crop) crop(.,study_extent) else .}, silent = TRUE)
          names(vcur) <- urls$date[i]
          vcur_stack <- addLayer(vcur_stack, vcur)
          
          ucur <- 
            try(raster(nc_path, varname = "UCUR") %>% 
                  {if (.crop) crop(.,study_extent) else .}, silent = TRUE)
          names(ucur) <- urls$date[i]
          ucur_stack <- addLayer(ucur_stack, ucur)
          
        },
        error = function(e) {
          message(e)
        })
        
        setTxtProgressBar(pb, i)
      }
      
      out_brick <- list(gsla = gsla_stack, vcur = vcur_stack, ucur = ucur_stack)
    }
    
    ## provide log of error prone urls
    if(nrow(error_log) > 0){
      message("Ocean current data were not found for ", n_distinct(error_log$date)," dates")
      message("Error log with missing data saved in the working directory")
      write_csv(error_log %>% mutate(variable = var_name), paste0(var_name, "_errorlog.txt"))}
      
  }
  
  ## Fill gaps in out_brick if fill_gap = TRUE; removed due to inaccurate temporal interpolation
  # if(.fill_gaps){
  #   if(verbose){
  #     message("Filling gaps in environmental layer using statistical interpolation across layers...")
  #   }
  #   
  #   znum <-
  #     names(out_brick) %>% 
  #     substr(., 2, 11) %>% 
  #     gsub("\\.", "-", .) %>%
  #     as.Date() %>% 
  #     as.numeric()
  #   
  #   out_brick <- approxNA(out_brick, rule = 2, z = znum)
  # }
    
  ## If caching raster stack, define and set up folders to store files locally
  if(.cache){
    dir.create("imos.cache", showWarnings = FALSE)
    dir.create("imos.cache/rs variables", showWarnings = FALSE)
    if(is.null(folder_name)){
      path <- file.path("imos.cache/rs variables", paste("extent", paste0(round(study_extent[1:4]), collapse = "_"), sep = "_"))
    } else {
      path <- file.path("imos.cache/rs variables", folder_name) 
    }
    dir.create(path, showWarnings = FALSE)
    
    ## Save as requested raster output format
    if(var_name %in% "rs_current"){
      writeRaster(out_brick$gsla, filename = paste(path, "rs_gsla", sep = "/"), overwrite = T, format = .output_format) 
      writeRaster(out_brick$vcur, filename = paste(path, "rs_vcur", sep = "/"), overwrite = T, format = .output_format) 
      writeRaster(out_brick$ucur, filename = paste(path, "rs_ucur", sep = "/"), overwrite = T, format = .output_format) 
    } else {
      if(is.na(projection(out_brick))){
        projection(out_brick) <- CRS("EPSG:4326")
        }
      writeRaster(out_brick, filename = paste(path, var_name, sep = "/"), overwrite = T, format = .output_format) 
    }
  } 

  return(out_brick)  
  
}







