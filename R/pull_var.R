##' @title Pull environmental data from IMOS THREDDS server and save to local system
##'
##' @description Accesses and download environmental data from the IMOS THREDDS server 
##'
##' @param dates detection data source in data frame with at the minimum a X, Y and date time field
##' @param study_extent `terra::ext` object defining the study area from which environmental data should be cropped
##' @param var_name variable needed options include current options ('rs_sst', 'rs_sst_interpolated', 
##' 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land')
##' @param folder_name name of folder within 'imos.cache' where downloaded rasters should be saved. 
##' default NULL produces automatic folder names based on study extent
##' @param .cache should the extracted environmental data be cached within the working directory? 
##' @param .crop should the extracted environmental data be cropped to within the study site
##' @param .output_format File type for cached environmental layers. See \code{\link[raster]{writeFormats}}. The default format is 'raster'.
##' @param .parallel should the environmental data download be conducted in parallel to speed up the process? Currently, parallel processing
##' is only supported when downloading 'rs_current' data. 
##' @param .ncores 
##'
##' @details Internal function to download environmental raster layers from IMOS Thredds server (http://thredds.aodn.org.au/)
##'
##' @return a SpatRaster object with daily spatial environmental layers associated with detection dates
##'
##' @importFrom dplyr '%>%' filter bind_rows n_distinct 
##' @importFrom readr write_csv 
##' @importFrom utils txtProgressBar setTxtProgressBar download.file
##' @importFrom terra rast ext crs 'crs<-' writeRaster 'time<-'
##' @importFrom parallel detectCores
##' @importFrom future plan
##' @importFrom furrr future_map furrr_options
##' @importFrom tibble tibble
##' @importFrom R.utils gunzip
##'
##' @keywords internal

.pull_env <- function(dates, study_extent, var_name, folder_name = NULL, .cache, .crop, .output_format = "raster", .parallel = TRUE, .ncores = NULL, verbose = TRUE){
  
  ## Check arguments
  if(!var_name %in% c('rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land')){
    stop("Environmental variable not recognised, options include:\n'rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land'")}
  
  ## setup parallelisation
  if(all(.parallel, var_name == "rs_current")) {
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
        try(rast(url) %>%
              {if (.crop) crop(., study_extent) else .} , silent=TRUE)
      names(out_brick) <- "bathy"
    }
    
    # Distance to land
    if(var_name %in% "dist_to_land"){
      url <- "https://github.com/IMOS-AnimalTracking/environmental_layers/blob/main/dist_to_land_AustralianEEZ.tif?raw=true"
      out_brick <-
        try(rast(url) %>%
              {if (.crop) crop(., study_extent) else .} , silent=TRUE)
      names(out_brick) <- "dist_to_land"
    }
    
  } 

  ## Ocean Color layers
  if(var_name %in% c('rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp')){

    ## build urls based on dates and variable names
    urls <- .build_urls(dates, var_name, verbose = verbose) 
    
      ## Looped version
      ## run through urls to download, crop and stack environmental variables

      ## establish a log to store all erroneous urls
      error_log <- tibble(date = as.Date(NULL), url_name = NULL, layer = NULL)
      pb <- txtProgressBar(max = nrow(urls), style = 3)
  
      ras_lst <- lapply(1:nrow(urls), function(i) {
        tryCatch({
          temp_nc <- tempfile(fileext = ".nc.gz")
          download.file(urls$url_name[i], destfile = temp_nc, quiet = TRUE)
          nc_path <- gunzip(temp_nc)
          
          ras <- try(rast(nc_path, 
                          lyrs = switch(urls$layer[i] != "", urls$layer[i], NULL),
                          win = switch(.crop, study_extent, NULL)),
                     silent = TRUE)
          
          if(var_name %in% c("rs_sst_interpolated", "rs_sst")){
            ## Convert to deg C
            ras <- ras - 273.15
          }
          names(ras) <- as.character(urls$date[i])
        }, 
        error=function(e) {
          error_log <<- bind_rows(error_log, urls[i,])
        })
        
        setTxtProgressBar(pb, i)
        
        return(ras)
      })
      cat("\n")
      out_brick <- rast(ras_lst)
      
      ## provide log of error prone urls
      if(nrow(error_log) > 0){
        message("Environmental data were not found for ", n_distinct(error_log$date)," dates")
        message("Error log with missing data saved in the working directory")
        write_csv(error_log %>% mutate(variable = var_name), paste0(var_name, "_errorlog.txt"))}
      
      if(any(is.na(crs(out_brick)), is.null(crs(out_brick)))) {
        crs(out_brick) <- "epsg:4326"
      }
    }


  ## Current layers
  if(var_name %in% "rs_current"){
    ## build urls based on dates and variable names
    built_urls <- .build_urls(dates, var_name, verbose = verbose)
  
    if(!is.null(built_urls)) {
      ## error log
      error_log <- built_urls %>% filter(is.na(layer))
      
      ## extract urls with data
      urls <- built_urls %>% filter(!is.na(layer))
    
      ## download raster files from built urls
    if(.parallel){
      message("Downloading IMOS Ocean Current data in parallel across ", 
              .ncores, 
              " cores...")
      
      plan("multisession", workers = .ncores)
      
      gsla_stack <- NULL
      vcur_stack <- NULL
      ucur_stack <- NULL
      
      fn2 <- function(url, .crop, study_extent){
        temp_nc <- tempfile(fileext = ".nc.gz")
        download.file(url$url_name, destfile = temp_nc, quiet = TRUE)
        nc_path <- gunzip(temp_nc)
        
        tryCatch({
          gsla <<- try(rast(nc_path, 
                   subds = "GSLA",
                   win = switch(.crop, study_extent, NULL)),
              silent = TRUE)
          names(gsla) <- url$date
          gsla_stack <<- c(gsla_stack, gsla)
          
          vcur <<- try(rast(nc_path, 
                            subds = "VCUR",
                            win = switch(.crop, study_extent, NULL)),
                       silent = TRUE)
          names(vcur) <- url$date
          vcur_stack <<- c(vcur_stack, vcur)
          
          ucur <<- try(rast(nc_path, 
                            subds = "UCUR",
                            win = switch(.crop, study_extent, NULL)),
                       silent = TRUE)
          names(ucur) <- url$date
          ucur_stack <<- c(ucur_stack, ucur)
          
          },
          error = function(e) {
            message(e)
          })
        return(tibble(date = url$date, url_name = url$url_name, tempfile = temp_nc))
      }
      
      ras_list <- 
        urls %>% 
        split(., .$date) %>% 
        future_map( ~ try(fn2(url =.x,
                              .crop = .crop,
                              study_extent = study_extent),
                          silent = TRUE),
                   .options = furrr_options(seed = TRUE),
                   .progress = TRUE)
      
      out_brick <- list(gsla = gsla_stack, vcur = vcur_stack, ucur = ucur_stack)
      
      plan("sequential")
      
    } else {
      ## Looped version
      ## run through urls to download, crop and stack environmental variables
      
      gsla_stack <- NULL
      vcur_stack <- NULL
      ucur_stack <- NULL
      
      pb <- txtProgressBar(max = nrow(urls), style = 3)
      
      for(i in 1:nrow(urls)){
        temp_nc <- tempfile(fileext = ".nc.gz")
        download.file(urls$url_name[i], destfile = temp_nc, quiet = TRUE)
        nc_path <- gunzip(temp_nc)
        
        tryCatch({
          gsla <- try(rast(nc_path, 
                           subds = "GSLA",
                           win = switch(.crop, study_extent, NULL)),
                      silent = TRUE)
          names(gsla) <- urls$date[i]
          gsla_stack <- c(gsla_stack, gsla)
          
          vcur <- try(rast(nc_path, 
                           subds = "VCUR",
                           win = switch(.crop, study_extent, NULL)),
                      silent = TRUE)
          names(vcur) <- urls$date[i]
          vcur_stack <- c(vcur_stack, vcur)
          
          ucur <- try(rast(nc_path, 
                           subds = "UCUR",
                           win = switch(.crop, study_extent, NULL)),
                      silent = TRUE)
          names(ucur) <- urls$date[i]
          ucur_stack <- c(ucur_stack, ucur)
          
        },
        error = function(e) {
          message(e)
        })
        
        setTxtProgressBar(pb, i)
      }
      
      out_brick <- list(gsla = gsla_stack, vcur = vcur_stack, ucur = ucur_stack)
    }
    cat("\n")
    ## provide log of error prone urls

    if(nrow(error_log) > 0){
      message("Ocean current data were not found for ", n_distinct(error_log$date)," dates")
      message("Error log with missing data saved in the working directory")
      write_csv(error_log %>% mutate(variable = var_name), paste0(var_name, "_errorlog.txt"))
      }
    } else {
      out_brick <- NULL
    }
  }
  
  ## If caching raster stack, define and set up folders to store files locally
  if(.cache){
    dir.create("imos.cache", showWarnings = FALSE)
    dir.create(file.path("imos.cache", "rs variables"), showWarnings = FALSE)
    if(is.null(folder_name)){
      path <- file.path("imos.cache", "rs variables", paste("extent", paste0(round(study_extent[1:4]), collapse = "_"), sep = "_"))
    } else {
      path <- file.path("imos.cache", "rs variables", folder_name) 
    }
    dir.create(path, showWarnings = FALSE)
    
    ## Save as requested raster output format
    if(var_name %in% "rs_current"){
      writeRaster(out_brick$gsla, filename = paste0(file.path(path, "rs_gsla"), ".grd"), overwrite = TRUE)#, format = .output_format) 
      writeRaster(out_brick$vcur, filename = paste0(file.path(path, "rs_vcur"), ".grd"), overwrite = TRUE)#, format = .output_format) 
      writeRaster(out_brick$ucur, filename = paste0(file.path(path, "rs_ucur"), ".grd"), overwrite = TRUE)#, format = .output_format) 
    } else {
      if(any(is.na(crs(out_brick)), is.null(crs(out_brick)))) {
        crs(out_brick) <- "epsg:4326"
      }
      writeRaster(out_brick, filename = paste0(file.path(path, var_name), ".grd"), overwrite = TRUE) #, format = .output_format) 
    }
  } 

  return(out_brick)  
  
}







