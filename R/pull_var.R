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
##' @param .nrt should NRT Ocean Current data be used if DM data is not available for certain years?
##' @param .output_format File type for cached environmental layers. See \code{\link[raster]{writeFormats}}. The default format is 'raster'.
##' @param .parallel should the environmental data download be conducted in parallel to speed up the process? Currently, parallel processing
##' is only supported when downloading 'rs_current' data. 
##' @param .ncores 
##'
##' @details Internal function to download environmental raster layers from IMOS Thredds server (http://thredds.aodn.org.au/)
##'
##' @return a SpatRaster object with daily spatial environmental layers associated with detection dates
##'
##' @importFrom dplyr %>% filter bind_rows n_distinct rowwise
##' @importFrom readr write_csv 
##' @importFrom utils txtProgressBar setTxtProgressBar download.file
##' @importFrom terra rast ext crs crs<- writeRaster time<- wrap unwrap crop
##' @importFrom parallel detectCores
##' @importFrom future plan
##' @importFrom furrr future_map furrr_options
##' @importFrom tibble tibble
##' @importFrom R.utils gunzip
##'
##' @keywords internal

.pull_env <-
  function(dates,
           study_extent,
           var_name,
           folder_name = NULL,
           .cache,
           .crop,
           .nrt = FALSE,
           .output_format = ".grd",
           .parallel = TRUE,
           .ncores = NULL,
           verbose = TRUE) {
    
  
  ## Check arguments
  if(!var_name %in% c('rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land')){
    stop("Environmental variable not recognised, options include:\n'rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land'")}
  
  ## setup parallelisation
  if(.parallel) {
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
        try(terra::rast(url) %>%
              {if (.crop) terra::crop(., study_extent) else .} , silent=TRUE)
      names(out_brick) <- "bathy"
    }
    
    # Distance to land
    if(var_name %in% "dist_to_land"){
      url <- "https://github.com/IMOS-AnimalTracking/environmental_layers/blob/main/dist_to_land_AustralianEEZ.tif?raw=true"
      out_brick <-
        try(terra::rast(url) %>%
              {if (.crop) terra::crop(., study_extent) else .} , silent=TRUE)
      names(out_brick) <- "dist_to_land"
    }
    
  } 

  ## Ocean Color layers
  if(var_name %in% c('rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp')) {

    ## build urls based on dates and variable names
    urls <- .build_urls(dates, var_name, verbose = verbose) 
    
    if (.parallel) {
      message("Downloading IMOS Ocean Current data in parallel across ",
              .ncores,
              " cores...")
      
      plan("multisession", workers = .ncores)
      
      fn2 <- function(url) {
        file.sfx <- str_split(url$url_name, "\\.", simplify = TRUE)
        file.sfx <- file.sfx[, ncol(file.sfx)]
        
        if(file.sfx == "gz") {
          temp_nc <- tempfile(fileext = ".nc.gz")
          download.file(url$url_name, 
                        destfile = temp_nc, 
                        method = "auto",
                        mode = "wb",
                        quiet = TRUE)
          nc_path <- gunzip(temp_nc)
          
          ras <- terra::rast(nc_path, 
                      lyrs = switch(url$layer != "", url$layer, NULL))
          
        } else if (file.sfx == "nc") {
          temp_nc <- tempfile(fileext = ".nc")
          download.file(url$url_name, 
                        destfile = temp_nc, 
                        method = "auto",
                        mode = "wb",
                        quiet = TRUE)
          
          ras <- terra::rast(temp_nc, 
                      lyrs = switch(url$layer != "", url$layer, NULL))
        }
        
        if(var_name %in% c("rs_sst_interpolated", "rs_sst")){
          ## Convert to deg C
          ras <- ras - 273.15
        }
        
        return(terra::wrap(ras))
      }
      
      ras.lst <- split(urls, urls$date) %>%
        future_map(~ fn2(.x),
                   .options = furrr_options(seed = TRUE),
                   .progress = TRUE)
      
      ## unpack SpatRasters now that they've passed through the connection
      ras.lst <- lapply(ras.lst, terra::unwrap)
      
      if (.crop) {
        ras.lst <- lapply(ras.lst, crop, y = study_extent)
      }
      
      out_brick <- terra::rast(ras.lst)
        
      plan("sequential")
      
    } else {
      
      ## Looped version
      ## run through urls to download, crop and stack environmental variables
      
      ## establish a log to store all erroneous urls
      # error_log <- tibble(date = as.Date(NULL), url_name = NULL, layer = NULL)
      pb <- txtProgressBar(max = nrow(urls), style = 3)
      
      ras_lst <- lapply(1:nrow(urls), function(i) {
        
          file.sfx <- str_split(urls$url_name[i], "\\.", simplify = TRUE)
          file.sfx <- file.sfx[, ncol(file.sfx)]
          
          if(file.sfx == "gz") {
            temp_nc <- tempfile(fileext = ".nc.gz")
            download.file(urls$url_name[i], 
                          destfile = temp_nc, 
                          method = "auto",
                          mode = "wb",
                          quiet = TRUE)
            nc_path <- gunzip(temp_nc)
            
            ras <- terra::rast(nc_path, 
                        lyrs = switch(urls$layer[i] != "", urls$layer[i], NULL),
                        win = switch(.crop, study_extent, NULL))
            
          } else if (file.sfx == "nc") {
            temp_nc <- tempfile(fileext = ".nc")
            download.file(urls$url_name[i], 
                          destfile = temp_nc, 
                          method = "auto",
                          mode = "wb",
                          quiet = TRUE)
            
            ras <- terra::rast(temp_nc, 
                        lyrs = switch(urls$layer[i] != "", urls$layer[i], NULL),
                        win = switch(.crop, study_extent, NULL))
          }
          
          if(var_name %in% c("rs_sst_interpolated", "rs_sst")){
            ## Convert to deg C
            ras <- ras - 273.15
          }
          names(ras) <- as.character(urls$date[i])
        
        setTxtProgressBar(pb, i)
        
        return(ras)
      })
      cat("\n")
      out_brick <- terra::rast(ras_lst)
      
    }
      
      if(any(is.na(terra::crs(out_brick)), is.null(terra::crs(out_brick)))) {
        terra::crs(out_brick) <- "epsg:4326"
      }
    }


  ## Current layers
  if(var_name %in% "rs_current"){
    ## build urls based on dates and variable names
    built_urls <- .build_urls(dates, var_name, .nrt = .nrt, verbose = verbose)
  
    if(!is.null(built_urls)) {
      ## error log
      error_log <- built_urls %>% filter(is.na(layer))
      
      ## extract urls with data
      urls <- built_urls %>% filter(!is.na(layer))
    
      ## download raster files from built urls
      if (.parallel) {
        message("Downloading IMOS Ocean Current data in parallel across ",
                .ncores,
                " cores...")
        
        plan("multisession", workers = .ncores)
        
        fn2 <- function(url) {
          temp_nc <- tempfile(fileext = ".nc.gz")
          download.file(
            url$url_name,
            destfile = temp_nc,
            quiet = TRUE,
            method = "auto",
            mode = "wb"
          )
          
          nc_path <- gunzip(temp_nc)
          
          ## use terra::wrap to pack SpatRasters & pass through the connection
          ##    set up by parallel processing via future
          gsla <- terra::rast(nc_path,
                       subds = "GSLA")
          
          vcur <- terra::rast(nc_path,
                       subds = "VCUR")
          
          ucur <- terra::rast(nc_path,
                       subds = "UCUR")
          
          return(list(
            gsla = terra::wrap(gsla),
            vcur = terra::wrap(vcur),
            ucur = terra::wrap(ucur)
          ))
          
        }
        
        ras.lst <- split(urls, urls$date) %>%
          future_map(~ fn2(.x),
                     .options = furrr_options(seed = TRUE),
                     .progress = TRUE)
        
        ## unpack SpatRasters now that they've passed through the connection
        ras.lst <- lapply(ras.lst, function(x)
          lapply(x, unwrap))
        
        if (.crop) {
          ras.lst <-
            lapply(ras.lst, function(x)
              lapply(x, crop, y = study_extent))
        }
        
        gsla_stack <- terra::rast(lapply(ras.lst, function(x)
          x[[1]]))
        vcur_stack <- terra::rast(lapply(ras.lst, function(x)
          x[[2]]))
        ucur_stack <- terra::rast(lapply(ras.lst, function(x)
          x[[3]]))
        
        out_brick <-
          list(gsla = gsla_stack,
               vcur = vcur_stack,
               ucur = ucur_stack)
        
        plan("sequential")
        
    } else {
      ## Looped version
      ## run through urls to download, crop and stack environmental variables
      
      gsla_stack <- NULL
      vcur_stack <- NULL
      ucur_stack <- NULL
      
      pb <- txtProgressBar(max = nrow(urls), style = 3)
      
      for (i in 1:nrow(urls)) {
        temp_nc <- tempfile(fileext = ".nc.gz")
        download.file(
          urls$url_name[i],
          destfile = temp_nc,
          quiet = TRUE,
          method = "auto",
          mode = "wb"
        )
        nc_path <- gunzip(temp_nc)
        
        gsla <- terra::rast(nc_path,
                     subds = "GSLA",
                     win = switch(.crop, study_extent, NULL))
        names(gsla) <- urls$date[i]
        gsla_stack <- c(gsla_stack, gsla)
        
        vcur <- terra::rast(nc_path,
                     subds = "VCUR",
                     win = switch(.crop, study_extent, NULL))
        names(vcur) <- urls$date[i]
        vcur_stack <- c(vcur_stack, vcur)
        
        ucur <- terra::rast(nc_path,
                     subds = "UCUR",
                     win = switch(.crop, study_extent, NULL))
        names(ucur) <- urls$date[i]
        ucur_stack <- c(ucur_stack, ucur)
        
        
        setTxtProgressBar(pb, i)
      }
      
      out_brick <-
        list(
          gsla = terra::rast(gsla_stack),
          vcur = terra::rast(vcur_stack),
          ucur = terra::rast(ucur_stack)
        )
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
      writeRaster(out_brick$gsla, filename = paste0(file.path(path, "rs_gsla"), .output_format), overwrite = TRUE)
      writeRaster(out_brick$vcur, filename = paste0(file.path(path, "rs_vcur"), .output_format), overwrite = TRUE)
      writeRaster(out_brick$ucur, filename = paste0(file.path(path, "rs_ucur"), .output_format), overwrite = TRUE)
    } else {
      if(any(is.na(terra::crs(out_brick)), is.null(terra::crs(out_brick)))) {
        terra::crs(out_brick) <- "epsg:4326"
      }
      writeRaster(out_brick, filename = paste0(file.path(path, var_name), .output_format), overwrite = TRUE)
    }
  } 

  return(out_brick)  
  
}







