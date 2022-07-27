##' @title extract_var
##' @description Extract variables from pulled variables 
##'
##' @param unique_positions dataframe with unique coordinates and dates from where to extract daily environmental data
##' @param env_stack a raster stack object with daily spatial environmental layers associated with detection dates (output from `.pull_var` function) 
##' @param env_var name of extracted variable (brought in from `extractEnv` function)
##' @param .fill_gaps Fill spatial gaps within each layer using a distance buffer
##' @param .buffer radius of buffer (in m) around each detection from which environmental variables should be extracted from. A median value of pixels 
##' that fall within the buffer will be used if `.fill_gaps = TRUE`
##' @param verbose should function provide details of what operation is being conducted. Uses parameter from parent function `extractEnv`
##'
##' @details Internal function to conduct raster extraction of environmental variables from downloaded raster stack (`env_stack`) and detection 
##' positions (`unique_positions`)
##'
##' @return a dataframe with unique position, unique dates and extracted environmental variables 
##'
##' @importFrom dplyr '%>%' mutate case_when select
##' @importFrom sf st_as_sf st_drop_geometry
##' @importFrom raster raster extract extent 
##'
##' @keywords internal

.extract_var <- function(unique_positions, env_stack, env_var, .fill_gaps, .buffer = NULL, verbose = TRUE){
  
  ## Check arguments
  
  ## Configure unique_positions to allow extraction
  pos_sf <- 
    unique_positions %>% 
    st_as_sf(coords = c(1,2), crs = 4326, remove = F) %>% 
    mutate(layer = paste0("X", gsub("\\-", ".", date)))
  
  ## setup output dataset
  out_data <-
    pos_sf %>% 
    st_drop_geometry()
  
  if(env_var %in% "rs_current"){
    env_names <- c("rs_gsla", "rs_vcur", "rs_ucur")
  } else {
      env_names <- env_var
    }

  ## Setup buffer for extractions when .fill_gaps = TRUE; 20km buffer for currents and 5km for others
  if(.fill_gaps){
    if(is.null(.buffer)){
      if(env_var %in% c("rs_current", "rs_salinity")){.buffer <- 20000}
      if(env_var %in% c("rs_sst_interpolated")){.buffer <- 15000} 
      if(env_var %in% c("rs_sst", "rs_chl", "rs_npp", "rs_turbidity")){.buffer <- 5000}
    }
  }
  
  if(.fill_gaps & verbose){
    message("Filling gaps in environmental data by extracting median values from a ", .buffer/1000, "km buffer around detections that fall on 'NA' values")
  }
  
  if(env_var %in% "rs_current"){
    ## extraction current datasets run through each current dataset (gsla, vcur, ucur)
    for(c in 1:length(env_names)){
      ext_matrix <- extract(env_stack[[c]], pos_sf)
      variable <- vector()
      for (i in 1:nrow(ext_matrix)) {
        val <-
          ext_matrix[i,][which(colnames(ext_matrix) %in% pos_sf$layer[i])]
        if (length(val) > 0) {
          variable[i] <- val
        } else {
          variable[i] <- NA
        }
      }
      
      ## gap filling
      if(.fill_gaps){
        ext_matrix_fill <- extract(env_stack[[c]], pos_sf, buffer = .buffer, fun = median)
        variable_fill <- vector()
        for (i in 1:nrow(ext_matrix)) {
          val <-
            ext_matrix_fill[i,][which(colnames(ext_matrix_fill) %in% pos_sf$layer[i])]
          if (length(val) > 0) {
            variable_fill[i] <- val
          } else {
            variable_fill[i] <- NA
          }
        } 
      }
      
      ## Append extracted variables to pos_sf dataset
      if(length(variable) > 0){
        out_data <-
          out_data %>% 
          mutate(variable = variable) %>% 
          {if(.fill_gaps) mutate(., 
                                        var_fill = variable_fill,
                                        variable = case_when(is.na(variable) ~ var_fill,
                                                                    TRUE ~ variable)) %>% 
              dplyr::select(., -var_fill) else .}
        colnames(out_data)[colnames(out_data) %in% "variable"] <- env_names[c]
      } else {
        out_data <-
          out_data %>% 
          mutate(variable = NA) %>% 
          {if(.fill_gaps) mutate(., 
                                        var_fill = variable_fill,
                                        variable = case_when(is.na(variable) ~ var_fill,
                                                                    TRUE ~ variable)) %>% 
              dplyr::select(., -var_fill) else .}
        colnames(out_data)[colnames(out_data) %in% "variable"] <- env_names[c]
      }
    }
  } 
  
  if(env_var %in% c("bathy", "dist_to_land")) {
    ## extraction for single/fixed layer ('bathy', 'dist_to_land')
    #Ran into some occasional problems; there's an extract function in tidyr as well as raster. Specifying this seems to have resolved it -- BD
    ext_matrix <- raster::extract(env_stack, pos_sf)
    view(ext_matrix)
    variable <- ext_matrix
    
    ## Append extracted variables to pos_sf dataset
    if(length(variable) > 0){
      out_data <- 
        out_data %>% 
        mutate(variable = variable) 
      colnames(out_data)[colnames(out_data) %in% "variable"] <- env_names
    } else {
      out_data <-
        out_data %>% 
        mutate(variable = NA) 
      colnames(out_data)[colnames(out_data) %in% "variable"] <- env_names
    }
  }
  
  
  if(env_var %in% c("rs_sst", "rs_sst_interpolated", "rs_salinity", "rs_chl", "rs_turbidity", "rs_npp")) {
    ## extraction for time-series raster stacks
    ext_matrix <- extract(env_stack, pos_sf)
    variable <- vector()
    for (i in 1:nrow(ext_matrix)) {
      val <-
        ext_matrix[i, ][which(colnames(ext_matrix) %in% pos_sf$layer[i])]
      if (length(val) > 0) {
        variable[i] <- val
      } else {
        variable[i] <- NA 
      }
    }
    
    ## gap filling
    if(.fill_gaps){
      ext_matrix_fill <- extract(env_stack, pos_sf, buffer = .buffer, fun = median)
      variable_fill <- vector()
      for (i in 1:nrow(ext_matrix)) {
        val <-
          ext_matrix_fill[i,][which(colnames(ext_matrix_fill) %in% pos_sf$layer[i])]
        if (length(val) > 0) {
          variable_fill[i] <- val
        } else {
          variable_fill[i] <- NA
        }
      } 
    }
    ## Append extracted variables to pos_sf dataset
    if(length(variable) > 0){
      out_data <- 
        out_data %>% 
        mutate(variable = variable) %>% 
        {if(.fill_gaps) mutate(., 
                                      var_fill = variable_fill,
                                      variable = case_when(is.na(variable) ~ var_fill,
                                                                  TRUE ~ variable)) %>% 
            dplyr::select(., -var_fill) else .}
      colnames(out_data)[colnames(out_data) %in% "variable"] <- env_names
    } else {
      out_data <-
        out_data %>% 
        mutate(variable = NA) %>% 
        {if(.fill_gaps) mutate(., 
                                      var_fill = variable_fill,
                                      variable = case_when(is.na(variable) ~ var_fill,
                                                                  TRUE ~ variable)) %>% 
            dplyr::select(., -var_fill) else .}
      colnames(out_data)[colnames(out_data) %in% "variable"] <- env_names
    }
  }
  return(out_data[-4])
}
