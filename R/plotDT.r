##' @title Depth-time plot 
##' @description Creates an interactive depth-time plot from a sensor dataset from a specified IMOS mooring, with an option to overlay species detection records from tracking dataset from closest receiver 
##' @param moorData Dataframe containing the sensor data from a single IMOS mooring.
##' @param moorName Character string specifying mooring name using IMOS ID code.
##' @param dateStart Optional character string of start date of date range of mooring data to plot, as "YYYY-MM-DD"
##' @param dateEnd Optional character string of end date of date range of mooring data to plot, as "YYYY-MM-DD"
##' @param varName Character string of mooring sensor variable of interest. Can be "temperature", "ucur" for meridional (u) velocity, "vcur" for zonal (v) velocity
##' @param trackingData Dataframe containing tag detections data to plot. Takes output from getDistance() function as input. Set as NULL for a base plot with no detections.
##' @param speciesID Character string describing species or tag identifier.
##' @param IDtype Character string describing type of species or tag identifier. Can be CAAB code, tag ID, common name or scientific name. Must match identifier in detections dataset.
##' @param detStart Optional character string of start date of date range of detections data to overlay on depth-time plot.
##' @param detEnd Optional character string of end date of date range of detections data to overlay on depth-time plot.
##' @importFrom lubridate as_date
##' @importFrom dplyr mutate filter group_by summarise 
##' @importFrom plotly plot_ly layout subplot
##' @importFrom viridis scale_fill_viridis
##' @export

plotDT <- function(moorData, 
                   moorName,
                   dateStart=NULL, 
                   dateEnd=NULL, 
                   varName = c("temperature", "vcur", "ucur", "psal"),
                   trackingData=NULL,
                   speciesID,
                   IDtype = "CAAB",
                   detStart=NULL,
                   detEnd=NULL){ 
  
  varName <- match.arg(varName, choices = c("temperature", "vcur", "ucur", "psal"))
  IDtype <- match.arg(IDtype, choices = c("CAAB","tag_id","species_common_name", "species_scientific_name"))
  
  if(!varName %in% c("temperature","vcur","ucur","psal")) 
    stop('Only "temperature", "vcur", "ucur" or "psal" can be provided as a valid varName')
  
  moorData <- moorData %>% 
    dplyr::mutate(mooring.date = lubridate::as_date(moor_timestamp), 
                  depth_bin = round((moor_depth/5))*5) 
  if (!is.null(dateStart) | !is.null(dateEnd)){
    dateStart <- lubridate::as_date(dateStart); dateEnd <- lubridate::as_date(dateEnd)
    moorData <- moorData %>% 
      dplyr::filter(mooring.date>=dateStart & mooring.date<=dateEnd)
  }
  moorData <- moorData %>% 
    dplyr::filter(moor_site_code == moorName)
  if (varName == "temperature"){  
    datplot <- moorData %>% 
      group_by(mooring.date,depth_bin) %>% 
      suppressMessages(
        summarise(moor_sea_temp = median(moor_sea_temp)) 
      )
    p1 <- plot_ly(
      data = datplot,
      x = ~mooring.date, y = ~depth_bin, z = ~moor_sea_temp, #text = ~paste('V2: ', V2),
      hovertemplate = paste('<br><b>Depth (m)</b>: %{y}',
                            '<br><b>Sea temperature (°C)</b>: %{z}<extra></extra> '),
      name = " ",
      type = "heatmap",
      colorbar = list(title = "Sea Temperature (°C)", len = 0.5, y = 0.5)) %>% 
      layout(
      yaxis = list(title = "Depth (m)",
                   autorange = "reversed",
                   zeroline = FALSE),
      xaxis = list(title = "Date",
                   tickformat = '%d %b %Y',
                   zeroline = FALSE)
    )
    
    #Create other plot when plotling without detections (the colourbar has different position)
    p3 <- plot_ly(
      data = datplot,
      x = ~mooring.date, y = ~depth_bin, z = ~moor_sea_temp, #text = ~paste('V2: ', V2),
      hovertemplate = paste('<br><b>Depth (m)</b>: %{y}',
                            '<br><b>Sea temperature (°C)</b>: %{z}<extra></extra> '),
      name = " ",
      type = "heatmap",
      colorbar = list(title = "Sea Temperature (°C)")) %>% 
      layout(
        yaxis = list(title = "Depth (m)",
                     autorange = "reversed",
                     zeroline = FALSE),
        xaxis = list(title = "Date",
                     tickformat = '%d %b %Y',
                     zeroline = FALSE)
      )
  } else if (varName == "ucur") {
    datplot <- moorData %>% 
      dplyr::filter(!moor_ucur=="NaN") %>% 
      group_by(mooring.date,depth_bin) %>%
      suppressMessages(
        summarise(moor_ucur = median(moor_ucur))
      )
    limit <- max(abs(datplot$moor_ucur)) * c(-1, 1)
    p1 <- plot_ly(
      data = datplot,
      x = ~mooring.date, y = ~depth_bin, z = ~moor_ucur, 
      hovertemplate = paste('<br><b>Depth (m)</b>: %{y}',
                            '<br><b>Eastward (u) velocity (m/s)</b>: %{z}<extra></extra> '),
      name = " ",
      type = "heatmap",
      colorbar = list(title = "Eastward velocity (m/s)", len = 0.5, y = 0.5),
      colorscale = "RdBu" ) %>% 
      layout(
        yaxis = list(title = "Depth (m)",
                     autorange = "reversed",
                     zeroline = FALSE),
        xaxis = list(title = "Date",
                     tickformat = '%d %b %Y',
                     zeroline = FALSE)
      )
    #Create other plot when plotling without detections (the colourbar has different position)
    p3 <- pplot_ly(
      data = datplot,
      x = ~mooring.date, y = ~depth_bin, z = ~moor_ucur, 
      hovertemplate = paste('<br><b>Depth (m)</b>: %{y}',
                            '<br><b>Eastward (u) velocity (m/s)</b>: %{z}<extra></extra> '),
      name = " ",
      type = "heatmap",
      colorbar = list(title = "Eastward velocity (m/s)"),
      colorscale = "RdBu" ) %>% 
      layout(
        yaxis = list(title = "Depth (m)",
                     autorange = "reversed",
                     zeroline = FALSE),
        xaxis = list(title = "Date",
                     tickformat = '%d %b %Y',
                     zeroline = FALSE)
      )

  } else if (varName == "vcur") {
    datplot <- moorData %>% 
      dplyr::filter(!moor_vcur=="NaN") %>%
      group_by(mooring.date,depth_bin) %>%
      suppressMessages(
        summarise(moor_vcur = median(moor_vcur))
      )
    limit <- max(abs(datplot$moor_vcur)) * c(-1, 1)
    p1 <- plot_ly(
      data = datplot,
      x = ~mooring.date, y = ~depth_bin, z = ~moor_vcur,
      hovertemplate = paste('<br><b>Depth (m)</b>: %{y}',
                            '<br><b>Northward (v) velocity (m/s)</b>: %{z}<extra></extra> '),
      name = " ",
      type = "heatmap",
      colorbar = list(title = "Northward velocity (m/s)", len = 0.5, y = 0.5),
      colorscale = "RdBu" ) %>% 
      layout(
        yaxis = list(title = "Depth (m)",
                     autorange = "reversed",
                     zeroline = FALSE),
        xaxis = list(title = "Date",
                     tickformat = '%d %b %Y',
                     zeroline = FALSE)
      )
  
    #Create other plot when plotling without detections (the colourbar has different position)    
   p3 <- plot_ly(
      data = datplot,
      x = ~mooring.date, y = ~depth_bin, z = ~moor_vcur, 
      hovertemplate = paste('<br><b>Depth (m)</b>: %{y}',
                            '<br><b>Northward (v) velocity (m/s)</b>: %{z}<extra></extra> '),
      name = " ",
      type = "heatmap",
      colorbar = list(title = "Northward velocity (m/s)"),
      colorscale = "RdBu" ) %>% 
     layout(
        yaxis = list(title = "Depth (m)",
                     autorange = "reversed",
                     zeroline = FALSE),
        xaxis = list(title = "Date",
                     tickformat = '%d %b %Y',
                     zeroline = FALSE)
      )
  } else if (varName == "psal") {
    datplot <- moorData %>% 
      dplyr::filter(!moor_psal=="NaN") %>%
      group_by(mooring.date,depth_bin) %>%
      suppressMessages(
        summarise(moor_psal = median(moor_psal))
      )
    #  limit <- max(abs(datplot$moor_psal)) * c(-1, 1)
    p1 <- plot_ly(
      data = datplot,
      x = ~mooring.date, y = ~depth_bin, z = ~moor_psal,
      hovertemplate = paste('<br><b>Depth (m)</b>: %{y}',
                            '<br><b>Practical salinity</b>: %{z}<extra></extra> '),
      name = " ",
      type = "heatmap",
      colorbar = list(title = "Practical salinity", len = 0.5, y = 0.5),
      colorscale = "RdBu" ) %>% 
      layout(
        yaxis = list(title = "Depth (m)",
                     autorange = "reversed",
                     zeroline = FALSE),
        xaxis = list(title = "Date",
                     tickformat = '%d %b %Y',
                     zeroline = FALSE)
      )
    
    #Create other plot when plotling without detections (the colourbar has different position)
    p3 <- plot_ly(
      data = datplot,
      x = ~mooring.date, y = ~depth_bin, z = ~moor_psal,
      hovertemplate = paste('<br><b>Depth (m)</b>: %{y}',
                            '<br><b>Practical salinity</b>: %{z}<extra></extra> '),
      name = " ",
      type = "heatmap",
      colorbar = list(title = "Practical salinity"),
      colorscale = "RdBu" ) %>% 
      layout(
        yaxis = list(title = "Depth (m)",
                     autorange = "reversed",
                     zeroline = FALSE),
        xaxis = list(title = "Date",
                     tickformat = '%d %b %Y',
                     zeroline = FALSE)
      )
}
  if(!is.null(trackingData)){
    trackingData <- trackingData %>% 
      dplyr::filter(moor_site_code == moorName) %>%
      mutate(detection_date = date(detection_datetime))
    if (!is.null(detStart) | !is.null(detEnd)){
      detIn <- trackingData %>% 
        dplyr::filter(detection_date>=detStart & detection_date<=detEnd)
    }
    detIn <- trackingData %>% 
      dplyr::filter(moor_site_code == moorName)
    if (IDtype == "CAAB"){
      detIn <- trackingData %>% 
        dplyr::filter(CAAB_species_id==speciesID)
    } else if (IDtype == "tag_id") {
      detIn <- trackingData %>% 
        dplyr::filter(tag_id==speciesID)
    } else if (IDtype == "species_common_name") {
      detIn <- trackingData %>% 
        dplyr::filter(species_common_name==speciesID)
    } else if (IDtype == "species_scientific_name") {
      detIn <- trackingData %>% 
        dplyr::filter(species_scientific_name==speciesID)
    }
    detUnique <- detIn %>%
      dplyr::filter(detection_date %in% datplot$mooring.date) %>%
      group_by(transmitter_id, detection_date) %>%
      summarise(n_detections = n(), species_common_name = species_common_name[1],.groups = 'drop')
    
    if (!is.null(trackingData)){
      p2 <- plot_ly(data = detUnique, x = ~detection_date, y = ~transmitter_id, type = "scatter",
                      mode = 'markers',
                      marker = list(size = 12, colorbar = list(title = "Number of detections", len = 0.5, y =0.85), 
                                    color = ~n_detections, colorscale = 'YlOrRd',
                                    reversescale = TRUE),
                      name = " ",
                      text = ~species_common_name,
                      hovertemplate = paste('<b>Number of detections</b>: %{marker.color}',
                                            '<br><b>Species</b>: %{text}<extra></extra>')
      ) %>% 
        layout(
          yaxis = list(title = ""),
          xaxis = list(title = "Date",
                       tickformat = '%d %b %Y')
        )
      subplot(p2, p1, nrows=2, shareX= TRUE, titleY = TRUE, shareY= FALSE) %>%
        layout (hovermode = "x unified") 
    }
  } else {
    p3
  }
}
