#Define pipe
`%>%` <- dplyr::`%>%`


#### Loading data ####

## First set wd to user wd so choose_file box opens to desirable path
oldwd <- getwd()
setwd(shiny::getShinyOption("wd"))

#The function generates a pop up window that allows the user to navigate through their folders to find the data
#Read the detections file downloaded from the web app
det.path <- choose_file(caption = 'Select detections data file')
detections <- data.table::fread(det.path, header=TRUE)

## revert to oldwd so app runs properly
setwd(oldwd)

#Some of the detections in the web app have wrong longitude and latitude
#Correct data for wrong latitude (positive latitude) *This is assuming all data is in Australia
detections$receiver_deployment_latitude <- ifelse(detections$receiver_deployment_latitude > 0, 
                                                  -1 * detections$receiver_deployment_latitude,
                                                  detections$receiver_deployment_latitude)

#Convert to POSIXct for Windows users
detections$detection_datetime <- as.POSIXct(detections$detection_datetime, tz = "UTC")
detections$transmitter_deployment_datetime <- as.POSIXct(detections$transmitter_deployment_datetime, tz = "UTC")




#Convert datetime to date
detections$date <- as.Date(detections$detection_datetime)




#Add month column for the season filtering (Detections over time tab)
detections$month <- format(detections$detection_datetime, format = "%B")
# detections$receiver_deployment_longitude<-format(round(detections$receiver_deployment_longitude,4), nsmall=4)
# detections$receiver_deployment_latitude<-format(round(detections$receiver_deployment_latitude,4), nsmall=4)



#Convert species common name to every word with capital letter
detections$species_common_name <- stringr::str_to_title(detections$species_common_name)

## First set wd to user wd so choose_file box opens to desirable path
setwd(shiny::getShinyOption("wd"))

#Read the receiver metadata file downloaded from the web app
receiver_meta <- data.table::fread(choose_file(caption = "Select receiver metadata file"), header=TRUE)
## revert to oldwd so app runs properly
setwd(oldwd)


# receiver_meta$receiver_deployment_longitude <- format(round(receiver_meta$receiver_deployment_longitude,4), nsmall=4)
# receiver_meta$receiver_deployment_latitude <- format(round(receiver_meta$receiver_deployment_latitude,4), nsmall=4)
receiver_meta$receiver_deployment_datetime <- as.POSIXct(receiver_meta$receiver_deployment_datetime, tz="UTC")
receiver_meta$receiver_recovery_datetime <- as.POSIXct(receiver_meta$receiver_recovery_datetime, tz="UTC")


#merged_detections <- dplyr::left_join(receiver_meta, detections)

#Group the receiver metadata by station
receiver_meta_station <- receiver_meta %>%
  dplyr::group_by(station_name) %>%
  dplyr::summarise(longitude = receiver_deployment_longitude[1],
                   latitude = receiver_deployment_latitude[1])

#Transform receiver_meta_station to a spatial dataframe
receiver_meta_station_sp<- receiver_meta_station %>% 
  sf::st_as_sf(coords = c("longitude", "latitude")) %>%
  sf::st_sf(crs = 4326)



#### Station location ####


#Receiver station location is calculated with the function det_per_station()
station_location <- det_per_station(detections)


#Transform station location to a spatial data frame
station_location_sp<- station_location %>%
  sf::st_as_sf(coords = c("longitude", "latitude")) %>%
  sf::st_sf(crs = 4326)





#Bins for number of detections per station
bins <- c(1, 500, 1000 , 5000, 10000, 50000, 100000, 200000, 400000, Inf)
#Create a palette with the bins
pal <- leaflet::colorBin("YlOrRd", domain = station_location$n.detections, bins = bins, na.color="#808080")

#Bins for number of transmitters per station
tag_bins <- c(1,20 , 30, 40, 50, 60, 70, 80, 90 ,Inf)
#Create a palette with the bins
tag_pal <- leaflet::colorBin("YlOrRd", domain = station_location$n.transmitters, bins = tag_bins, na.color="#808080")

#Bins for number of transmitters per station
species_bins <- c(1, 2, 4, 6, 8, 10, 12, 14, Inf)
#Create a palette with the bins
species_pal <- leaflet::colorBin("YlOrRd", domain = station_location$n.species, bins = species_bins, na.color="#808080")



####Stacked bar plot for the number of total detections per station####


det_species <- det_species_station(detections)
det_species <- det_species[order(det_species$species_common_name, decreasing = TRUE),]
det_species <- det_species %>% 
  dplyr::rename(n.detections = n.species)

det_species_pal <- leaflet::colorFactor(palette = "Paired", domain = levels(as.factor(det_species$species_common_name)))

det_species$species_colour <- det_species_pal(det_species$species_common_name)



#Palette for Detections over time tab
receiver_pal <- leaflet::colorFactor(palette = "Paired", domain = levels(as.factor(detections$receiver_name)))

## Timezone for detections per hour plot
timezone_list <- c("UTC", "Australia/Adelaide", "Australia/Brisbane", "Australia/Broken_Hill",
                   "Australia/Darwin", "Australia/Hobart", "Australia/Lindeman", "Australia/Lord_Howe",
                   "Australia/Melbourne", "Australia/Perth", "Australia/Sydney")





#### Start of Shiny App ####
#Note that the ui and server are contained in the same script

#The theme for the app is default from the bootstrap themes -> flatly
#The shiny app includes another styles (font and button format) in the style.css file
#The shiny app is divided by tabs (tab panels) which contain different visualisations and statistics
#The structure is: Detections map | Detections overview | Transmitters detections | Detections over time | Receiver effiency

ui <- shiny::bootstrapPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  #Code for creating the navigation bar at the top of the web page
  shiny::navbarPage(
    windowTitle = "remora Receiver Report",
    theme = shinythemes::shinytheme("flatly"),
    collapsible = TRUE,
    id = "nav",
    title = htmltools::div(
      htmltools::img(
        src = 'white_remora.png',
        height = '60',
        width = '183',
        style = "vertical-align:middle"
      ),
      style = "margin-top:-16px; margin-right:-40px; margin-left:-40px"
    ),
    #Station location tab
    shiny::tabPanel(
      "Detections map",
      htmltools::div(
        class = "outer",
        tags$head(htmltools::includeCSS("styles.css")),
        tags$style(
          type = "text/css",
          "html, body {width:100%;height:100%}",
          ".leaflet .legend i{
                                     border-radius: 50%;
                                     width: 10px;
                                     height: 10px;
                                     margin-top: 4px;
                                     line-height: 10px;
                                     }"
        ),
        
        leaflet::leafletOutput("mymap", width = "100%", height =
                                 "100%"),
        
        #Draggable left panel
        shiny::absolutePanel(
          id = "controls",
          class = "panel-default",
          top = 75,
          left = 70,
          width = 250,
          fixed = TRUE,
          draggable = TRUE,
          height = "auto",
          
          #Reactive text that changes as user interacts with map
          htmltools::h3(textOutput("detections_count"), align = "right"),
          htmltools::h4(textOutput("tags_count"), align = "right"),
          htmltools::h5(textOutput("species_count"), align = "right"),
          
          
          #Slider for the transmitter deployment datetimes
          shiny::sliderInput(
            "date",
            htmltools::h5("Detection datetime"),
            ticks = FALSE,
            min = min(detections$detection_datetime, na.rm = TRUE),
            max = max(detections$detection_datetime, na.rm =
                        TRUE),
            value = range(
              min(detections$detection_datetime, na.rm = TRUE),
              max(detections$detection_datetime, na.rm =
                    TRUE)
            ),
            step = 100
          ),
          shinyWidgets::prettyRadioButtons("legend",
                                           htmltools::h5("Select data to display:"),
                                           inline = TRUE,
                                           choices = c("Number of detections", 
                                                       "Number of transmitters", "Number of species"),
                                           selected = "Number of detections",
                                           icon = shiny::icon("check"),
                                           bigger = TRUE,
                                           status = "info",
                                           animation = "smooth")
        ),
        #IMOS logo that appears at the bottom left of the page
        shiny::absolutePanel(
          id = "logo",
          class = "card",
          bottom = 60,
          left = 60,
          width = 80,
          fixed = TRUE,
          draggable = FALSE,
          height = "auto",
          tags$a(
            href = 'https://animaltracking.aodn.org.au/',
            tags$img(
              src = 'imos-logo.png',
              height = '100',
              width = '200'
            )
          )
        )
        
      )
    ),
    #Total detections per station stacked bar plot tab
    shiny::tabPanel("Detections overview",
                    shiny::sidebarLayout(
                      shiny::sidebarPanel(
                        #Filter by station
                        shinyWidgets::pickerInput("station_select", 
                                                  htmltools::h5("Select one or more station(s):"), 
                                                  choices = c("All stations", sort(unique(det_species$station_name))),
                                                  multiple = TRUE, selected = "All stations"),
                        #Filter by species
                        shinyWidgets::pickerInput("species_select", 
                                                  htmltools::h5("Select species:"), 
                                                  choices = c("All species", sort(unique(det_species$species_common_name))),
                                                  multiple = TRUE, selected = "All species"),
                        #Action button for refreshing the plot
                        shiny::actionButton("refresh", "Reset filters", icon=shiny::icon("redo-alt")),
                        htmltools::br(),
                        htmltools::br(),
                        #Detections table 
                        htmltools::p(
                          "Summary table: detections per species per station"
                        ),
                        DT::DTOutput("species_det_table")
                      ),
                      
                      shiny::mainPanel (
                        htmltools::h3("Total number of detections per species per station"),
                        htmltools::br(),
                        #Stacked bar plot of the detections per station
                        plotly::plotlyOutput("detections_station", width = "100%", height = "110%") %>% shinycssloaders::withSpinner(color =
                                                                                                                                       "#3b6e8f", type = 7),
                        htmltools::br(),
                        htmltools::br(),
                        htmltools::p(paste("\U2022","Hover on the plot to see station name, species common name and total detections for each species.")),
                        htmltools::br(),
                        htmltools::p(paste("\U2022","Save the plot by clicking the camera icon in the top right corner.")),
                        htmltools::br(),
                        htmltools::p(paste("\U2022","Download the summary table in .CSV, Excel or .PDF format"))
                        
                      )
                    )),
    #Total tags per station stacked bar plot tab
    shiny::tabPanel(
      "Transmitters overview",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          #Filter by station
          shinyWidgets::pickerInput("station_select2", 
                                    htmltools::h5("Select one or more station(s):"), 
                                    choices = c("All stations", sort(unique(det_species$station_name))),
                                    multiple = TRUE, selected = "All stations"),
          
          #Filter by species
          shinyWidgets::pickerInput("species_select2", 
                                    htmltools::h5("Select species:"), 
                                    choices = c("All species", sort(unique(det_species$species_common_name))),
                                    multiple = TRUE, selected = "All species"),
          
          shiny::actionButton("refresh2", "Reset filters", icon = shiny::icon("redo-alt")),
          htmltools::br(),
          htmltools::br(),
          htmltools::p(
            "Summary table: transmitters detected per species per station"
          ),
          DT::DTOutput("species_tags_table")
        ),
        
        shiny::mainPanel (
          htmltools::h3("Total number of transmitters detected per species per station"),
          htmltools::br(),
          plotly::plotlyOutput("tags_station", width = "100%", height = "110%") %>% shinycssloaders::withSpinner(color =
                                                                                                                   "#3b6e8f", type = 7),
          htmltools::br(),
          htmltools::br(),
          htmltools::p(paste("\U2022","Hover on the plot to see station name, species common name and total transmitters detected for each species.")),
          htmltools::br(),
          htmltools::p(paste("\U2022","Save the plot by clicking the camera icon in the top right corner.")),
          htmltools::br(),
          htmltools::p(paste("\U2022","Download the summary table in .CSV, Excel or .PDF format"))
          
        )
      )
    ),
    shiny::tabPanel("Detections over time",
                    shiny::sidebarLayout(
                      shiny::sidebarPanel(width = 3,
                                          shinyWidgets::pickerInput(inputId = "select_installation",
                                                                    label = htmltools::h5("Select installation"),
                                                                    choices = c(sort(unique(detections$installation_name))),
                                                                    multiple = TRUE,
                                                                    selected = c(sort(unique(detections$installation_name)))[1],
                                                                    options = list('none-selected-text' = 'Please select installation')
                                          ),
                                          shinyWidgets::pickerInput(inputId = "select_station",
                                                                    label = htmltools::h5("Select station"),
                                                                    choices = c(sort(unique(detections$station_name))),
                                                                    multiple = TRUE,
                                                                    selected = c(sort(unique(detections$station_name)))[1],
                                                                    options = list('none-selected-text' = 'Please select station')
                                          ),
                                          shinyWidgets::pickerInput(inputId = "select_month",
                                                                    label = htmltools::h5("Select month"),
                                                                    choices = c("All months","January", "February", "March", "April", "May",
                                                                                "June", "July", "August", "September", "October", "November",
                                                                                "December"),
                                                                    multiple = TRUE,
                                                                    selected = "All months",
                                                                    options = list('none-selected-text' = 'Please select month')
                                          ),
                                          shinyWidgets::pickerInput(inputId = "select_species",
                                                                    label = htmltools::h5("Select species"),
                                                                    choices = c("All species", sort(unique(detections$species_common_name))),
                                                                    multiple = FALSE,
                                                                    selected = "All species"
                                          ),
                                          shinyWidgets::pickerInput(inputId = "select_timezone",
                                                                    label = htmltools::h5("Select timezone"),
                                                                    choices = timezone_list,
                                                                    multiple = FALSE,
                                                                    selected = "UTC"
                                          ),
                                          shiny::actionButton("refresh3", "Reset filters", icon = shiny::icon("redo-alt"))
                      ),
                      shiny::mainPanel(
                        #Create tabs for each plot
                        shiny::tabsetPanel(type="tabs",
                                           shiny::tabPanel("Detections per station", 
                                                           htmltools::h3("Detections over time recorded for the station selected"),
                                                           #Number of detections vs time plot
                                                           plotly::plotlyOutput("detections_time", width="100%", 
                                                                                height = "100%")%>% shinycssloaders::withSpinner(color ="#3b6e8f", type = 7),
                                                           htmltools::br(),
                                                           htmltools::br(),
                                                           htmltools::p(paste("\U2022","Hover on the plot to see date, number of detections and receiver name.")),
                                                           htmltools::br(),
                                                           htmltools::p(paste("\U2022","Double click on the receiver name on the legend to see the detections just for the selected receiver")),
                                                           htmltools::br(),
                                                           htmltools::p(paste("\U2022","Save the plot by clicking the camera icon in the top right corner."))
                                           ),
                                           shiny::tabPanel("Detections per hour",
                                                           htmltools::h3("Detections per hour for the station selected"),
                                                           plotly::plotlyOutput("detections_hour", width="100%", 
                                                                                height="100%")%>% shinycssloaders::withSpinner(color ="#3b6e8f", type = 7),
                                                           htmltools::br(),
                                                           htmltools::br(),
                                                           htmltools::p(paste("\U2022","Hover on the plot to see first quartile (Q1), median and third quartile (Q3).")),
                                                           htmltools::br(),
                                                           htmltools::p(paste("\U2022","Save the plot by clicking the camera icon in the top right corner."))
                                           )
                        )
                        
                      ))),
    shiny::tabPanel("Array performance",
                    shiny::sidebarLayout(
                      shiny::sidebarPanel(width = 3,
                                          shinyWidgets::pickerInput(inputId = "select_installation2",
                                                                    label = htmltools::h5("Select installation"),
                                                                    choices = c(sort(unique(receiver_meta$installation_name))),
                                                                    multiple = FALSE,
                                                                    selected = c(sort(unique(receiver_meta$installation_name)))[1],
                                                                    options = list('none-selected-text' = 'Please select installation')
                                          ),
                                          shiny::sliderInput("date_range",
                                                             htmltools::h5("Select date range"),
                                                             ticks = FALSE,
                                                             min = min(receiver_meta$receiver_deployment_datetime, na.rm = TRUE),
                                                             max = max(receiver_meta$receiver_recovery_datetime, na.rm =
                                                                         TRUE),
                                                             value = range(
                                                               min(receiver_meta$receiver_deployment_datetime, na.rm = TRUE),
                                                               max(receiver_meta$receiver_recovery_datetime, na.rm =
                                                                     TRUE)
                                                             ),
                                                             step = 100),
                                          
                                          shiny::actionButton("refresh4", "Reset filters", icon = shiny::icon("redo-alt")),
                                          htmltools::br(),
                                          htmltools::br(),
                                          htmltools::p("Summary table: receiver(s) deployed at each station for the selected time period"),
                                          DT::DTOutput("receivers_station_table")
                      ),
                      shiny::mainPanel(
                        htmltools::h3("Station efficiency index"),
                        shiny::actionButton("learn_more", "Learn more", icon = shiny::icon("book-reader"), 
                                            onclick = "window.open('https://doi.org/10.1016/j.fishres.2018.09.015', '_blank' )"),
                        htmltools::br(),
                        htmltools::br(),
                        #Text that appears when selected date is out of range
                        shiny::textOutput("selected_date")%>% 
                          htmltools::tagAppendAttributes(style= 'font-size:20px; color:red;'),
                        plotly::plotlyOutput("lollipop", width="100%", 
                                             height="100%")%>% shinycssloaders::withSpinner(color ="#3b6e8f", type = 7),
                        htmltools::br(),
                        htmltools::br(),
                        htmltools::p(paste("\U2022","Save the plot by clicking the camera icon in the top right corner.")),
                        htmltools::br(),
                        htmltools::p(paste("\U2022","Download the summary table in .CSV, Excel or .PDF format"))
                      )))
  ))


#Server for the shiny app: contains all the outputs and the reactive data
server <- function (input, output, session){
  
  ####Reactive data (changes when user selects inputs)####
  
  ##Reactive data for receiver location map (changes when selecting date)
  reactive_data <- shiny::reactive({detections[detections$detection_datetime >= input$date[1] &
                                                 detections$detection_datetime <= input$date[2], ]
    
  })
  
  ##Reactive data for the station location
  
  reactive_station_location <- shiny::reactive({
    subset<-detections[detections$detection_datetime >= input$date[1] &
                         detections$detection_datetime <= input$date[2], ] 
    return(det_per_station(subset))
  })
  
  ##Reactive data for the station location spatial dataframe
  reactive_station_location_sp <- shiny::reactive({
    subset_sp<-detections[detections$detection_datetime >= input$date[1] &
                            detections$detection_datetime <= input$date[2], ] 
    location_subset_sp<- det_per_station(subset_sp)%>%
      sf::st_as_sf(coords = c("longitude", "latitude")) %>%
      sf::st_sf(crs = 4326)
    
    
    return(location_subset_sp)
    
  })
  ## Reactive data for detections overview tab
  shiny::observeEvent (input$station_select,{
    shiny::req(input$station_select, input$species_select)
    if(input$station_select != "All stations" || length(input$station_select) > 1){
      shinyWidgets::updatePickerInput(session = session, inputId = "species_select",
                                      choices = "All species", selected = "All species")
    }else{
      shinyWidgets::updatePickerInput(session = session, inputId = "species_select",
                                      choices = c("All species", unique(det_species$species_common_name)), selected = "All species")
    }
  }, ignoreInit = TRUE)
  
  
  
  reactive_detections <- shiny::reactive({
    shiny::req(input$station_select, input$species_select)
    if (input$station_select == "All stations" && input$species_select == "All species"){
      return(det_species)
    }else if(input$species_select == "All species" && input$station_select != ""){
      det_species <- det_species %>% 
        dplyr::filter(station_name %in% input$station_select)
      return(det_species)
    }else if(input$station_select == "All stations" && input$species_select != ""){
      det_species <- det_species %>%
        dplyr::filter(species_common_name %in% input$species_select)
      return(det_species)
    }else if(input$station_select != "" && input$species_select != ""){
      det_species <- det_species %>% 
        dplyr::filter(station_name %in% input$station_select) %>%
        dplyr::filter(species_common_name %in% input$species_select)
      return(det_species)
    }else{
      return(NULL)
    }
  })
  
  ## Reactive data for the Transmitter overview tab
  shiny::observeEvent (input$station_select2,{
    shiny::req(input$station_select2, input$species_select2)
    if(input$station_select2 != "All stations" || length(input$station_select2) > 1){
      shinyWidgets::updatePickerInput(session = session, inputId = "species_select2",
                                      choices = "All species", selected = "All species")
    }else{
      shinyWidgets::updatePickerInput(session = session, inputId = "species_select2",
                                      choices = c("All species", unique(det_species$species_common_name)), selected = "All species")
    }
  }, ignoreInit = TRUE)      
  
  reactive_tags <- shiny::reactive({
    shiny::req(input$station_select2, input$species_select2)
    if (input$station_select2 == "All stations" && input$species_select2 == "All species"){
      return(det_species)
    }else if(input$species_select2 == "All species" && input$station_select2 != ""){
      det_species <- det_species %>% 
        dplyr::filter(station_name %in% input$station_select2)
      return(det_species)
    }else if(input$station_select2 == "All stations" && input$species_select2 != ""){
      det_species <- det_species %>%
        dplyr::filter(species_common_name %in% input$species_select2)
      return(det_species)
    }else if(input$station_select2 != "" && input$species_select2 != ""){
      det_species <- det_species %>% 
        dplyr::filter(station_name %in% input$station_select2) %>%
        dplyr::filter(species_common_name %in% input$species_select2)
      return(det_species)
    }else{
      return(NULL)
    }
  })
  
  ##Reactive data for detections per date filtered by station
  
  shiny::observeEvent (input$select_installation,{
    shiny::req(input$select_installation)
    subset <- detections %>% 
      dplyr::filter(installation_name %in% input$select_installation)
    
    shinyWidgets::updatePickerInput(session = session, inputId = "select_station",
                                    choices = sort(unique(subset$station_name)),
                                    options = list('none-selected-text' = 'Please select station'))
  }, ignoreInit = TRUE) 
  
  
  
  reactive_station_sub <- shiny::reactive({
    shiny::req(input$select_installation,input$select_station, input$select_month, input$select_species, input$select_timezone)
    
    if (input$select_timezone != "UTC"){
      detections <- data.table::setDF(detections) %>% 
        dplyr::mutate(detection_local_datetime = purrr::map2(.x = detection_datetime, .y = input$select_timezone, 
                                                             .f = function(x, y) {with_tz(time = x, tzone = y)}))%>%
        tidyr::unnest(detection_local_datetime)
      detections$month <- format(detections$detection_local_datetime, format = "%B")
    }else{
      detections$month <- format(detections$detection_datetime, format = "%B")
    }
    
    if(input$select_installation != "" && input$select_station != "" && 
       input$select_month == "All months" && input$select_species == "All species"){
      
      subset <- detections %>% 
        dplyr::filter(station_name %in% input$select_station)%>%
        dplyr::filter(installation_name %in% input$select_installation)%>%
        dplyr::group_by(receiver_name, date) %>%
        dplyr::summarise(n.detections = dplyr::n(), .groups = "drop")
      return(subset)
      
    }else if(input$select_installation != "" && input$select_station != "" && 
             input$select_month == "All months" && input$select_species != ""){
      subset <- detections %>% 
        dplyr::filter(species_common_name %in% input$select_species) %>%
        dplyr::filter(station_name %in% input$select_station)%>%
        dplyr::filter(installation_name %in% input$select_installation)%>%
        dplyr::group_by(receiver_name, date) %>%
        dplyr::summarise(n.detections = dplyr::n(), .groups = "drop")
      return(subset)
      
    }else if(input$select_installation != "" && input$select_station != "" && 
             input$select_month != "" && input$select_species == "All species"){
      subset <- detections %>% 
        dplyr::filter(station_name %in% input$select_station) %>%
        dplyr::filter(installation_name %in% input$select_installation)%>%
        dplyr::filter(month %in% input$select_month)%>%
        dplyr::group_by(receiver_name, date) %>%
        dplyr::summarise(n.detections = dplyr::n(), .groups = "drop")
      return(subset)
      
    }else if(input$select_installation != "" && input$select_station != "" && 
             input$select_month != "" && input$select_species != ""){
      subset <- detections %>%
        dplyr::filter(station_name %in% input$select_station) %>%
        dplyr::filter(installation_name %in% input$select_installation)%>%
        dplyr::filter(month %in% input$select_month)%>%
        dplyr::filter(species_common_name %in% input$select_species) %>%
        dplyr::group_by(receiver_name, date) %>%
        dplyr::summarise(n.detections = dplyr::n(), .groups = "drop")
      return(subset)
    }else{
      return(NULL)
    }
  })
  
  reactive_receiver_pal <- reactive ({
    shiny::req(input$select_installation,input$select_station, input$select_month, input$select_species, input$select_timezone,
               reactive_station_sub())
    
    colourCount <- length(unique(reactive_station_sub()$receiver_name))
    getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))
    return(getPalette(colourCount))
    
  })
  
  
  ##Reactive data for detections per hour filtered by station
  reactive_hour_station <- shiny::reactive({
    shiny::req(input$select_installation,input$select_station, input$select_month, 
               input$select_species, input$select_timezone)
    
    #Convert to local datetime using receiver deployment coordinates. This will just be used for the detections per hour plot
    if (input$select_timezone != "UTC"){
      detections <- data.table::setDF(detections) %>% 
        dplyr::mutate(detection_local_datetime = purrr::map2(.x = detection_datetime, .y = input$select_timezone, 
                                                             .f = function(x, y) {with_tz(time = x, tzone = y)}))%>%
        tidyr::unnest(detection_local_datetime)
      detections$hour<-format(detections$detection_local_datetime, format="%H")
      detections$hour<-as.numeric(detections$hour)
    }else{
      detections$hour<-format(detections$detection_datetime, format="%H")
      detections$hour<-as.numeric(detections$hour)
    }
    
    
    
    
    if(input$select_installation != "" && input$select_station != "" && 
       input$select_month == "All months" && input$select_species == "All species"){
      
      subset <- detections %>% 
        dplyr::filter(station_name %in% input$select_station)%>%
        dplyr::filter(installation_name %in% input$select_installation)%>%
        dplyr::group_by(station_name, hour, date)%>%
        dplyr::summarise(n.detections=dplyr::n(), n.species=length(unique(species_common_name)), .groups="drop")
      return(subset)
      
    }else if (input$select_installation != "" && input$select_station != "" && 
              input$select_month == "All months" && input$select_species != ""){
      subset <- detections %>%
        dplyr::filter(station_name %in% input$select_station)%>%
        dplyr::filter(installation_name %in% input$select_installation)
      
      if (input$select_species %in% subset$species_common_name){
        subset<-subset %>%
          dplyr::filter(species_common_name %in% input$select_species)%>%
          dplyr::group_by(station_name, hour, date)%>%
          dplyr::summarise(n.detections=dplyr::n(), n.species=length(unique(species_common_name)), .groups="drop")
        return(subset)
      }else{
        return(NULL)
      }
      
    }else if(input$select_installation != "" && input$select_station != "" && 
             input$select_month != "" && input$select_species == "All species"){
      subset <- detections %>% 
        dplyr::filter(station_name %in% input$select_station) %>%
        dplyr::filter(installation_name %in% input$select_installation)%>%
        dplyr::filter(month %in% input$select_month)%>%
        dplyr::group_by(station_name, hour, date)%>%
        dplyr::summarise(n.detections=dplyr::n(), n.species=length(unique(species_common_name)), .groups="drop")
      return(subset)
      
    }else if(input$select_installation != "" && input$select_station != "" && 
             input$select_month != "" && input$select_species != ""){
      subset <- detections %>%
        dplyr::filter(station_name %in% input$select_station) %>%
        dplyr::filter(installation_name %in% input$select_installation)%>%
        dplyr::filter(month %in% input$select_month)
      if (input$select_species %in% subset$species_common_name){
        subset <- subset %>%
          dplyr::filter(species_common_name %in% input$select_species) %>%
          dplyr::group_by(station_name, hour, date)%>%
          dplyr::summarise(n.detections=dplyr::n(), n.species=length(unique(species_common_name)), .groups="drop")
        return(subset)
      }else{
        return(NULL)
      }
      
    }else{
      return(NULL)
    }
  })
  
  ## Reactive data for station efficiency index
  reactive_detections_sei <- shiny::reactive({
    detections %>%
      dplyr::filter(detection_datetime >= input$date_range[1] &
                      detection_datetime <= input$date_range[2])
  })
  
  reactive_receivers_sei <- shiny::reactive({
    receiver_meta %>%
      dplyr::filter((receiver_deployment_datetime >= input$date_range[1] &
                       receiver_deployment_datetime <= input$date_range[2]) |
                      (receiver_recovery_datetime >= input$date_range[1] &
                         receiver_recovery_datetime <= input$date_range[2]) |
                      (receiver_deployment_datetime < input$date_range[1] &
                         receiver_recovery_datetime > input$date_range[2]))
  })
  
  
  reactive_sei <- shiny::reactive({
    shiny::req(input$select_installation2, input$date_range)
    
    
    if(input$select_installation != ""){
      
      merged_data <- suppressMessages(dplyr::left_join(reactive_receivers_sei(), reactive_detections_sei()))
      
      merged_data <- merged_data %>%
        dplyr::filter(installation_name %in% input$select_installation2)
      
      if(nrow(merged_data)==0){
        return(NULL)
      }else{
        sei_subset <- sei(data = merged_data, date1 = input$date_range[1], date2 = input$date_range[2])
        
        
        
        plot_data <- sei_subset %>%
          dplyr::arrange(index) %>%
          dplyr::mutate(rank = dplyr::row_number())
        return(plot_data)
      }
      
      
    }else{
      return(NULL)
    }
  })
  
  reactive_station_table <- shiny::reactive({
    shiny::req(input$select_installation2,input$date_range)
    
    
    if(input$select_installation != ""){
      merged_data <- suppressMessages(dplyr::left_join(reactive_receivers_sei(), reactive_detections_sei()))
      

      
      merged_data <- merged_data %>%
        dplyr::filter(installation_name %in% input$select_installation2)
      
      subset <- merged_data[, c("station_name", "receiver_name")]
      
      subset<- subset %>%
        dplyr::group_by(receiver_name)%>%
        dplyr::summarise(station_name = station_name[1])
      
    } else {
      subset <- merged_data[, c("station_name", "receiver_name")]
      subset<- subset %>%
        dplyr::group_by(receiver_name)%>%
        dplyr::summarise(station_name = station_name[1])
    }
    return(subset)
  })
  
  ##### Station location ####
  ##Count detections 
  output$detections_count <- shiny::renderText({
    paste0(prettyNum(n_detections(reactive_data()), big.mark=","), " detections")
  })
  
  ##Count tags
  output$tags_count <- shiny::renderText({
    paste0(prettyNum(n_tags(reactive_data()), big.mark=","), " transmitters")
  })
  
  ##Count species
  
  output$species_count <- shiny::renderText({
    paste0(prettyNum(n_species(reactive_data()), big.mark=","), " species")
  })
  
  
  
  #Reactive text when hovering on the bubble of the station location map
  mytext = shiny::reactive(paste(
    "Station name:", reactive_station_location()$station_name, "<br/>",
    "Installation name:",reactive_station_location()$installation_name, "<br/>",
    "n. detections: ", reactive_station_location()$n.detections, "<br/>",
    "n. species: ", reactive_station_location()$n.species, "<br/>",
    "n. transmitters:", reactive_station_location()$n.transmitters, "<br/>",
    "Longitude: ", reactive_station_location()$longitude, "<br/>",
    "Latitude: ", reactive_station_location()$latitude, sep="")%>%
      lapply(htmltools::HTML))
  
  ##Text that appears when there are no detections
  text = paste(
    "Station name:", station_location$station_name, "<br/>",
    "Installation name:",station_location$installation_name, "<br/>",
    "n. detections: ", 0, "<br/>",
    "n. species: ", 0, "<br/>",
    "n. transmitters: ", 0, "<br/>",
    "Longitude: ", station_location$longitude, "<br/>",
    "Latitude: ", station_location$latitude, sep="")%>%
    lapply(htmltools::HTML)
  
  #Leaflet basemap for all the maps
  basemap <- shiny::reactive({ leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group="Satellite")%>%
      leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap, group="Bathymetry")%>%
      leaflet::addScaleBar(position='topright')%>%
      leaflet::addLayersControl(
        baseGroups = c("Satellite","Bathymetry"),  # specify the desired basemap options for the output map
        options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
      leaflet::setView(lng = mean(as.numeric(detections$receiver_deployment_longitude)), 
              lat = mean(as.numeric(detections$receiver_deployment_latitude)), zoom = 13)
    
  })
  
  #Render basemap
  output$mymap <- leaflet::renderLeaflet({
    basemap()
  })
  
  #Render receiver locations
  shiny::observeEvent(c(reactive_station_location_sp(),input$legend), {
    if (input$legend =="Number of detections"){
      leaflet::leafletProxy("mymap", data=reactive_station_location_sp()) %>% 
        leaflet::clearMarkers() %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addMeasure(
          primaryLengthUnit = "kilometers",
          secondaryLengthUnit = "miles",
          primaryAreaUnit = "hectares",
          secondaryAreaUnit = "acres",
          position = 'topleft')%>%
        leaflet::addCircles(data=receiver_meta_station_sp,
                            color="grey",
                            radius=5,
                            label= text)%>%
        leaflet::addCircleMarkers(
          fillColor = ~pal(n.detections),
          fillOpacity=.9,
          radius= 20,
          label= mytext(),
          stroke=FALSE
        ) %>%
        leaflet::addLegend(pal = pal, values = station_location_sp$n.detections, opacity=.9,
                           position = "topright", title="Number of detections")
    }else if(input$legend == "Number of transmitters"){
      leaflet::leafletProxy("mymap", data = reactive_station_location_sp()) %>% 
        leaflet::clearMarkers() %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addMeasure(
          primaryLengthUnit = "kilometers",
          secondaryLengthUnit = "miles",
          primaryAreaUnit = "hectares",
          secondaryAreaUnit = "acres",
          position = 'topleft')%>%
        leaflet::addCircles(data=receiver_meta_station_sp,
                            color="grey",
                            radius=5,
                            label= text)%>%
        leaflet::addCircleMarkers(
          fillColor = ~tag_pal(n.transmitters),
          fillOpacity=.9,
          radius= 20,
          label= mytext(),
          stroke=FALSE
        ) %>%
        leaflet::addLegend(pal = tag_pal, values = station_location_sp$n.transmitters, opacity=.9,
                           position = "topright", title="Number of transmitters")
    }else if(input$legend=="Number of species"){
      leaflet::leafletProxy("mymap", data=reactive_station_location_sp()) %>% 
        leaflet::clearMarkers() %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addMeasure(
          primaryLengthUnit = "kilometers",
          secondaryLengthUnit = "miles",
          primaryAreaUnit = "hectares",
          secondaryAreaUnit = "acres",
          position = 'topleft')%>%
        leaflet::addCircles(data=receiver_meta_station_sp,
                            color="grey",
                            radius=5,
                            label= text)%>%
        leaflet::addCircleMarkers(
          fillColor = ~species_pal(n.species),
          fillOpacity=.9,
          radius= 20,
          label= mytext(),
          stroke=FALSE
        ) %>%
        leaflet::addLegend(pal = species_pal, values = station_location_sp$n.species, opacity=.9,
                           position = "topright", title="Number of species")
    }else{
      leaflet::leafletProxy("mymap", data=reactive_station_location_sp()) %>% 
        leaflet::clearMarkers() %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addMeasure(
          primaryLengthUnit = "kilometers",
          secondaryLengthUnit = "miles",
          primaryAreaUnit = "hectares",
          secondaryAreaUnit = "acres",
          position = 'topleft')%>%
        leaflet::addCircles(data=receiver_meta_station_sp,
                            color="grey",
                            radius=5,
                            label= text)
    }
    
    
  })
  
  
  
  ####Detections overview####
  
  #Render stacked bar plot
  output$detections_station <- plotly::renderPlotly({
    
    
    if (is.null(reactive_detections()))
      return(NULL)
    
    plotly::plot_ly(reactive_detections(), x = ~station_name, y = ~n.detections, text = ~species_common_name, type = 'bar',
                    textposition = 'none', marker = list(color = ~species_colour), hovertemplate = paste('%{x}', '<br>%{text}<br>', '%{y}','<extra></extra>')) %>%
      plotly::layout(yaxis = list(title = 'Number of detections', 
                                  tickfont=list(size=14),  titlefont=list(size=18)),
                     xaxis = list(title='Station name',
                                  tickfont=list(size=14),  titlefont=list(size=18)),
                     barmode = 'stack',
                     bargap=20
      )
    
    
    
  })
  
  ## Render table made with reactive data
  output$species_det_table <- DT::renderDT({
    DT::datatable(reactive_detections(),
                  extensions=c("Buttons","Scroller"), style="bootstrap", class="compact", width="100%",
                  options=list(dom = "lrtipB",deferRender=TRUE, scrollY=300, scroller=TRUE,
                               buttons=c('csv', 'excel', 'pdf'),
                               columnDefs = list(list(visible = FALSE, targets = c(4,5)))
                  ))
  }, server = FALSE)
  
  
  #Refresh plot button will update the filters and change them to all stations and all species
  shiny::observeEvent(input$refresh,{
    shinyWidgets::updatePickerInput(session = session, inputId = "species_select",
                                    choices = c("All species", unique(det_species$species_common_name)), selected = "All species")
    shinyWidgets::updatePickerInput(session = session, inputId = "station_select",
                                    choices = c("All stations", unique(det_species$station_name)), selected = "All stations")
  })
  
  
  ####Transmitters overview####
  
  
  output$tags_station <- plotly::renderPlotly({
    
    tags_bar_plot = plotly::plot_ly(reactive_tags(), x = ~station_name, y = ~n.tags, text= ~species_common_name,
                                    textposition = 'none', type = 'bar', marker = list(color= ~species_colour),
                                    hovertemplate = paste('%{x}', '<br>%{text}<br>', '%{y}','<extra></extra>')) %>%
      plotly::layout(yaxis = list(title = 'Number of transmitters',
                                  tickfont=list(size=14),  titlefont=list(size=18)),
                     xaxis = list(title='Station name',
                                  tickfont=list(size=14),  titlefont=list(size=18)),
                     barmode = 'stack',
                     bargap=20 
      )
    
  })
  
  
  
  #Render table made with crosstalk and the download buttons
  output$species_tags_table <- DT::renderDT({
    DT::datatable(reactive_tags(),
                  extensions=c("Buttons","Scroller"), style="bootstrap", class="compact", width="100%",
                  options=list(dom = "lrtipB",deferRender=TRUE, scrollY=300, scroller=TRUE,
                               buttons=c('csv', 'excel', 'pdf') ,
                               columnDefs = list(list(visible = FALSE, targets = c(3,5)))
                  ))
  }, server = FALSE)
  
  #Refresh plot button will update the filters and change them to all stations and all species
  shiny::observeEvent(input$refresh2,{
    shinyWidgets::updatePickerInput(session = session, inputId = "species_select2",
                                    choices = c("All species", unique(det_species$species_common_name)), selected = "All species")
    shinyWidgets::updatePickerInput(session = session, inputId = "station_select2",
                                    choices = c("All stations", unique(det_species$station_name)), selected = "All stations")
  })
  
  
  
  ####Detections over time####
  
  ## Detections vs date
  output$detections_time <- plotly::renderPlotly({
    
    if (is.null(reactive_station_sub()))
      return(NULL)
    
    subset_plot <- ggplot2::ggplot(reactive_station_sub(), ggplot2::aes(x=date, y=n.detections, color=receiver_name))+
      ggplot2::geom_point(alpha=.7, stroke=0, size=2)+
      ggplot2::scale_x_date(limits=c(min(reactive_station_sub()$date), max(reactive_station_sub()$date)),
                            date_labels = "%Y-%m-%d")+
      ggplot2::scale_color_manual(values = reactive_receiver_pal() , name="Receiver name")+
      ggplot2::ylab("Number of detections")+
      ggplot2::xlab("Date")+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text=ggplot2::element_text(size=14),
                     axis.title=ggplot2::element_text(size=16))
    
    plotly::ggplotly(subset_plot)%>%
      plotly::layout(legend=list(x=1.02, y=1))
    
  })
  
  shiny::observeEvent(input$refresh3,{
    shinyWidgets::updatePickerInput(session = session,inputId = "select_installation",
                                    choices = c(sort(unique(detections$installation_name))),
                                    selected = c(sort(unique(detections$installation_name)))[1],
                                    options = list('none-selected-text' = 'Please select installation')
    )
    shinyWidgets::updatePickerInput(session = session, inputId = "select_station",
                                    choices= c(sort(unique(detections$station_name))),
                                    selected = c(sort(unique(detections$station_name)))[1],
                                    options = list('none-selected-text' = 'Please select station')
    )
    shinyWidgets::updatePickerInput(session = session, inputId = "select_month",
                                    choices = c("All months","January", "February", "March", "April", "May",
                                                "June", "July", "August", "September", "October", "November",
                                                "December"),
                                    selected = "All months",
                                    options = list('none-selected-text' = 'Please select month')
    )
    shinyWidgets::updatePickerInput(session = session, inputId = "select_species",
                                    choices = c("All species", sort(unique(detections$species_common_name))),
                                    selected = "All species"
    )
    shinyWidgets::updatePickerInput(session = session, inputId = "select_timezone",
                                    choices = timezone_list,
                                    selected = "UTC"
    )
  })
  ## Detections vs hour
  
  output$detections_hour <- plotly::renderPlotly({
    if (is.null(reactive_hour_station()))
      return(NULL)
    
    bplot <- ggplot2::ggplot(reactive_hour_station(), ggplot2::aes(x=factor(hour), y=n.detections, alpha=0.7))+
      ggplot2::geom_boxplot(alpha=.7, lwd=.2, outlier.size=.2, fill = "orange2" )+
      # scale_fill_manual(values=c("orange2"))+
      ggplot2::xlab("Hour")+
      ggplot2::ylab("Number of detections")+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text=ggplot2::element_text(size=14),
                     axis.title=ggplot2::element_text(size=16))
    
    bplotly <- plotly::ggplotly(bplot)
    
    for(i in 1:length(bplotly$x$data)){
      bplotly$x$data[[i]]$marker$opacity = 0.2 
    }
    
    bplotly%>%
      plotly::layout(legend=list(x=1.02, y=1))
  })
  
  #### Station performance tab ####
  
  ## Table output
  
  output$receivers_station_table <- DT::renderDT({
    DT::datatable(reactive_station_table(),
                  extensions=c("Buttons","Scroller"), style="bootstrap", class="compact", width="100%",
                  options=list(dom = "lrtipB",deferRender=TRUE, scrollY=300, scroller=TRUE,
                               buttons=c('csv', 'excel', 'pdf'))
    )
  }, server = FALSE)
  
  ## Station efficiency index lollipop plot
  output$lollipop <- plotly::renderPlotly({
    if (is.null(reactive_sei()))
      return(NULL)
    lollipop <- ggplot2::ggplot(reactive_sei(), ggplot2::aes(x=rank, y=index)) +
      ggplot2::geom_segment(ggplot2::aes(x=rank, xend = rank, y=0, yend=index), color="grey") +
      ggplot2::geom_point( size=4, alpha=.7, stroke=0, ggplot2::aes(colour = index)) +
      ggplot2::scale_colour_gradient("Efficiency index:",low = "#5C038C", high="#76B947")+
      ggplot2::coord_flip()+
      ggplot2::theme_light() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
        
      ) +
      ggplot2::scale_x_continuous(
        breaks = reactive_sei()$rank, # specify tick breaks using rank column
        labels = reactive_sei()$station_name # specify tick labels using x column
      ) +
      ggplot2::theme_bw()+
      ggplot2::xlab("Station name") +
      ggplot2::ylab("Station efficiency index")
    
    lolliplotly <- plotly::ggplotly(lollipop, height = 800, width = 1000, tooltip = c("colour")) 
    lolliplotly
    
    
  })
  
  session$onSessionEnded(function() {
    shiny::stopApp()
  })
}

shiny::shinyApp(ui, server)
