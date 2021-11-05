## define pipe
`%>%` <- dplyr::`%>%`
`%notin%` <- Negate(`%in%`)

#### Loading data ####

## First set wd to user wd so choose_file box opens to desirable path
oldwd <- getwd()
setwd(shiny::getShinyOption("wd"))

#Read the detections file downloaded from the web app
#The function generates a pop up window that allows the user to navigate through their folders to find the data
detections <- data.table::fread(choose_file(caption="Select detections data file"), header=TRUE)

## revert to oldwd so app runs properly
setwd(oldwd)

#Some of the detections in the web app have wrong longitude and latitude
#Correct data for wrong latitude (positive latitude) *This is assuming all data is in Australia
## IDJ - vectorised; this shouldn't be necessary if intent is to run report on QC'd data
detections$receiver_deployment_latitude <- ifelse(detections$receiver_deployment_latitude > 0, 
                                                  -1 * detections$receiver_deployment_latitude,
                                                  detections$receiver_deployment_latitude)


#The detections file include a timestamp column that includes the date and time
#Create a date column without time
detections$date <- as.Date(detections$detection_datetime)
detections$detection_datetime <- as.POSIXct(detections$detection_datetime, tz="UTC")

#Convert species common name to every word with capital letter
detections$species_common_name <- stringr::str_to_title(detections$species_common_name)

#Create a transmitter ID - species common name column
detections$tag_species <- paste0(detections$transmitter_id, " - ", detections$species_common_name)

#### Transmitter deployment location ####

#Create a palette that has levels for the different species in the detections data
species_colour <- leaflet::colorFactor(palette = "Paired", levels = levels(as.factor(detections$species_common_name)))

#Tag deployment location is calculated with a previous function det_per_tag_location()
tag_location <- det_per_tag_location(detections)

#Copy the tag location so that when transforming to a spatial dataframe the tag_location data frame doesn't change
## ODJ - not needed as you're creating a new object, `tag_location_sp`
##tag_location_copy <- copy(tag_location)

#Transform tag deployment location (copy) to a spatial data frame
## IDJ - use original tag_location object instead of a copy
tag_location_sp <- sf::st_as_sf(tag_location, 
                                coords = c("transmitter_deployment_longitude", 
                                           "transmitter_deployment_latitude"), 
                                crs = 4326)



####Horizontal bar plot for the number of total detections per transmitter####
#Calculate detections per transmitter with the function det_per_tag()
det_per_tag <- det_per_tag(detections)
#Create static palette fopr the horizontal bar plot 
det_species_pal <- leaflet::colorFactor(palette = "Paired", domain = levels(as.factor(det_per_tag$species_common_name)))

det_per_tag$species_colour <- det_species_pal(det_per_tag$species_common_name)

#Colour palette for the abacus plot and arch graph 
#See https://github.com/bhaskarvk/colormap for different colours
#Palette is created to have different levels according to the installation
installation_colour <- colormap::colormap(colormap = colormap::colormaps$hsv, 
                                          nshades = length(unique(detections$installation_name)), 
                                          reverse = TRUE)
installation_colour <- sample(installation_colour, length(installation_colour))




#Group by station name and transmitter id to be able to create the colour palette for the network map
det_per_station <- detections %>%
  dplyr::group_by(station_name, transmitter_id) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "keep" )

#Create bins for tracks colour. Bins are detections per station per transmitter
#Bins for number of detections per transmitter per station
bins <- c(1, 10, 100, 500, 1000, 5000, Inf)

#Colours for network leaflet map
#Palette for the bubbles -> more detections are the reddest bubbles
pal <- leaflet::colorBin("YlOrRd", domain = det_per_station$n, bins = bins)


#Crosstalk shared data for the network map
shared_detections <- crosstalk::SharedData$new(detections)


#### Start of Shiny App ####
#Note that the ui and server and contained in the same script

#The theme for the app is default from the bootstrap themes -> flatly
#The shiny app includes another styles (font and button format) in the style.css file
#The shiny app is divided by tabs (tab panels) which contain different visualisations and statistics
#The structure is: Tag location map | Tag detections | Abacus plot and arch graph | Network map




ui <- shiny::bootstrapPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  shiny::navbarPage( windowTitle = "remora Transmitter Report", #Appears in the tab of the browser
                     
                     theme = shinythemes::shinytheme("flatly"), 
                     collapsible = TRUE,
                     #R package hex logo appears on the nav bar
                     id="nav", title = htmltools::div(htmltools::img(src='white_remora.png',
                                                             height='60', 
                                                             width='183', 
                                                             style="vertical-align:middle"
                     ), 
                     style="margin-top:-16px; margin-right:-40px; margin-left:-40px"
                     ),
                     #Transmitter deployment location tab
                     shiny::tabPanel("Transmitter deployments",
                                     htmltools::div(class = "outer",
                                                    tags$head(htmltools::includeCSS("styles.css")),
                                                    
                                                    leaflet::leafletOutput("mymap", width = "100%", height = "100%"),
                                                    
                                                    #Draggable left panel
                                                    shiny::absolutePanel(id = "controls", 
                                                                         class = "panel-default", 
                                                                         top = 75, 
                                                                         left = 70, 
                                                                         width = 250, 
                                                                         fixed=TRUE,
                                                                         draggable = TRUE, 
                                                                         height = "auto",
                                                                         
                                                                         htmltools::h3(shiny::textOutput("tags_count"), align = "right"), #Reactive text that changes as user interacts with map
                                                                         
                                                                         htmltools::h4(shiny::textOutput("species_count"), align = "right"), #Reactive text that changes as user interacts with map
                                                                         
                                                                         #Slider for the transmitter deployment datetimes
                                                                         shiny::sliderInput("date", 
                                                                                            htmltools::h4("Transmitter deployment date"),
                                                                                            ticks = FALSE, 
                                                                                            min = min(detections$transmitter_deployment_datetime),
                                                                                            max = max(detections$transmitter_deployment_datetime),
                                                                                            value = range(min(detections$transmitter_deployment_datetime),
                                                                                                          max(detections$transmitter_deployment_datetime)),
                                                                                            step = 100),
                                                                         #Filter by species
                                                                         shinyWidgets::pickerInput("species_select", 
                                                                                                   htmltools::h5("Select species:"), 
                                                                                                   choices = c("All species", sort(unique(detections$species_common_name))),
                                                                                                   multiple = TRUE, selected = "All species"),
                                                                         shiny::actionButton("refresh", "Reset filters", icon=shiny::icon("redo-alt"))
                                                                         
                                                    ),
                                                    #IMOS logo that appears at the bottom left of the page
                                                    shiny::absolutePanel(id = "logo", 
                                                                         class = "card", 
                                                                         bottom = 60, 
                                                                         left = 60, 
                                                                         width = 80, 
                                                                         fixed=TRUE, 
                                                                         draggable = FALSE, 
                                                                         height = "auto",
                                                                         tags$a(href = 'https://animaltracking.aodn.org.au/', 
                                                                                tags$img(src = 'imos-logo.png', height = '100', width = '200')))
                                                    
                                     )
                     ),
                     #Transmitter detections horizontal bar plot tab
                     shiny::tabPanel("Transmitter detections",
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shinyWidgets::pickerInput(inputId = "select_species",
                                                                   label = htmltools::h5("Select species"),
                                                                   choices = c("All species",sort(unique(detections$species_common_name))),
                                                                   multiple = TRUE,
                                                                   selected = "All species",
                                                                   options = list('none-selected-text' = 'Please select species')
                                         ),
                                         shinyWidgets::pickerInput(inputId = "select_transmitter_id",
                                                                   label = htmltools::h5("Select transmitter"),
                                                                   choices = c("All transmitters", sort(unique(detections$transmitter_id))),
                                                                   multiple = TRUE,
                                                                   selected = "All transmitters",
                                                                   options = list('none-selected-text' = 'Please select transmitter')
                                         ),
                                         shiny::actionButton("refresh2", "Reset filters", icon=shiny::icon("redo-alt")),
                                         htmltools::br(),
                                         htmltools::br(),
                                         htmltools::p("Summary table: detections per transmitter ID"),
                                         DT::DTOutput("tag_det_table")
                                       ),
                                       
                                       shiny::mainPanel (
                                         htmltools::h3("Number of detections recorded for the selected transmitter(s)"), 
                                         #Horizontal bar plot of the detections
                                         plotly::plotlyOutput("detections_transmitter", width = "100%", height = "110%") %>% 
                                           shinycssloaders::withSpinner(color ="#3b6e8f", type = 7),
                                         htmltools::br(),
                                         htmltools::br(),
                                         htmltools::p(paste("\U2022","Hover on the plot to see transmitter ID, species common name and number of detections.")),
                                         htmltools::br(),
                                         htmltools::p(paste("\U2022","Save the plot by clicking the camera icon in the top right corner.")),
                                         htmltools::br(),
                                         htmltools::p(paste("\U2022","Download the summary table in .CSV, Excel or .PDF format"))
                                         
                                       ))),
                     #Abacus plot and arch graph tab
                     shiny::tabPanel("Detections over time",
                                     
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(width = 2,
                                                           #Select by transmitter
                                                           shinyWidgets::pickerInput(inputId = "select_tag",
                                                                                     label = h5("Select transmitter"),
                                                                                     choices = c("All transmitters",sort(unique(detections$transmitter_id))),
                                                                                     multiple = TRUE,
                                                                                     selected = "All transmitters"),
                                                           #Select by date (slider)
                                                           shiny::sliderInput("select_date", htmltools::h5("Transmitter detetection date"),
                                                                              ticks = FALSE, min = min(detections$date),
                                                                              max = max(detections$date),
                                                                              value = range(detections$date) , step = 10),
                                                           shiny::actionButton("refresh3", "Reset filters", icon=shiny::icon("redo-alt"))
                                       ),
                                       
                                       shiny::mainPanel (
                                         shiny::tabsetPanel(type = "tabs",
                                                            shiny::tabPanel("Detections over time",
                                                                            htmltools::h3("Detections over time recorded for the selected transmitter(s):"),
                                                                            #Text that appears when selected date is out of range for the transmitter
                                                                            shiny::textOutput("selected_date")%>% 
                                                                              htmltools::tagAppendAttributes(style= 'font-size:20px; color:red;'),
                                                                            #Abacus plot
                                                                            plotly::plotlyOutput("detections_day", width = "120%", height = "120%") %>% 
                                                                              shinycssloaders::withSpinner(color ="#3b6e8f", type = 7),
                                                                            htmltools::br(),
                                                                            htmltools::br(),
                                                                            htmltools::p(paste("\U2022","Hover on the plot to see station name, date, number of detections, species and transmitter ID.")),
                                                                            htmltools::br(),
                                                                            htmltools::p(paste("\U2022","Save the plot by clicking the camera icon in the top right corner."))
                                                            ),
                                                            shiny::tabPanel("Connectivity plot",
                                                                            htmltools::h3("Connectivity between stations visited by the selected transmitter(s)"),
                                                                            #Text that appears when selected date is out of range for the transmitter
                                                                            shiny::textOutput("selected_date2")%>% 
                                                                              htmltools::tagAppendAttributes(style= 'font-size:20px; color:red;'),
                                                                            #Download button for the arch graph
                                                                            htmltools::div(shiny::downloadButton("download_graph", "Download plot")),#, style="float:right"),
                                                                            #Arch graph
                                                                            shiny::plotOutput("arch_graph", width = "110%", height = "500px") %>% 
                                                                              shinycssloaders::withSpinner(color ="#3b6e8f", type = 7),
                                                                            htmltools::p(style="text-align: justify;",
                                                                                         "This plot presents the movement connections between receiver stations and installations visited by the selected transmitter(s). 
                                                                            Colours identify separate installations and line thickness highlights the importance of these movement paths within the dataset."),
                                                                            htmltools::p(paste("\U2022","Save the plot by clicking the 'Download plot' button."))
                                                                            
                                                            )
                                         )
                                         
                                         
                                       ))),
                     
                     #Network leaflet map
                     shiny::tabPanel("Detections network map",
                                     htmltools::div(class = "outer",
                                                    tags$head(htmltools::includeCSS("styles.css")),
                                                    
                                                    leaflet::leafletOutput("tracks_map", width = "100%", height = "100%"), #leaflet map output
                                                    
                                                    shiny::absolutePanel(id = "controls", 
                                                                         class = "panel-default",
                                                                         top = 75, 
                                                                         left = 70, 
                                                                         width = 300, 
                                                                         fixed = TRUE,
                                                                         draggable = TRUE, 
                                                                         height = "auto",
                                                                         htmltools::h2(textOutput("species"), align = "right"), #Reactive text, changes when user interacts with map
                                                                         htmltools::h3(textOutput("detections_total"), align = "right"), #Reactive text, changes when user interacts with map
                                                                         htmltools::h4("Transmitter detected in:", align = "right"), #Reactive text, changes when user interacts with map
                                                                         htmltools::h4(textOutput("stations_total"), align = "right"), #Reactive text, changes when user interacts with map
                                                                         htmltools::h4(textOutput("installations_total"), align = "right"), #Reactive text, changes when user interacts with map
                                                                         #Select transmitter
                                                                         shinyWidgets::pickerInput(inputId = "select_transmitter",
                                                                                                   label = htmltools::h5("Select transmitter"),
                                                                                                   choices = c(sort(unique(as.character(detections$tag_species)))),
                                                                                                   selected = NULL,
                                                                                                   multiple = TRUE,
                                                                                                   options = list('none-selected-text' = 'Please select transmitter', 'max-options' = 1)
                                                                                                   ),
                                                                         #tableOutput("view")
                                                                         #DTOutput("view")
                                                                         
                                                                         
                                                    ),
                                                    #IMOS logo that appears at the bottom left of the page
                                                    shiny::absolutePanel(id = "logo", 
                                                                         class = "card", 
                                                                         bottom = 60, 
                                                                         left = 60, 
                                                                         width = 80, 
                                                                         fixed = TRUE, 
                                                                         draggable = FALSE, 
                                                                         height = "auto",
                                                                         tags$a(href = 'https://animaltracking.aodn.org.au/', 
                                                                                tags$img(src = 'imos-logo.png', height = '100', width = '200')))
                                                    
                                     )
                     )))




#Server for the shiny app: contains all the outputs and the reactive data
server <- function (input, output, session) {
  
  ####Reactive data (changes when user selects inputs)####
  
  reactive_tag_location <- shiny::reactive({
    req(input$species_select)
    tag_location <- tag_location[tag_location$transmitter_deployment_datetime >= input$date[1] & 
                                   tag_location$transmitter_deployment_datetime <= input$date[2], ]
    if("All species" %in% input$species_select){
      return(tag_location)
    }else{
      tag_location <- tag_location %>%
        dplyr::filter(species_common_name %in% input$species_select)
      return(tag_location)
    }
  })
  
  reactive_tag_location_sp <- shiny::reactive({
    req(input$species_select)
    tag_location_sp <- tag_location_sp[tag_location_sp$transmitter_deployment_datetime >= input$date[1] & 
                                         tag_location_sp$transmitter_deployment_datetime <= input$date[2], ]
    if("All species" %in% input$species_select){
      return(tag_location_sp)
    }else{
      tag_location_sp <- tag_location_sp %>%
        dplyr::filter(species_common_name %in% input$species_select)
      return(tag_location_sp)
    }
  })
  
  #Reactive data for Transmitter detections bar plot
  reactive_tag <- shiny::reactive({
    req(input$select_transmitter_id, input$select_species)
    if( "All transmitters" %in% input$select_transmitter_id &&  "All species" %in% input$select_species){
      return(det_per_tag)
    }else if (input$select_transmitter_id != "" && "All species" %in% input$select_species){
      det_per_tag <- det_per_tag %>%
        dplyr::filter(transmitter_id %in% input$select_transmitter_id)
      return(det_per_tag)
    }else if ("All transmitters" %in% input$select_transmitter_id && input$select_species != ""){
      det_per_tag <- det_per_tag %>%
        dplyr::filter(species_common_name %in% input$select_species)
      return(det_per_tag)
    }else if (input$select_transmitter_id != "" && input$select_species != ""){
      det_per_tag <- det_per_tag %>%
        dplyr::filter(species_common_name %in% input$select_species) %>%
        dplyr::filter(transmitter_id %in% input$select_transmitter_id)
      return(det_per_tag)
    }else{
      return(NULL)
    }
    
  })
  
  
  #Reactive data for abacus plot and graph (changes when selecting a transmitter and/or date)
  reactive_detections <- shiny::reactive({
    req(input$select_tag, input$select_date)
    detections <- detections[detections$date >= input$select_date[1] & 
                               detections$date <= input$select_date[2], ]
    detections <- detections[order(detections$detection_datetime),]
    
    if ("All transmitters" %in% input$select_tag){
      return(detections)
    }else if (input$select_tag != "" && input$select_tag != "All transmitters"){
      detections <- detections %>%
        dplyr::filter(transmitter_id %in% input$select_tag)
      
      return(detections)
    }else{
      return(NULL)
    }
  })
  
  
  # Groupby of the detections per station and day to generate the nodes for the network
  detections_subset <- shiny::reactive({
    req(input$select_tag, input$select_date)
    
    detections <- detections[detections$date >= input$select_date[1] & 
                               detections$date <= input$select_date[2], ]
    detections <- detections[order(detections$detection_datetime),]
    
    if ("All transmitters" %in% input$select_tag){
      detections <- detections %>%
        dplyr::group_by(station_name, date) %>%
        dplyr::summarise(n.detections = dplyr::n(), installation_name = installation_name[1],
                         species_common_name = species_common_name[1], transmitter_id = transmitter_id[1],
                         .groups = "drop")
      return(detections)
    }else if (input$select_tag != "" && input$select_tag != "All transmitters"){
      detections <- detections %>%
        dplyr::filter(transmitter_id %in% input$select_tag)
      detections <- detections %>%
        dplyr::group_by(station_name, date) %>%
        dplyr::summarise(n.detections = dplyr::n(), installation_name = installation_name[1],
                         species_common_name = species_common_name[1], transmitter_id = transmitter_id[1],
                         .groups = "drop")
      return(detections)
    }else{
      return(NULL)
    }
    
  })
  
  station_subset <- shiny::reactive({
    reactive_detections() %>%
      dplyr::group_by(station_name) %>%
      dplyr::summarise(n = dplyr::n())
  })
  
  #Creating from and to reactive nodes (stations)
  df <- shiny::reactive({
    #Condition for when there is just one detection
    if(nrow(detections_subset()) == 1){
      dat <- data.frame(from = reactive_detections()$station_name, to = reactive_detections()$station_name)
    } else {
      rows <- lapply(seq(nrow(reactive_detections())), function(i) {
        if (i != nrow(reactive_detections())){ #Condition to avoid generating a null node for the last station visited
          data.frame(from = reactive_detections()$station_name[i], to = reactive_detections()$station_name[i+1])
        }})
      dat <- dplyr::bind_rows(rows)}
    if(length(unique(detections_subset()$station_name))>1){
      dat <- dat %>%
        dplyr::filter(from != to)
    }
    
    return(dat)
  })
  
  #Reactive metadata of the nodes (stations)
  meta <- shiny::reactive({
    df2 <- c(as.character(df()$from), as.character(df()$to)) %>%
      tibble::as_tibble() %>%
      dplyr::group_by(value) %>%
      dplyr::summarise(station_name = value, .groups="drop")
    
    df2 <- dplyr::left_join(df2,
                            reactive_detections()[,c('station_name','receiver_deployment_longitude',
                                                     'receiver_deployment_latitude', 'installation_name')],
                            by = 'station_name')
    df2 <- dplyr::left_join(df2, station_subset(), by='station_name')
    
    df2 <- dplyr::distinct(df2, station_name, .keep_all = TRUE)
    
    #Condition for when there is just one detection
    if(nrow(detections_subset()) == 1){
      df2$n <- df2$n - 1
    }
    
    return(df2)
  })
  
  #Final graph 
  mygraph <- shiny::reactive({
    igraph::graph_from_data_frame(df(), vertices = meta(), directed = FALSE )
  })
  
  #Reactive palette for abacus plot and arch graph. Create colour per installation
  
  reactive_installation_pal <- reactive ({
    #shiny::req()
    
    colourCount <- length(unique(detections_subset()$installation_name))
    getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))
    return(getPalette(colourCount))
    
  })
  
  ####Reactive data for tracks map####
  reactive_tracks <- shiny::reactive({
    detections <- detections[detections$tag_species == input$select_transmitter, ]
    detections[order(detections$detection_datetime),]
  })
  
  det_station_tracks <- shiny::reactive({
    reactive_tracks() %>%
      dplyr::group_by(station_name) %>%
      dplyr::summarise(n = dplyr::n())
  })
  
  df_tracks <- shiny::reactive({
    #Condition for when there is just one detection
    if(nrow(reactive_tracks()) == 1){
      dat <- data.frame(from = reactive_tracks()$station_name, to = reactive_tracks()$station_name)
    } else {
      rows <- lapply(seq(nrow(reactive_tracks())), function(i) {
        if (i != nrow(reactive_tracks())){
          data.frame(from = reactive_tracks()$station_name[i], to = reactive_tracks()$station_name[i+1])
        }})
      dat <- dplyr::bind_rows(rows)}
    dat <- dat %>%
      dplyr::filter(from != to)
    return(dat)
  })
  
  
  meta_tracks <- shiny::reactive({
    df2<-c(as.character(df_tracks()$from), as.character(df_tracks()$to)) %>%
      tibble::as_tibble() %>%
      dplyr::group_by(value) %>%
      dplyr::summarise(station_name = value, .groups="drop")
    df2 <- dplyr::left_join(df2,
                            reactive_tracks()[,c('station_name','receiver_deployment_longitude',
                                                 'receiver_deployment_latitude','installation_name')],
                            by = 'station_name')
    df2 <- dplyr::left_join(df2, det_station_tracks(), by = 'station_name')
    
    df2 <- dplyr::distinct(df2, station_name, .keep_all = TRUE)
    
    
    
    #Condition for when there is just one detection
    if (nrow(reactive_tracks()) == 1){
      df2$n <- df2$n - 1
    }
    
    return(df2)
  })
  
  mygraph_tracks <- shiny::reactive({
    igraph::graph_from_data_frame(df_tracks(), vertices = meta_tracks(), directed = FALSE )
  })
  
  #Creating bubbles and lines for the leaflet map
  #Code adapted from https://rpubs.com/martinjhnhadley/geographic_network
  #Create spatial points for the nodes (vert) and spatial lines for the edges (edges)
  gg <- shiny::reactive({
    igraph::get.data.frame(mygraph_tracks(), "both")
  })
  
  vert <- shiny::reactive({
    vert <- gg()$vertices
    sp::coordinates(vert) <- ~ receiver_deployment_longitude + receiver_deployment_latitude
    vert <- vert[order(vert$n), ]
    return(vert)
  })
  
  edges <- shiny::reactive({
    edges <- gg()$edges
    edges <- lapply(1:nrow(edges), function(i) {
      as(rbind(vert()[vert()$station_name == edges[i, "from"], ],
               vert()[vert()$station_name == edges[i, "to"], ]),
         "SpatialLines")
    })
    for (i in seq_along(edges)) {
      edges[[i]] <- sp::spChFIDs(edges[[i]], as.character(i))
    }
    
    edges <- do.call(rbind, edges)
    return(edges)
  })
  
  #Pop up text when hovering on the bubbles
  mytext_tracks <- shiny::reactive(paste(
    "Detections: ", vert()$n, "<br/>",
    "Longitude: ", vert()$receiver_deployment_longitude, "<br/>",
    "Latitude: ", vert()$receiver_deployment_latitude, "<br/>",
    "Station name: ", vert()$station_name, "<br/>",
    "Installation name: ", vert()$installation_name, "<br/>",
    "Transmitter ID - Species: ", unique(reactive_tracks()$tag_species)) %>%
      lapply(htmltools::HTML))
  
  #Start of the outputs for each tab for the shiny app, every tab is a section on the code
  
  
  ####Transmitter location map tab####
  #Count transmitters
  output$tags_count <- shiny::renderText({
    paste0(prettyNum(n_tags(reactive_tag_location()), big.mark = ","), " transmitters") 
  })
  
  #Count species
  output$species_count <- shiny::renderText({
    paste0(prettyNum(length(unique(reactive_tag_location()$species_common_name)), big.mark = ","), " species") 
  })
  
  #Reset the filters
  shiny::observeEvent(input$refresh,{
    shinyWidgets::updatePickerInput(session = session, inputId = "species_select",
                                    choices = c("All species", unique(detections$species_common_name)), selected = "All species")
    shiny::updateSliderInput(session = session, inputId = "date", min = min(detections$transmitter_deployment_datetime),
                             max = max(detections$transmitter_deployment_datetime),
                             value = range(min(detections$transmitter_deployment_datetime),
                                           max(detections$transmitter_deployment_datetime)),
                             step = 100)
  })
  
  #Reactive text when hovering on tag location map
  mytext <- shiny::reactive(paste(
    "Transmitter ID: ", reactive_tag_location()$transmitter_id, "<br/>",
    "Species: ", reactive_tag_location()$species_common_name, "<br/>",
    "Longitude: ", reactive_tag_location()$transmitter_deployment_longitude, "<br/>",
    "Latitude: ", reactive_tag_location()$transmitter_deployment_latitude, sep = "")%>%
      lapply(htmltools::HTML))
  
  
  #Leaflet basemap for all the maps
  basemap <- shiny::reactive({ leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite") %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap, group = "Bathymetry") %>%
      leaflet::addScaleBar(position = 'topright') %>%
      leaflet::addLayersControl(
        baseGroups = c("Satellite", "Bathymetry"),  # specify the desired basemap options for the output map
        options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
      leaflet::setView(lng = 135, lat = -27, zoom = 5)
    
  })
  
  #Render basemap
  output$mymap <- leaflet::renderLeaflet({
    basemap()
  })
  
  #Render tag locations
  shiny::observeEvent(reactive_tag_location_sp(), {
    leaflet::leafletProxy("mymap", data=reactive_tag_location_sp()) %>% 
      leaflet::clearMarkers() %>%
      leaflet::clearShapes() %>%
      leaflet::clearControls() %>%
      leaflet::addMeasure(
        primaryLengthUnit = "kilometers",
        secondaryLengthUnit = "miles",
        primaryAreaUnit = "hectares",
        secondaryAreaUnit = "acres",
        position = 'topleft') %>%
      leaflet::addCircleMarkers(
        #fillColor= ifelse(length(unique(reactive_tag_location_sp()$species_common_name))>1, ~species_common_name, "navy"),
        fillColor = ~ species_colour(species_common_name),
        fillOpacity = 0.7,
        radius = 6,
        label = mytext(),
        stroke = FALSE
      )%>%
      leaflet::addLegend(pal = species_colour, 
                         values = reactive_tag_location()$species_common_name, opacity = 0.7, title = "Species detected",
                         position = "topright")
    
  })
  
  
  
  ####Transmitter detections####
  
  #Render horizontal bar plot
  output$detections_transmitter <- plotly::renderPlotly({
    if(is.null(reactive_tag)){
      return(NULL)
    }
    
    plotly::plot_ly(reactive_tag(),
                    y = ~transmitter_id,
                    x = ~n.detections,
                    text = ~paste('</br>Transmitter:', transmitter_id,
                                  '</br>Species:', species_common_name, 
                                  '</br>Detections:', n.detections),
                    textposition = 'none',
                    hoverinfo = 'text',
                    type = 'bar',
                    orientation='h',
                    marker = list(color = ~species_colour)) %>% #text when hovering
      plotly::layout(xaxis = list(title = 'Number of detections',
                                  tickfont=list(size=14),
                                  titlefont=list(size=18)),
                     yaxis = list(title='Transmitter ID',
                                  showticklabels=FALSE,
                                  titlefont=list(size=18)),
                     bargap=30)
    
  })
  
  #Render table 
  output$tag_det_table <- DT::renderDT({
    DT::datatable(reactive_tag(), 
                  extensions = c("Buttons","Scroller"), style = "bootstrap", class="compact", width="100%",
                  options = list(dom = "lrtipB",deferRender=TRUE, scrollY=300, scroller=TRUE,
                                 buttons=c('csv', 'excel', 'pdf'), 
                                 columnDefs = list(list(visible=FALSE, targets=4))
                  )) 
  }, server = FALSE)
  
  #Reset plot
  shiny::observeEvent(input$refresh2,{
    shinyWidgets::updatePickerInput(session = session, inputId = "select_transmitter_id",
                                    choices = c("All transmitters", unique(detections$transmitter_id)), selected = "All transmitters")
    shinyWidgets::updatePickerInput(session = session, inputId = "select_species",
                                    choices = c("All species", unique(detections$species_common_name)), selected = "All species")
  })
  
  #Narrow down filters
  
  shiny::observeEvent (input$select_species,{
    shiny::req(input$select_species)
    if ("All species" %notin% input$select_species){
    subset <- detections %>% 
      dplyr::filter(species_common_name %in% input$select_species)
    
    shinyWidgets::updatePickerInput(session = session, inputId = "select_transmitter_id",
                                    choices = sort(unique(subset$transmitter_id)),
                                    options = list('none-selected-text' = 'Please select transmitter'))
    }
  }, ignoreInit = TRUE)
  
  ####Detections over time####
  
  
  
  output$detections_day <- plotly::renderPlotly({
    if ((nrow(detections_subset()) == 0)){
      output$selected_date <- shiny::renderText({ #Render text when selecting a date out of range
        "You have selected a date out of range"
      })
      return(NULL)
    }
    
    ####Abacus plot of the number of detections per transmitter colour coded by station####
    
    #Reset the filters
    shiny::observeEvent(input$refresh3,{
      shinyWidgets::updatePickerInput(session = session, inputId = "select_tag",
                                      choices = c("All transmitters", unique(detections$transmitter_id)), selected = "All transmitters")
      shiny::updateSliderInput(session = session, inputId = "select_date", min = min(detections$date),
                               max = max(detections$date),
                               value = range(detections$date) , step = 10)
    })
    
    
    output$selected_date <- shiny::renderText("")
    p <- ggplot2::ggplot(detections_subset(), mapping = ggplot2::aes(x=date, y=station_name, color=installation_name,
                                                                     text= paste("detections:", n.detections, "</br>",
                                                                                 "species:", species_common_name, "</br>",
                                                                                 "transmitter_id:", transmitter_id)%>%
                                                                       lapply(htmltools::HTML))) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Station name") +
      ggplot2::geom_point(size=4, alpha=0.7, stroke=0.1) +
      ggplot2::scale_color_manual(values = reactive_installation_pal(), name = "Installation name")+
      ggplot2::scale_x_date(limits=c(min(detections_subset()$date), max(detections_subset()$date)),
                            date_labels = "%Y-%m-%d")+
      ggplot2::theme_bw()+
      ggplot2::theme(legend.position="none",
                     axis.text.y = ggplot2::element_blank(),
                     axis.text = ggplot2::element_text(size=14),
                     axis.title = ggplot2::element_text(size=16))
    
    
    #Plotly to add interactivity
    
    plotly::ggplotly(p, tooltip=c("x","y","installation_name","text"))%>%
      plotly::layout(xaxis= list(
        tickformat = "%d %m %y")
      ) %>%
      plotly::highlight(on = "plotly_click", off='plotly_doubleclick')
    
  })
  
  
  #### Arch graph ####
  arch <- shiny::reactiveValues()
  
  output$arch_graph <- shiny::renderPlot({
    if ((nrow(detections_subset())==0)){
      output$selected_date2 <- shiny::renderText({ 
        "You have selected a date out of range"
      })
      return(NULL)
    }
    par(mar = c(0,0,0,0))
    output$selected_date2 <- shiny::renderText("")
    arch_graph <- ggraph::ggraph(mygraph(), layout="linear") +
      ggraph::geom_edge_arc(edge_colour="black", edge_alpha=0.3, edge_width=0.5, fold=TRUE) +
      ggraph::geom_node_point(ggplot2::aes(size=n, color=installation_name, fill=installation_name), alpha=0.7) +
      ggplot2::scale_size_continuous(range=c(5,20)) +
      ggplot2::scale_color_manual(values = reactive_installation_pal()) +
      ggraph::geom_node_text(ggplot2::aes(label=stringr::str_trunc(station_name, 10)), angle=65  , hjust=1, nudge_y = -0.3, size=6) +
      ggplot2::scale_x_discrete(label=function(x) abbreviate(x, minlength=7))+
      ggraph::theme_graph() +
      ggplot2::theme(
        legend.position="none",
        plot.margin=grid::unit(c(0,0,0.2,0), "null")#,
        #panel.spacing=grid::unit(c(0,0,3.4,0), "null")
      ) +
      ggplot2::expand_limits(x = c(-1, 1.5), y = c(-5, 4))
    arch$plot<-arch_graph
    arch_graph
  })
  
  output$download_graph <- shiny::downloadHandler(
    filename = function(){paste(input$select_transmitter,'arch-graph' ,'.pdf', sep = '')},
    
    content = function(file){
      grDevices::pdf(file, width = 10, height = 5)
      print(arch$plot)
      grDevices::dev.off()
    })
  
  
  
  # output$view <- renderTable({ detections_subset() })
  
  
  ####Network map tab####
  #Species
  output$species <- shiny::renderText({
    unique(reactive_tracks()$species_common_name) 
  })
  #Count detections
  output$detections_total <- shiny::renderText({
    paste0(prettyNum(nrow(reactive_tracks()), big.mark=","), " detections") 
  })
  #Count stations
  output$stations_total <- shiny::renderText({
    paste0(prettyNum(length(unique(reactive_tracks()$station_name)), big.mark=","), " stations") 
  })
  #Count installations
  output$installations_total <- shiny::renderText({
    paste0(prettyNum(length(unique(reactive_tracks()$installation_name)), big.mark=","), " installations") 
  })
  
  #Render basemap
  output$tracks_map<- leaflet::renderLeaflet({
    basemap() 
  })
  
  #Render network map
  shiny::observeEvent(c(reactive_tracks(), input$select_transmitter), {
    
    leaflet::leafletProxy("tracks_map") %>%
      leaflet::clearMarkers() %>%
      leaflet::clearShapes() %>%
      leaflet::clearControls()%>%
      leaflet::addMeasure(
        primaryLengthUnit = "kilometers",
        secondaryLengthUnit = "miles",
        primaryAreaUnit = "hectares",
        secondaryAreaUnit = "acres",
        position = 'topleft')%>%
      leaflet::addPolylines(data=edges(), weight=3, color = "white")%>%
      leaflet::addCircleMarkers(
        data=vert(),
        fillColor = ~ pal(n),
        fillOpacity = 1,
        radius= 10,
        label= mytext_tracks(),
        stroke=FALSE) %>%
      leaflet::addLegend(pal = pal, values = vert()$n, opacity = 0.7, title = "Number of detections",
                         position = "topright") %>%
      leaflet::flyToBounds(lng1 = min(reactive_tracks()$receiver_deployment_longitude, na.rm = TRUE),
                           lng2 = max(reactive_tracks()$receiver_deployment_longitude, na.rm = TRUE),
                           lat1 = min(reactive_tracks()$receiver_deployment_latitude, na.rm = TRUE),
                           lat2 = max(reactive_tracks()$receiver_deployment_latitude, na.rm = TRUE))
    
    
  }, ignoreInit = TRUE)
  
  session$onSessionEnded(function() {
    shiny::stopApp()
  })
}

shiny::shinyApp(ui, server)
