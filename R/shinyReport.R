##' @title Render Shiny report
##' @description Renders the Shiny App for transmitter and receiver reports. 
##' @param type "transmitters" or "receivers" to produce the corresponding report
##' @return Shiny app with Transmitter or Receiver Project visualisations and statistics
##' @examples
##' \dontrun{
##' shinyReport(type = "transmitters")
##' }
##' @importFrom shiny runApp shinyOptions
##' @export
shinyReport <- function(type = "transmitters"){
  type <- match.arg(type, choices = c("transmitters", "receivers"))
  
  wd <- getwd()
  shinyOptions(wd = wd)
  
  if (type == "transmitters") {
    appFile <- system.file(file.path("shinyReport", "transmitter-app.R"), package = "remora")
    
  } else if (type == "receivers") {
    appFile <- system.file(file.path("shinyReport", "receiver-app.R"), package = "remora")

  } else {
    stop(paste("Report type", type ,"doesn't exist"))
    
  }
  
  runApp(appDir = appFile, launch.browser = TRUE, display.mode = "normal", quiet = TRUE)
}
