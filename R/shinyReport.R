##' @title Render Shiny report
##' @description Renders the Shiny App for transmitter and receiver reports. 
##' @param type "transmitter" or "receiver" to produce the corresponding report
##' @return Shiny app with Transmitter or Receiver Project visualisations and statistics
##' @examples
##' shinyReport(type = "transmitter")
##' @importFrom shiny runApp shinyOptions
##' @export
shinyReport <- function(type = "transmitter"){
  type <- match.arg(type, choices = c("transmitter", "receiver"))
  
  wd <- getwd()
  shinyOptions(wd = wd)
  
  if (type == "transmitter") {
    appFile <- system.file(file.path("shinyReport", "transmitter-app.R"), package = "remora")
    
  } else if (type == "receiver") {
    appFile <- system.file(file.path("shinyReport", "receiver-app.R"), package = "remora")

  } else {
    stop(paste("Report type", type ,"doesn't exist"))
    
  }
  
  runApp(appDir = appFile, launch.browser = TRUE, display.mode = "normal", quiet = TRUE)
}
