#' `fxn_valueBoxLayout.R` - Build value box layout for display
#' 
#' @param inData - AZMet 15-minute data for user-selected station from `fxn_az15min.R`
#' @return `valueBoxLayout` - Value box layout for display


fxn_valueBoxLayout <- function(inData) {
  
  vb_T <- 
    bslib::value_box(
      class = "border-0 shadow-none",
      fill = TRUE,
      full_screen = FALSE,
      height = NULL,
      id = NULL,
      max_height = NULL,
      min_height = NULL,
      theme = NULL,
      
      title = "Air Temperature",
      value = 
        paste0(
          dplyr::filter(
            inData, 
            lubridate::as_datetime(datetime) == max(lubridate::as_datetime(datetime))
          ) %>% 
            dplyr::pull(temp_airF),
          " °F"
        ),
      # htmltools::p("Heat index: NA °F"), # Make into conditional
      # htmltools::p("Wind chill: NA °F"), # Make into conditional
      htmltools::p(class = "value-box-text", "Maximum: 112.5 °F"),
      htmltools::p(class = "value-box-text", "Minimum: 89.1 °F"),
      showcase = shiny::plotOutput("vbChart_T"),
      showcase_layout = bslib::showcase_left_center(width = 0.45)
    )
  
  vb_RH <- 
    bslib::value_box(
      class = "border-0 shadow-none",
      
      title = "Relative Humidity",
      value = "33 %",
      htmltools::p(class = "value-box-text", "Dew point: 49.8 °F"),
      showcase = shiny::plotOutput("vbChart_RH"),
      showcase_layout = bslib::showcase_left_center(width = 0.45)
    )
  
  vb_P <- 
    bslib::value_box(
      class = "border-0 shadow-none",
      
      title = "Precipitation",
      value = "0.00 inches",
      htmltools::p(class = "value-box-text", "Since midnight"),
      showcase = shiny::plotOutput("vbChart_P"),
      showcase_layout = bslib::showcase_left_center(width = 0.45)
    )
  
  vb_Tsoil10cm <- 
    bslib::value_box(
      class = "border-0 shadow-none",
      
      title = "Soil Temperature",
      value = "90.4 °F",
      htmltools::p(class = "value-box-text", "4-inch depth"),
      showcase = shiny::plotOutput("vbChart_Tsoil10cm"),
      showcase_layout = bslib::showcase_left_center(width = 0.45)
    )
  
  vb_Tsoil50cm <-
    bslib::value_box(
      class = "border-0 shadow-none",
      
      title = "Soil Temperature",
      value = "82.7 °F",
      htmltools::p(class = "value-box-text", "20-inch depth"),
      showcase = shiny::plotOutput("vbChart_Tsoil50cm"),
      showcase_layout = bslib::showcase_left_center(width = 0.45)
    )
  
  vb_SR <- 
    bslib::value_box(
      class = "border-0 shadow-none",
      
      title = "Solar Radiation",
      value = "560.34 W/m\u00B2",
      showcase = shiny::plotOutput("vbChart_SR"),
      showcase_layout = bslib::showcase_left_center(width = 0.45)
    )
  
  vb_WS <- 
    bslib::value_box(
      class = "border-0 shadow-none",
      
      title = "Wind speed",
      value = "10.5 mph",
      htmltools::p(class = "value-box-text", "Direction: E"),
      htmltools::p(class = "value-box-text", "Maximum: 33.7 mph"),
      showcase = shiny::plotOutput("vbChart_WS"),
      showcase_layout = bslib::showcase_left_center(width = 0.45)
    )
  
  vb_WS2min <- 
    bslib::value_box(
      class = "border-0 shadow-none",
      
      title = "Wind speed 2-min",
      value = "7.4 mph",
      htmltools::p(class = "value-box-text", "Direction: SE"),
      htmltools::p(class = "value-box-text", "Maximum: 24.9 mph"),
      showcase = shiny::plotOutput("vbChart_WS2min"),
      showcase_layout = bslib::showcase_left_center(width = 0.45)
    )
  
  valueBoxLayout <- 
    list(
      vb_T,
      vb_RH,
      vb_P,
      vb_Tsoil10cm,
      vb_Tsoil50cm,
      vb_SR,
      vb_WS,
      vb_WS2min
    )
  
  return(valueBoxLayout)
}
