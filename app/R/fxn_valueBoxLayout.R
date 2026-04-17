fxn_valueBoxLayout <- function(inData) {
  
  vb_T <- 
    value_box(
      class = "border-0 shadow-none",
      fill = TRUE,
      full_screen = FALSE,
      height = NULL,
      id = NULL,
      max_height = NULL,
      min_height = NULL,
      theme = NULL,
      
      title = "Air Temperature",
      # value = "99.0 °F",
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
      htmltools::p("Maximum: 112.5 °F"),
      htmltools::p("Minimum: 89.1 °F"),
      showcase = shiny::plotOutput("p1"),
      showcase_layout = bslib::showcase_left_center(width = 0.45)
    )
  
  vb_RH <- 
    value_box(
      class = "border-0 shadow-none",
      
      title = "Relative Humidity",
      value = "33 %",
      htmltools::p("Dew point: 49.8 °F"),
      showcase = shiny::plotOutput("p2"),
      showcase_layout = bslib::showcase_left_center(width = 0.45)
    )
  
  vb_P <- 
    value_box(
      class = "border-0 shadow-none",
      
      title = "Precipitation",
      value = "0.00 inches",
      htmltools::p("Since midnight"),
      showcase = shiny::plotOutput("p3"),
      showcase_layout = bslib::showcase_left_center(width = 0.45)
    )
  
  vb_Tsoil10cm <- 
    value_box(
      class = "border-0 shadow-none",
      
      title = "Soil Temperature",
      value = "90.4 °F",
      htmltools::p("4-inch depth"),
      showcase = shiny::plotOutput("p4"),
      showcase_layout = bslib::showcase_left_center(width = 0.45)
    )
  
  vb_Tsoil50cm <-
    value_box(
      class = "border-0 shadow-none",
      
      title = "Soil Temperature",
      value = "82.7 °F",
      htmltools::p("20-inch depth"),
      showcase = shiny::plotOutput("p5"),
      showcase_layout = bslib::showcase_left_center(width = 0.45)
    )
  
  vb_SR <- 
    value_box(
      class = "border-0 shadow-none",
      
      title = "Solar Radiation",
      value = "560.34 W/m\u00B2",
      showcase = shiny::plotOutput("p6"),
      showcase_layout = bslib::showcase_left_center(width = 0.45)
    )
  
  vb_WS <- 
    value_box(
      class = "border-0 shadow-none",
      
      title = "Wind speed",
      value = "10.5 mph",
      htmltools::p("Direction: easterly"),
      htmltools::p("Maximum: 33.7 mph"),
      # htmltools::p("Wind chill: NA °F"), # Make into conditional
      showcase = shiny::plotOutput("p7"),
      showcase_layout = bslib::showcase_left_center(width = 0.45)
    )
  
  vb_WS2min <- 
    value_box(
      class = "border-0 shadow-none",
      
      title = "Wind speed 2-min",
      value = "7.4 mph",
      htmltools::p("Direction: southeasterly"),
      htmltools::p("Maximum: 24.9 mph"),
      showcase = shiny::plotOutput("p8"),
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