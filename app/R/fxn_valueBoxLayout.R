#' `fxn_valueBoxLayout.R` - Build value box layout for display
#' 
#' @param inData - AZMet 15-minute data for user-selected station from `fxn_az15min.R`
#' @return `valueBoxLayout` - Value box layout for display


fxn_valueBoxLayout <- function(inData) {
  
  dataPoint <- inData %>% 
    dplyr::filter(
      lubridate::as_datetime(datetime) == max(lubridate::as_datetime(datetime))
    )
  
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
      
      showcase = shiny::plotOutput("vbChart_T"),
      showcase_layout = bslib::showcase_left_center(width = 0.4),
      
      title = "Air Temperature",
      value = 
        paste0(format(dataPoint %>% dplyr::pull(temp_airF), nsmall = 1), " °F"),
      
      htmltools::p(
        class = "value-box-text", 
        paste0(
          "Heat index: ", format(dataPoint %>% dplyr::pull(temp_heat_indexF), nsmall = 1), " °F"
        )
      ),
      htmltools::p(
        class = "value-box-text", 
        paste0(
          "Maximum: ", format(dataPoint %>% dplyr::pull(temp_air_maxF), nsmall = 1), " °F"
        )
      ),
      htmltools::p(
        class = "value-box-text", 
        paste0(
          "Minimum: ", format(dataPoint %>% dplyr::pull(temp_air_minF), nsmall = 1), " °F"
        )
      )
      # htmltools::p("Wind chill: NA °F"), # Make into conditional
    )
  
  vb_RH <- 
    bslib::value_box(
      class = "border-0 shadow-none",
      fill = TRUE,
      full_screen = FALSE,
      height = NULL,
      id = NULL,
      max_height = NULL,
      min_height = NULL,
      theme = NULL,
      
      showcase = shiny::plotOutput("vbChart_RH"),
      showcase_layout = bslib::showcase_left_center(width = 0.4),
      
      title = "Relative Humidity",
      value = 
        paste0(format(dataPoint %>% dplyr::pull(relative_humidity), nsmall = 0), " %"),
      
      htmltools::p(
        class = "value-box-text", 
        paste0(
          "Dew point: ", format(dataPoint %>% dplyr::pull(dwptF), nsmall = 1), " °F"
        )
      ),
      htmltools::br() # For even spacing when more than one column
    )
  
  vb_P <- 
    bslib::value_box(
      class = "border-0 shadow-none",
      fill = TRUE,
      full_screen = FALSE,
      height = NULL,
      id = NULL,
      max_height = NULL,
      min_height = NULL,
      theme = NULL,
      
      showcase = shiny::plotOutput("vbChart_P"),
      showcase_layout = bslib::showcase_left_center(width = 0.4),
      
      title = "Precipitation",
      value = 
        paste0(format(dataPoint %>% dplyr::pull(precip_total_in), nsmall = 2), " in."),
      
      htmltools::p(class = "value-box-text", "Daily total"),
      htmltools::br() # For even spacing when more than one column
    )
  
  vb_Tsoil10cm <- 
    bslib::value_box(
      class = "border-0 shadow-none",
      fill = TRUE,
      full_screen = FALSE,
      height = NULL,
      id = NULL,
      max_height = NULL,
      min_height = NULL,
      theme = NULL,
      
      showcase = shiny::plotOutput("vbChart_Tsoil10cm"),
      showcase_layout = bslib::showcase_left_center(width = 0.4),
      
      title = "Soil Temperature",
      value = 
        paste0(format(dataPoint %>% dplyr::pull(temp_soil_10cmF), nsmall = 1), " °F"),
      
      htmltools::p(class = "value-box-text", "4-inch depth"),
      htmltools::br() # For even spacing when more than one column
    )
  
  vb_Tsoil50cm <-
    bslib::value_box(
      class = "border-0 shadow-none",
      fill = TRUE,
      full_screen = FALSE,
      height = NULL,
      id = NULL,
      max_height = NULL,
      min_height = NULL,
      theme = NULL,
      
      showcase = shiny::plotOutput("vbChart_Tsoil50cm"),
      showcase_layout = bslib::showcase_left_center(width = 0.4),
      
      title = "Soil Temperature",
      value = 
        paste0(format(dataPoint %>% dplyr::pull(temp_soil_50cmF), nsmall = 1), " °F"),
      
      htmltools::p(class = "value-box-text", "20-inch depth"),
      htmltools::br() # For even spacing when more than one column
    )
  
  vb_SR <- 
    bslib::value_box(
      class = "border-0 shadow-none",
      fill = TRUE,
      full_screen = FALSE,
      height = NULL,
      id = NULL,
      max_height = NULL,
      min_height = NULL,
      theme = NULL,
      
      showcase = shiny::plotOutput("vbChart_SR"),
      showcase_layout = bslib::showcase_left_center(width = 0.4),
      
      title = "Solar Radiation",
      value = 
        paste0(format(dataPoint %>% dplyr::pull(sol_rad_Wm2), nsmall = 2), " W/m\u00B2"),
      
      htmltools::br(), # For even spacing when more than one column
      htmltools::br() # For even spacing when more than one column
    )
  
  vb_WS <- 
    bslib::value_box(
      class = "border-0 shadow-none",
      fill = TRUE,
      full_screen = FALSE,
      height = NULL,
      id = NULL,
      max_height = NULL,
      min_height = NULL,
      theme = NULL,
      
      showcase = shiny::plotOutput("vbChart_WS"),
      showcase_layout = bslib::showcase_left_center(width = 0.4),
      
      title = "Wind speed",
      value = 
        paste0(format(dataPoint %>% dplyr::pull(wind_spd_mph), nsmall = 1), " mph"),
      
      htmltools::p(
        class = "value-box-text", 
        paste0(
          "Direction: ", fxn_deg_to_dir(valueIn = dataPoint %>% dplyr::pull(wind_vector_dir))
        )
      ),
      htmltools::p(
        class = "value-box-text", 
        paste0(
          "Maximum: ", format(dataPoint %>% dplyr::pull(wind_spd_max_mph), nsmall = 1), " mph"
        )
      )
    )
  
  vb_WS2min <- 
    bslib::value_box(
      class = "border-0 shadow-none",
      fill = TRUE,
      full_screen = FALSE,
      height = NULL,
      id = NULL,
      max_height = NULL,
      min_height = NULL,
      theme = NULL,
      
      showcase = shiny::plotOutput("vbChart_WS2min"),
      showcase_layout = bslib::showcase_left_center(width = 0.4),
      
      title = htmltools::span("Wind speed", htmltools::tags$sub("2-min")),
      value = 
        paste0(format(dataPoint %>% dplyr::pull(wind_2min_spd_mean_mph), nsmall = 1), " mph"),
      
      htmltools::p(
        class = "value-box-text", 
        paste0(
          "Direction: ", fxn_deg_to_dir(valueIn = dataPoint %>% dplyr::pull(wind_2min_vector_dir))
        )
      ),
      htmltools::p(
        class = "value-box-text", 
        paste0(
          "Maximum: ", format(dataPoint %>% dplyr::pull(wind_2min_spd_max_mph_daily), nsmall = 1), " mph"
        )
      )
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
