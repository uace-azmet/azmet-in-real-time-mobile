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
      showcase_layout = bslib::showcase_left_center(width = 0.45),
      
      title = "Air Temperature",
      value = paste0(dataPoint %>% dplyr::pull(temp_airF), " °F"),
      
      # htmltools::p("Heat index: NA °F"), # Make into conditional
      # htmltools::p("Wind chill: NA °F"), # Make into conditional
      htmltools::p(
        class = "value-box-text", 
        paste0("Maximum: ", dataPoint %>% dplyr::pull(temp_air_maxF), " °F")
      ),
      htmltools::p(
        class = "value-box-text", 
        paste0("Minimum: ", dataPoint %>% dplyr::pull(temp_air_minF), " °F")
      )
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
      showcase_layout = bslib::showcase_left_center(width = 0.45),
      
      title = "Relative Humidity",
      value = paste0(dataPoint %>% dplyr::pull(relative_humidity), " %"),
      
      htmltools::p(
        class = "value-box-text", 
        paste0("Dew point: ", dataPoint %>% dplyr::pull(dwptF), " °F")
      )
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
      showcase_layout = bslib::showcase_left_center(width = 0.45),
      
      title = "Precipitation",
      value = paste0(dataPoint %>% dplyr::pull(precip_total_in), " in.")
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
      showcase_layout = bslib::showcase_left_center(width = 0.45),
      
      title = "Soil Temperature",
      value = paste0(dataPoint %>% dplyr::pull(temp_soil_10cmF), " °F"),
      
      htmltools::p(class = "value-box-text", "4-inch depth")
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
      showcase_layout = bslib::showcase_left_center(width = 0.45),
      
      title = "Soil Temperature",
      value = paste0(dataPoint %>% dplyr::pull(temp_soil_50cmF), " °F"),
      
      htmltools::p(class = "value-box-text", "20-inch depth")
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
      showcase_layout = bslib::showcase_left_center(width = 0.45),
      
      title = "Solar Radiation",
      value = paste0(dataPoint %>% dplyr::pull(sol_rad_Wm2), " W/m\u00B2")
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
      showcase_layout = bslib::showcase_left_center(width = 0.45),
      
      title = "Wind speed",
      value = paste0(dataPoint %>% dplyr::pull(wind_spd_mph), " mph"),
      
      # htmltools::p(class = "value-box-text", "Direction: E"),
      htmltools::p(
        class = "value-box-text", 
        paste0("Direction: ", fxn_deg_to_dir(valueIn = dataPoint %>% dplyr::pull(wind_vector_dir)))
      ),
      # htmltools::p(class = "value-box-text", "Maximum: 33.7 mph"),
      htmltools::p(
        class = "value-box-text", 
        paste0("Maximum: ", dataPoint %>% dplyr::pull(wind_spd_max_mph), " mph")
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
      showcase_layout = bslib::showcase_left_center(width = 0.45),
      
      title = "Wind speed 2-min",
      value = paste0(dataPoint %>% dplyr::pull(wind_2min_spd_mean_mph), " mph"),
      
      htmltools::p(
        class = "value-box-text", 
        paste0("Direction: ", fxn_deg_to_dir(valueIn = dataPoint %>% dplyr::pull(wind_2min_vector_dir)))
      ),
      htmltools::p(
        class = "value-box-text", 
        paste0("Maximum: ", dataPoint %>% dplyr::pull(wind_2min_spd_max_mph_daily), " mph")
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
