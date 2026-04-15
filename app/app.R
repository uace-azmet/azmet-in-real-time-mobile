# Mobile-first Shiny app displaying tabular and graphical summaries of the latest 15-minute data from stations across the network

# Add code for the following
# 
# 'azmet-shiny-template.html': <!-- Google tag (gtag.js) -->


# # For now just get data for all sites on app load
data <- az_15min(start = now() - hours(3), end = now())

station_choices <- azmetr::station_info |>
  select(
    choice = meta_station_name,
    value = meta_station_id,
    lat = latitude,
    lon = longitude
  ) |>
  filter(choice != "Test") |>
  dplyr::arrange(choice)


# UI --------------------


ui <- 
  htmltools::htmlTemplate(
    
    filename = "azmet-shiny-template.html",
    
    azmetInRealTimeMobile = 
      
      bslib::page_fillable(
        shiny::actionButton(
          inputId = "open",
          label = span(
            bsicons::bs_icon("geo-alt"),
            textOutput("selected_station", container = span)
          ),
          class = "btn-primary btn-m"
        )
      )
  ) # htmltools::htmlTemplate()


# Server --------------------


server <- function(input, output, session) {
  
  # Observables -----
  
  # Reactives -----
  
  # Outputs -----
  
}


# Run --------------------


shiny::shinyApp(ui = ui, server = server)
