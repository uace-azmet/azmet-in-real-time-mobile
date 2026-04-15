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
        title = NULL,
        theme = theme, # `scr##_theme.R`
        
        # htmltools::tags$head(htmltools::includeHTML("www/pwa/pwa.html")),
        
        htmltools::p(
          bsicons::bs_icon("sliders", class = "bolder-icon"), 
          htmltools::HTML("&nbsp;<strong>DATA OPTIONS</strong>&nbsp;"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Specify an AZMet station or tap on the location pin to select the nearest one.",
            id = "infoDataOptions",
            placement = "right"
          ),
          
          class = "data-options-title"
        ),
        
        htmltools::div(
          class = "download-buttons-div",
          style = "display: flex; align-items: center; column-gap: 1.5rem;", # Flexbox styling
          
          shiny::selectInput(
            inputId = "azmetStation", 
            label = "AZMet Station",
            choices = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5", "Group 6"),
            selected = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5", "Group 6")[1]
          ),
          
          shiny::actionButton(
            inputId = "open",
            label = span(
              bsicons::bs_icon("geo-alt", class = "bolder-icon geoloc"),
              textOutput("selected_station", container = span)
            ),
            class = "btn btn-default btn-blue geoloc"
          )
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
