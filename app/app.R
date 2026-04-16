# Mobile-first Shiny app displaying tabular and graphical summaries of the latest 15-minute data from stations across the network

# Add code for the following
# 
# 'azmet-shiny-template.html': <!-- Google tag (gtag.js) -->


# # For now just get data for all sites on app load
data <- fxn_az15min()

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
        fillable = FALSE,
        fillable_mobile = FALSE,
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
            choices = station_choices,
            selected = station_choices[1]
          ),
          
          shiny::actionButton(
            inputId = "open",
            label = 
              span(
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
  
  
  # Set a default station
  # TODO: use cookies for this
  station <- reactiveVal("az01")
  
  # Create modal to contain picker with location button
  observeEvent(input$open, {
    showModal(
      # TODO: it would be nice if making a selection closed the modal
      modalDialog(
        location_select_ui(
          "loc_module",
          "Select a station:",
          station_choices,
          selected = station()
        ),
        footer = NULL,
        easyClose = TRUE
      )
    )
  })
  
  # Get results of location selection
  station_id_choice <- location_select_server(
    "loc_module",
    station_choices
  )
  
  # When a choice is made, update the default station
  observeEvent(station_id_choice(), {
    station(isolate(station_id_choice()))
  })
  
  # Print the station name for the modal button.
  output$selected_station <- renderText({
    station_choices |> filter(value == station()) |> pull(choice)
  })
  
  station_data <- reactive({
    data |> filter(meta_station_id == station())
  })
  
  
  # Observables -----
  
  # Reactives -----
  
  # Outputs -----
  
}


# Run --------------------


shiny::shinyApp(ui = ui, server = server)
