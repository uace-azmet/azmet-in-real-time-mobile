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
          # bsicons::bs_icon("sliders", class = "bolder-icon"), 
          htmltools::HTML("&nbsp;<strong>AZMet Station</strong>&nbsp;"),
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
            label = NULL,
              # htmltools::p(
              #   "AZMet Station",
              #   bslib::tooltip(
              #     bsicons::bs_icon("info-circle"),
              #     "Specify an AZMet station or tap on the location pin to select the nearest one.",
              #     id = "infoDataOptions",
              #     placement = "right"
              #   )
              # ),
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
        ),
        
        # htmltools::div(
        #   # class = "col-12",
        #   htmltools::hr()
        # ),
        
        value_box(
          class = "border-0 shadow-none",
          fill = TRUE,
          full_screen = FALSE,
          height = NULL,
          id = NULL,
          max_height = NULL,
          min_height = NULL,
          theme = NULL,
          
          showcase = bs_icon("graph-up"),
          showcase_layout = showcase_left_center(width = 0.5),
          title = "Air Temperature",
          value = "99 °F",
          p("Feels like 109 °F")
        ),
        
        value_box(
          title = "I got",
          value = "99 problems",
          showcase = bs_icon("music-note-beamed"),
          p("bslib ain't one", bs_icon("emoji-smile")),
          p("hit me", bs_icon("suit-spade"))
        ),
        
        value_box(
          title = "I got",
          value = "99 problems",
          showcase = bs_icon("music-note-beamed"),
          p("bslib ain't one", bs_icon("emoji-smile")),
          p("hit me", bs_icon("suit-spade"))
        ),
        
        value_box(
          title = "I got",
          value = "99 problems",
          showcase = bs_icon("music-note-beamed"),
          p("bslib ain't one", bs_icon("emoji-smile")),
          p("hit me", bs_icon("suit-spade"))
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
  
  # Temperature outputs (both regular and fullscreen)
  output$temp_current <- renderText({
    paste0(station_data() |> slice_tail(n = 1) |> pull(temp_airC), "°C")
  })

  output$temp_plot <- renderPlot({
    p <- ggplot(station_data(), aes(x = datetime, y = temp_airC)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 3) +
      scale_y_continuous(labels = \(x) paste(x, "ºC")) +
      scale_x_datetime(date_breaks = "hours", date_labels = "%I:%M %p") +
      theme(axis.title = element_blank())
    plot(p)
  })
}


# Run --------------------


shiny::shinyApp(ui = ui, server = server)
