library(rgdal)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(sf)
library(bslib)


source("Data_cleaning_2.R")

# Load data ---------------------------------------------------------------

dat <- df_s |> 
  group_by(states) |>
  mutate(n_course_state = sum(n_each_course_state)) |>
  mutate(state_n = paste(states, "<br/>", "<br/>",
                         "(Total Number of Training = ", 
                         n_course_state, 
                         ")",
                         sep = ""))

# Join the state name to the main list

state_name <- openxlsx::read.xlsx(here::here("data/States_codes_regions.xlsx")) |> 
  janitor::clean_names() 

dat <- dat |>
  left_join(state_name,by = c("states"= "state_2")) |>
  mutate(region_2 = ifelse(!is.na(region), 
                           paste0("REGION ", as.roman(region)),
                           NA))

region_order <- c("REGION I", "REGION II", "REGION III", "REGION IV", "REGION V", 
                  "REGION VI", "REGION VII", "REGION VIII", "REGION IX", "REGION X")


# state and aggregated courses
dat_agg <- dat %>%
  group_by(state) %>%
  summarise(training_list = paste(training, collapse = ", "))


# us map data

us_spdf <- rgdal::readOGR(
  dsn= here::here("data/us_states_simplified"),
  verbose=FALSE
)

us_spdf@data <- us_spdf@data |> 
  left_join(
    dat |> 
      group_by(state) |> 
      filter(row_number() == 1) |> 
      rename(NAME = state) |>
      select("NAME", "n_course_state"),
    by = "NAME"
  ) |>
  left_join(
    dat_agg, by = c("NAME" = "state")
  )


# us FEMA Region data
region_spdf <- st_read("data/fema_regions_simplified")

# us CNMI shapefile

cnmi_spdf <- st_read("data/commonwealth_of_the_northern_mariana_islands")

# User Interface ----------------------------------------------------------

ui <- fluidPage(
  
  # Using custom CSS to set heights based on the viewport height
  tags$head(
    tags$style(HTML("
      .top-section { height: 50vh; } /* 50% of the viewport height */
      .bottom-section { height: 50vh; } /* 50% of the viewport height */
    "))
  ),
  
  # title
  titlePanel("ILT Scheduling & Outreach Tracking (active courses)"),
  
  # input panel
  fluidRow(
    column(4,
           div(class = "top-section",      
           h4("Choose a course"),
           awesomeCheckboxGroup("course", 
                                label = "",
                                choices = sort(unique(dat$training)),
                                selected = NULL,
                                inline = F)),
           
           div(class = "bottom-section",
           column(6,
                  leafletOutput("cnmi_map", height = "300px")),
           column(4,
                  p("NOTE: We held an MGT472 in Commonwealth of the Northern Mariana Islands"))
           )),
    
    mainPanel(
      div(class = "top-section", leafletOutput("us_map")), # output map
      div(class = "buttom-section", plotlyOutput("course_bar_chart")) # output bar chart
     
    )
  )
)

# Server Logic ------------------------------------------------------------

server <- function(input, output, session) {
  
  # Gradient colors for the map based on n_course_state
  
  # Define the number of colors you want in the palette (e.g., 9 for the maximum in Blues)
  n_colors <- 9
  all_blues <- brewer.pal(n_colors, "Blues")
  
  # Remove the first (lightest) color to make the lightest blue darker
  modified_blues <- all_blues[-1]
  
  # Create the colorNumeric palette with the modified colors
  base_pal <- colorNumeric(palette = modified_blues, domain = us_spdf@data$n_course_state)
  
  # Your custom palette function remains unchanged
  colorpal <- function(x) {
    if (length(x) == 0) {
      return(NULL)
    }
    ifelse(is.na(x), "#00000000", base_pal(x))
  }
  
   
  # Add State: to actual state names
  mytext <- paste(
    "State: ", us_spdf@data$NAME,"<br/>",
    "Total Number of Trainings: ", us_spdf@data$n_course_state,"<br/>",
    "Trainings:", us_spdf@data$training_list, "<br/>",
    sep="") %>%
    lapply(htmltools::HTML)

  
  output$cnmi_map <- renderLeaflet({
    
    map <- leaflet(cnmi_spdf) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lat = 15.0979, lng = 145.6739, zoom = 7) %>%
      addPolygons(
        fillColor = "transparent",
        stroke = TRUE,
        color = "blue",
        opacity = 0.1,
        weight = 1)
    
    return(map)
  })
  
  
  
  
  output$us_map <- renderLeaflet({
    
    map <- leaflet(us_spdf) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lat = 37.0902, lng = -95.7129, zoom = 4) %>%
      addPolygons(
        fillColor = ~colorpal(us_spdf@data$n_course_state),
        stroke = TRUE,
        fillOpacity = 0.5,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addPolylines(stroke = TRUE, weight = 0.3, color = "grey") %>%
      addLegend(pal = base_pal, values = na.omit(us_spdf@data$n_course_state),
                title = NULL,
                position = "bottomright")%>%
    addPolygons(
      data = region_spdf,
      fillColor = "transparent",
      stroke = TRUE,
      color = "blue",
      opacity = 0.5,
      weight = 1)
    
    # Add custom labels without markers or icons
    locations <- data.frame(
      label = c("REGION I", "REGION II", "REGION III", "REGION IV", "REGION V", 
                "REGION VI", "REGION VII", "REGION VIII", "REGION IX", "REGION X"),
      lat = c(44.3601, 42.7128, 39.0458, 32.7465, 40.0000,
              32.7465, 40.0000, 45.5760, 37.7783, 45.5760),
      lng = c(-68.0589, -73.5006, -75.6413, -82.6868, -84.6812,
              -102.2896, -99.5018, -108.2903, -121.5542, -121.5542)
    )
    
    for (i in 1:nrow(locations)) {
      label <- locations$label[i]
      lat <- locations$lat[i]
      lng <- locations$lng[i]
      
      # Add labels for other locations without markers or icons (transparent circle)
      map <- addCircleMarkers(map, lat = lat, lng = lng, radius = 0, color = "transparent",
                              fillOpacity = 0,
                              label = list(
                                htmltools::HTML(sprintf("<span style='font-size: 12px; color: black;'>%s</span>", label))
                              ),
                              labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE))
    }
    
    return(map)
  })
  
  

  output$course_bar_chart <- renderPlotly({
    
    plot_data <- dat %>%
      group_by(region_2, training) %>%
      summarize(total_courses = sum(n_each_course_state), .groups = "drop") %>%
      mutate(region_2 = factor(region_2, levels = region_order)) %>%
      arrange(match(region_2, region_order))
    
    # Get the number of unique trainings for color assignment
    n_trainings <- length(unique(dat$training))
    
    # If there are more trainings than colors in the palette, repeat the palette
    color_scale <- colorRampPalette(RColorBrewer::brewer.pal(min(9, n_trainings), "Pastel1"))(n_trainings)
    
    # Calculating totals for each region_2 for text annotations
    totals <- plot_data %>%
      group_by(region_2) %>%
      summarize(total = sum(total_courses), .groups = "drop")
    
    p <- plot_ly(data = plot_data,
                 x = ~region_2,
                 y = ~total_courses,
                 color = ~training,
                 type = "bar",
                 colors = color_scale) %>%
      layout(barmode = "stack",
             colorway = color_scale)  # ensure that the colorway matches the colors for a consistent appearance
    
    # Adding total labels on top of the bars
    p <- p %>% add_trace(
      data = totals,
      y = ~total,
      x = ~region_2,
      text = ~paste("Total:", total),
      type = "scatter",
      mode = "text",
      textposition = "top center",
      showlegend = FALSE,
      inherit = FALSE # prevent inheriting color and other aesthetics from the previous trace
    )
    
    # Return the final plot
    return(p)
    
  })
  
  # add a leaflet proxy
  proxy <- leafletProxy("us_map")
  
  # interactive control of the map
  observe({
    
    if(length(input$course) > 0){
      
      # Filter states that have any of the selected courses in their training list
      selected_polygons <- subset(us_spdf, sapply(training_list, function(x) {
        any(stringr::str_detect(x, pattern = input$course))
      }))
      
      # Remove any previously highlighted polygon
      proxy %>% clearGroup("highlighted_polygon")
      
      for (i in 1:nrow(selected_polygons)) {
        selected_polygon <- selected_polygons[i, ]
        polygon_labelPt <- selected_polygon@polygons[[1]]@labpt
        
        # Add a slightly thicker red polygon on top of each selected state
        proxy %>% addPolylines(stroke=TRUE,
                               weight = 2,
                               color="red",
                               data=selected_polygon,
                               group="highlighted_polygon")
      }
    } else {
      proxy %>% clearGroup("highlighted_polygon")
    }
  })
}


# Run app ----
shinyApp(ui, server)

