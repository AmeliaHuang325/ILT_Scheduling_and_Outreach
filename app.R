library(rgdal)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(shinyWidgets)
library(plotly)
library(dplyr)

# Clean and Load data -----------------------------------------------------
# Import google sheet data and get authorization --------------------------

sheet <- read_csv("data/Instructor-led Trainings Scheduling & Outreach - Scheduling.csv") %>% 
  janitor::clean_names()


# Select data needed ------------------------------------------------------

col_index <- which(sheet[1, ] == "REMAINING")[1] # see where the word "remaining" is

sheet_selected <- sheet %>%
  select(1:(col_index-1)) %>%  # select needed columns
  filter(row_number() < which(training == "TOTAL")[1]) #select needed rows

## generate new column name based on the first row
new_col_name <- sapply(1:ncol(sheet_selected), function(i) {
  val <- sheet_selected[1,i]
  
  # Check for NA values
  if (is.na(val)) {
    return(names(sheet_selected)[i])
  }
  
  # Check for numbers from 1 to 10
  if (str_detect(val, "^([1-9]|10)$")) {
    return(paste0("region ", val))
  } else {
    return(names(sheet_selected)[i])
  }
}) # if the row is number from 1-10 then use the number, otherwise copy the column name

# Assign the new values to the first row

colnames(sheet_selected) <- new_col_name

# Generate a dataset for Dashboard ----------------------------------------

df_plotting <- sheet_selected %>% 
  slice(-1) %>% # remove the first row of the dataset
  select("training", starts_with("region")) %>% # select training and region data
  fill(!!names(sheet_selected)[1], .direction = "down") %>% #To fill NA values in the first column with the value from the row above
  filter(str_detect(as.character(.[[1]]), "[0-9]"))


df_s <- df_plotting %>% 
  pivot_longer(cols = starts_with("region"),
               names_to = "region",
               values_to = "states"
  )

df_s <- df_s %>% 
  select(training, states) %>% 
  na.omit() %>% 
  group_by(training, states) %>% 
  summarise(n_each_course_state = n())


# Load data ---------------------------------------------------------------


#source(here::here("Data_Cleaning_csv.R")) # source the data from the cleaned app


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

# state and aggregated courses
dat_agg <- dat %>%
  group_by(state) %>%
  summarise(training_list = paste(training, collapse = ", "))

# create a list for the course info

# var_list <- list(
#   Housing = structure(list(
#     `MGT-477` = structure("MGT-477", stselected=FALSE, sttype="default", sticon="file"), 
#     `MGT-472` = structure("MGT-472", stselected=FALSE, sttype="default", sticon="file")
#   ), stselected=FALSE, sttype="default", sticon="file"),
#   
#   Mass_Care = structure(list(
#     `MGT-487` = structure("MGT-487", stselected=FALSE, sttype="default", sticon="file"), 
#     `PER-406` = structure("PER-406", stselected=FALSE, sttype="default", sticon="file")
#   ), stselected=FALSE, sttype="default", sticon="file"),
#   
#   Pandemic_Preparedness = structure(list(
#     `MGT-488` = structure("MGT-488", stselected=FALSE, sttype="default", sticon="file"), 
#     `PER-409` = structure("PER-409", stselected=FALSE, sttype="default", sticon="file")
#   ), stselected=FALSE, sttype="default", sticon="file"),
#   
#   Climate_Equity = structure(list(
#     `MGT-491` = structure("MGT-491", stselected=FALSE, sttype="default", sticon="file"), 
#     `PER-420` = structure("PER-420", stselected=FALSE, sttype="default", sticon="file")
#   ), stselected=FALSE, sttype="default", sticon="file")
# )


# us map data


us_spdf <- rgdal::readOGR(
  dsn= here::here("data/cb_2018_us_state_20m"),
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



#merged_data <- left_join(us_spdf@data, dat_agg, by = c("NAME" = "state"))

# FEMA region data
region_spdf <- rgdal::readOGR(
  dsn= here::here("data/fema_regions"),
  verbose=FALSE
)

#region_spdf <- st_read("data/fema_regions")

# User Interface ----------------------------------------------------------

ui <- fluidPage(
  
  # title
  titlePanel("ILT Scheduling & Outreach Tracking (active courses)"),
  
  # input panel
  fluidRow(
    column(4,
           column(6,
                  
                  #checkboxInput("all", "Select All/None", value = F),
                  
                  # choose state
                  h4("Choose a state"),
                  awesomeCheckboxGroup("state", 
                                       label = "",
                                       choices = sort(unique(dat$state)),
                                       selected = NULL,
                                       inline = F)
           ),

           column(6,
                  h4("Choose a region"),
                  awesomeCheckboxGroup("region", 
                                       label = "",
                                       choices = sort(unique(region_spdf@data$REGION)),
                                       selected = NULL,
                                       inline = F))
    ),
    
    mainPanel(
      leafletOutput("us_map"), # output map
      plotlyOutput("course_bar_chart") # output bar chart
     
    )
  )
)


# Server Logic ------------------------------------------------------------

server <- function(input, output, session) {
  
  # output$tree <- renderTree({
  #   var_list
  # })
  
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
  

  
  #merged_data <- merge(us_spdf@data, dat, by.x = "NAME", by.y = "state")
  
  # mytext <- mapply(function(name, courses, trainings) {
  #   return(paste("State: ", name, "<br/>",
  #                "Total Number of Trainings: ", courses, "<br/>",
  #                "Trainings: ", trainings, "<br/>",
  #                sep=""))
  # }, merged_data$NAME, merged_data$n_course_state, merged_data$training_list) %>%
  #   lapply(htmltools::HTML)
  
  
   
  # Add State: to actual state names
  mytext <- paste(
    "State: ", us_spdf@data$NAME,"<br/>",
    "Total Number of Trainings: ", us_spdf@data$n_course_state,"<br/>",
    "Trainings:", us_spdf@data$training_list, "<br/>",
    sep="") %>%
    lapply(htmltools::HTML)

  # Leaflet map using US states shapefile downloaded online
  # output$us_map <- renderLeaflet({
  #   leaflet(us_spdf) %>%
  #     addTiles() %>%
  #     setView(lat=10, lng=0 , zoom=2) %>%
  #     addPolygons(
  #       fillColor = ~colorpal(us_spdf@data$n_course_state),
  #       stroke = TRUE,
  #       fillOpacity = 0.5,
  #       color = "white",
  #       weight = 0.3,
  #       label = mytext,
  #       labelOptions = labelOptions(
  #         style = list("font-weight" = "normal", padding = "3px 8px"),
  #         textsize = "13px",
  #         direction = "auto"
  #       )
  #     ) %>%
  #     addPolylines(stroke = TRUE, weight = 2, color = "skyblue") %>%
  #     addLegend(pal = base_pal, values = us_spdf@data$n_course_state,
  #               title = "Number of Trainings",
  #               position = "bottomright")
  # })
  
  
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
                position = "bottomright")
    
    # Adjust the bounding box to zoom in more
    adjust_bounds <- function(bounds, factor = 0.5) {
      lng_center <- mean(c(bounds[1, 1], bounds[1, 2]))
      lat_center <- mean(c(bounds[2, 1], bounds[2, 2]))
      
      lng1 <- bounds[1, 1] + (lng_center - bounds[1, 1]) * factor
      lat1 <- bounds[2, 1] + (lat_center - bounds[2, 1]) * factor
      lng2 <- bounds[1, 2] - (bounds[1, 2] - lng_center) * factor
      lat2 <- bounds[2, 2] - (bounds[2, 2] - lat_center) * factor
      
      return(matrix(c(lng1, lat1, lng2, lat2), ncol = 2))
    }
    
    # Zoom into selected states
    if (!is.null(input$state) && length(input$state) > 0) {
      subset_states <- us_spdf[us_spdf@data$NAME %in% input$state, ]
      if (nrow(subset_states) > 0) {
        bounds <- bbox(subset_states)
        adjusted_bounds <- adjust_bounds(bounds)
        map <- map %>% fitBounds(
          lng1 = adjusted_bounds[1, 1], lat1 = adjusted_bounds[2, 1],
          lng2 = adjusted_bounds[1, 2], lat2 = adjusted_bounds[2, 2]
        )
      }
    } else if (!is.null(input$region) && length(input$region) > 0) {
      subset_regions <- region_spdf[region_spdf@data$REGION %in% input$region, ]
      if (nrow(subset_regions) > 0) {
        bounds <- bbox(subset_regions)
        adjusted_bounds <- adjust_bounds(bounds)
        map <- map %>% fitBounds(
          lng1 = adjusted_bounds[1, 1], lat1 = adjusted_bounds[2, 1],
          lng2 = adjusted_bounds[1, 2], lat2 = adjusted_bounds[2, 2]
        )
        map <- map %>%
          addPolygons(
            data = subset_regions,
            fillColor = "transparent",
            stroke = TRUE,
            color = "blue",
            weight = 3
          )
      }
    }
    
    return(map)
  })
  

  output$course_bar_chart <- renderPlotly({


      plot_data <- dat %>%
        group_by(region_2, training) %>%
        summarize(total_courses = sum(n_each_course_state)) %>%
        ungroup()

      plot_ly(data = plot_data,
              x = ~region_2,
              y = ~total_courses,
              color = ~training,
              type = "bar",
              colors = RColorBrewer::brewer.pal(n=length(unique(dat$training)), name="Set1")) %>%
        layout(barmode = "stack")

  })
  
  
  # add a leaflet proxy
  proxy <- leafletProxy("us_map")
  
  # interactive control of the map
  observe({
    
    if(length(input$state) > 0){
      
      #get the selected polygon and extract the label point
      selected_polygon <- subset(us_spdf, us_spdf$NAME %in% input$state)
      
      polygon_labelPt <- selected_polygon@polygons[[1]]@labpt
      
      #remove any previously highlighted polygon
      proxy %>% clearGroup("highlighted_polygon")
      
      #center the view on the polygon
      proxy %>% setView(lng = polygon_labelPt[1],
                        lat=polygon_labelPt[2],
                        zoom = 4)
      
      #add a slightly thicker red polygon on top of the selected one
      proxy %>% addPolylines(stroke=TRUE,
                             weight = 4,
                             color="red",
                             data=selected_polygon,
                             group="highlighted_polygon")
    } else {
      proxy %>% clearGroup("highlighted_polygon")
    }
  }
  )

  
}


# Run app ----
shinyApp(ui, server)

