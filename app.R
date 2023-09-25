library(rgdal)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(DT)
library(shinyTree)
library(shinyWidgets)



# Clean and Load data -----------------------------------------------------

# load data
source("Data Cleaning.R") # source the data from the cleaned app

dat <- df_s |> 
  group_by(states) |>
  mutate(n_course_state = sum(n_each_course_state)) |>
  mutate(state_n = paste(states, "<br/>", "<br/>",
                         "(Total Number of Training = ", 
                         n_course_state, 
                         ")",
                         sep = ""))

# Join the state name to the main list

state_name <- openxlsx::read.xlsx("data/States_codes_regions.xlsx") |> 
  janitor::clean_names() 

dat <- dat |>
  left_join(state_name,by = c("states"= "state_2"))



# create a list for the course info

var_list <- list(
  Housing = structure(list(
    MGT_477 = structure("MGT-477", stselected=FALSE, sttype="default", sticon="file"), 
    MGT_472 = structure("MGT-472", stselected=FALSE, sttype="default", sticon="file")
  ), stselected=FALSE, sttype="default", sticon="file"),
  
  Mass_Care = structure(list(
    MGT_487 = structure("MGT-487", stselected=FALSE, sttype="default", sticon="file"), 
    PER_406 = structure("PER-406", stselected=FALSE, sttype="default", sticon="file")
  ), stselected=FALSE, sttype="default", sticon="file"),
  
  Pandemic_Preparedness = structure(list(
    MGT_488 = structure("MGT-488", stselected=FALSE, sttype="default", sticon="file"), 
    PER_409 = structure("PER-409", stselected=FALSE, sttype="default", sticon="file")
  ), stselected=FALSE, sttype="default", sticon="file"),
  
  Climate_Equity = structure(list(
    MGT_491 = structure("MGT-491", stselected=FALSE, sttype="default", sticon="file"), 
    PER_420 = structure("PER-420", stselected=FALSE, sttype="default", sticon="file")
  ), stselected=FALSE, sttype="default", sticon="file")
)


# us map data
us_spdf <- rgdal::readOGR( 
  dsn= "data/cb_2018_us_state_20m", 
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
  )


# User Interface ----------------------------------------------------------

ui <- fluidPage(
  
  # title
  titlePanel("Instructor-led Trainings Scheduling & Outreach Tracking"),
  
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
                  h4("Choose a category"),
                  shinyTree("tree",checkbox = TRUE, search = TRUE,
                            themeIcons = FALSE, themeDots = FALSE,
                            wholerow = FALSE))
           
           
           
           # column(6,
           #   # choose ncdp categories
           #   awesomeCheckboxGroup("ncdp_categories",
           #                      label = "Choose a category",
           #                      choices = sort(unique(dat$ncdp_categories)),
           #                      selected = sort(unique(dat$ncdp_categories)))
           # )
    ),
    
    mainPanel(
      leafletOutput("us_map"), # output map
      # DTOutput('us_table') # output table
     
    )
  )
)


# Server Logic ------------------------------------------------------------

server <- function(input, output, session) {
  
  output$tree <- renderTree({
    var_list
  })
  
  # Gradient colors for the map based on n_course_state
  colorpal <- colorNumeric(palette = "viridis", domain = us_spdf@data$n_course_state)
  
  
  # Add State: to actual state names
  mytext <- paste(
    "State: ", us_spdf@data$NAME,"<br/>",
    "Total Number of Trainings: ", us_spdf@data$n_course_state,"<br/>",
    sep="") %>%
    lapply(htmltools::HTML)
  
  # Leaflet map using US states shapefile downloaded online
  output$us_map <- renderLeaflet({
    leaflet(us_spdf) %>%
      addTiles() %>%
      setView(lat=10, lng=0 , zoom=2) %>%
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
      addPolylines(stroke = TRUE, weight = 2, color = "skyblue") %>%
      addLegend(pal = colorpal, values = us_spdf@data$n_course_state,
                title = "Number of Trainings",
                position = "bottomright")
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

