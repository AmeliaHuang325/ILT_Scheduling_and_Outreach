library(rgdal)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(DT)
library(shinyTree)
library(shinyWidgets)

# load data
dat <- readr::read_csv("data/2022 NCDP Disaster Mitigation State Bill Tracking_Public.csv") |> 
  janitor::clean_names() |> 
  select(state, bill_number, summary, hazard_type, ncdp_categories, subcategory)

temp <- dat |> 
  group_by(state) |> 
  summarise(n_bill = n_distinct(bill_number)) |> 
  mutate(state_n = paste(state, "<br/>", "<br/>",
                         "(Total Number of Bills = ", 
                         n_bill, 
                         ")",
                         sep = "")) |> 
  select(state, state_n)

dat <- dat |> left_join(temp, by = "state") 

dat <- dat |> mutate(ncdp_categories_n = ncdp_categories,
                     subcategory_n = subcategory)

dat <- dat |> 
  mutate(ncdp_categories = gsub("[\\&]", "and", ncdp_categories),
         ncdp_categories = gsub("[+-]", " ", ncdp_categories),
         ncdp_categories = gsub("\\s+", ".", ncdp_categories),
         subcategory = gsub("[\\&]", "and", subcategory),
         subcategory = gsub("[+-]", " ", subcategory),
         subcategory = gsub(" ", ".", subcategory)
         )

var_list <- list(
  Communications = structure("Communications", stselected=TRUE, sttype="default", sticon="file"),
  Energy = structure("Energy", stselected=FALSE, sttype="default", sticon="file"),
  Governance = structure("Governance", stselected=FALSE, sttype="default", sticon="file"),
  `Hazardous Materials` = structure("Hazardous Materials", stselected=FALSE, sttype="default", sticon="file"),
  `Health and Medical` = structure("Health and Medical", stselected=FALSE, sttype="default", sticon="file"),
  Housing = structure("Housing", stselected=FALSE, sttype="default", sticon="file"),
  `Land Use` = structure("Land Use", stselected=FALSE, sttype="default", sticon="file"),
  `Safety and Security` = structure("Safety and Security", stselected=FALSE, sttype="default", sticon="file"),
  Transportation = structure("Transportation", stselected=FALSE, sttype="default", sticon="file"),
  
  Equity = structure(list(
    Disability = structure("Disability", stselected=FALSE, sttype="default", sticon="file"), 
    Elderly = structure("Elderly", stselected=FALSE, sttype="default", sticon="file"), 
    Gender = structure("Gender", stselected=FALSE, sttype="default", sticon="file"), 
    `LGBTQ+` = structure("LGBTQ+", stselected=FALSE, sttype="default", sticon="file"),
    `Low Income` = structure("Low Income", stselected=FALSE, sttype="default", sticon="file"), 
    Minority = structure("Minority", stselected=FALSE, sttype="default", sticon="file"), 
    Rural = structure("Rural", stselected=FALSE, sttype="default", sticon="file")
  ), stselected=FALSE, sttype="default", sticon="file"),
  
  `Food and Water` = structure(
    list(Food = structure("Food", stselected=FALSE, sttype="default", sticon="file"), 
         Water = structure("Water", stselected=FALSE, sttype="default", sticon="file")), 
    stselected=FALSE, sttype="default", sticon="file"
    ),
  
  Funding = structure(list(
    `Assistance for Individuals and Households` = structure("Assistance for Individuals and Households", stselected=FALSE, sttype="default", sticon="file")  ,
    `Assistance to Individuals and Households` = structure("Assistance to Individuals and Households", stselected=FALSE, sttype="default", sticon="file")   ,
    `Assistance to Local Governments` = structure("Assistance to Local Governments", stselected=FALSE, sttype="default", sticon="file")            ,
    `Assistance to Private Sector and Nonprofits` = structure("Assistance to Private Sector and Nonprofits", stselected=FALSE, sttype="default", sticon="file"),
    `Cost share` = structure("Cost share", stselected=FALSE, sttype="default", sticon="file")                                 ,
    `Federally related Spending` = structure("Federally related Spending", stselected=FALSE, sttype="default", sticon="file")                 ,
    `Insurance` = structure("Insurance", stselected=FALSE, sttype="default", sticon="file")                                  ,
    `State Program Funding` = structure("State Program Funding", stselected=FALSE, sttype="default", sticon="file")                      
  ), 
  stselected=FALSE, sttype="default", sticon="file"
  )
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
      summarise(n_bill = length(unique(bill_number))) |> 
      rename(NAME = state),
    by = "NAME"
  )

# my_choices <- sort(unique(dat$state))

# function
# collapsibleAwesomeCheckboxGroupInput <- 
#   function(inputId, label, i, choices = NULL, selected = NULL,  
#            status = "primary", width = NULL){
#     input <- awesomeCheckboxGroup(inputId, label, choices = choices, 
#                                   selected = selected, width = width,
#                                   status = status)
#     checkboxes <- input[[3]][[2]][[3]][[1]]
#     id_btn <- paste0(inputId, "_btn")
#     id_div <- paste0(inputId, "_collapsible")
#     btn <- actionButton(id_btn, "Sub-category...", 
#                         style = "margin-bottom: 12px",
#                         icon = icon("collapse-up", lib = "glyphicon"), 
#                         class = "btn-primary btn-sm", 
#                         `data-toggle`="collapse", 
#                         `data-target` = paste0("#", id_div))
#     collapsible <- div(id = id_div, class = "collapse")
#     collapsible$children <- checkboxes[(i+1):length(checkboxes)]
#     children <- c(checkboxes[1:i], list(btn), list(collapsible))
#     input[[3]][[2]][[3]][[1]] <- children
#     script <- sprintf('$(document).ready(function(){
#       $("#%s_collapsible").on("hide.bs.collapse", function(){
#         $("#%s_btn").html("<span class=\\\"glyphicon glyphicon-collapse-down\\\"></span> Sub-category");
#       });
#       $("#%s_collapsible").on("show.bs.collapse", function(){
#         $("#%s_btn").html("<span class=\\\"glyphicon glyphicon-collapse-up\\\"></span> Less...");
#       });
#     });', inputId, inputId, inputId, inputId)
#     tagList(input, tags$script(HTML(script)))
#   }

# User interface ----
ui <- fluidPage(
  
  # title
  titlePanel("2022 NCDP Disaster Mitigation State Bill Tracking"),
  
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
      DTOutput('us_table') # output table
      # fluidRow(column("",
      #                 tableOutput("Table"), width = 12,
      #                 align = "center"))
      )
  )
)

# Server logic ----
server <- function(input, output, session) {
  
  output$tree <- renderTree({
    # sss = var_list
    # attr(sss[[1]],"stselected")=TRUE
    var_list
  })
  
  # output$Table <- renderPrint({
  #   
  #   names(as.data.frame(get_selected(input$tree, format = "slices")))
  #   
  # })
  
  # add State: to actual state names
  mytext <- paste(
    "State: ", us_spdf@data$NAME,"<br/>",
    "Total Number of Bills: ", us_spdf@data$n_bill,"<br/>",
    sep="") %>%
    lapply(htmltools::HTML)
  
  # leaflet map using US states shapefile downloaded online
  output$us_map <- renderLeaflet({
    leaflet(us_spdf) %>%
      addTiles()  %>%
      setView(lat=10, lng=0 , zoom=2) %>%
      addPolygons(
        stroke=TRUE,
        fillOpacity = 0.5,
        color="white",
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addPolylines(stroke=TRUE, weight = 2, color= "skyblue")
  })
  
  # add a leaflet proxy
  proxy <- leafletProxy("us_map")
  
  # interactive control of the map
  observe({
    # 
    # updateCheckboxGroupInput(
    #   session, "state", choices = my_choices,
    #   selected = if(input$all) my_choices
    # )
    
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
  
  # render table based on selections of states and categories
  output$us_table <- renderDT(
    {
      
      # get the input categories
      x <- get_selected(input$tree, format = "slices") |> as.data.frame() |> names()
      
      # filter the dataset by input categories
      dat_s <- dat |> filter(state %in% input$state,
                  ncdp_categories %in% x[x %in% names(var_list)] | subcategory %in% x[!x %in% names(var_list)])
      
      # rename variables for printed table  
      dat_print <- dat_s |> 
        select(state_n, bill_number, summary, 
               hazard_type, ncdp_categories_n, subcategory_n) |> 
        rename(State = state_n, `Bill Number` = bill_number, `Summary` = summary, 
               `Hazard Type` = hazard_type, `NCDP Categories` = ncdp_categories_n, 
               `NCDP Subcategories` = subcategory_n)
      
      # render datatable
      dtable <- datatable(dat_print,  
                          rownames = FALSE, 
                          escape = FALSE,
                          options = list(
                            rowsGroup = list(0,1,2,3,4),
                            pageLength = 50,
                            columnDefs = list(list(width = '100px', targets = 0)),
                            language = list(lengthMenu = 
                                              paste("Display _MENU_ Entries for", 
                                                    length(unique(dat_print$`Bill Number`)), 
                                                    "Bills across", 
                                                    length(unique(dat_print$State)),
                                                    "States",
                                                    sep = " "))
                          )) |> 
        formatStyle(columns = c("State"), fontWeight = 'bold')
      path <- here::here() # folder containing dataTables.rowsGroup.js
      dep <- htmltools::htmlDependency(
        "RowsGroup", "2.0.0", 
        path, script = "dataTables.rowsGroup.js")
      dtable$dependencies <- c(dtable$dependencies, list(dep))
      dtable
    }
    
    )
  
}

# Run app ----
shinyApp(ui, server)