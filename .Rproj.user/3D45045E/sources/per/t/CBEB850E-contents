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
      summarise(n_bill = length(unique(bill_number))) |> 
      rename(NAME = state),
    by = "NAME"
  )

