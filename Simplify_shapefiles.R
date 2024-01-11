library(rmapshaper)
library(sf)
library(dplyr)

region_spdf_raw <- st_read("data/fema_regions")
simplified_sf <- ms_simplify(region_spdf_raw , keep = 0.01)  

st_write(simplified_sf, "data/fema_regions_simplified/simplified_data.shp")


states_spdf_raw <- st_read("data/cb_2018_us_state_20m")
simplified_states_sf <- ms_simplify(states_spdf_raw , keep = 0.5) 

lost_objects <- setdiff(states_spdf_raw$GEOID, simplified_states_sf$GEOID)

st_write(simplified_states_sf, "data/us_states_simplified/us_states_simplified.shp")



