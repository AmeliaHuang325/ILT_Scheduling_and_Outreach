library(rmapshaper)
library(sf)

region_spdf_raw <- st_read("data/fema_regions")
simplified_sf <- ms_simplify(region_spdf_raw , keep = 0.01)  

st_write(simplified_sf, "data/fema_regions_simplified/simplified_data.shp")
