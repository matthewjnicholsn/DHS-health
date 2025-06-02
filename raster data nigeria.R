library(terra)
library(sf)
library(ggplot2)
library(dplyr)

#read and transform ng shape file
sf <- read_sf("/Users/matthewnicholson/Downloads/nigeria-lgas/new_lga_nigeria_2003.shp")

state_region_mapping <- read.csv("/Users/matthewnicholson/DHS/Nigeria States.csv")

sf <- sf %>%
  left_join(state_region_mapping, by = c("STATE" = "state"))

sf <- sf %>%
  group_by(region) %>%
  summarize(geometry = st_union(geometry)) 

#load the pop raster
Ng_pop <- rast("/Users/matthewnicholson/Downloads/NGA_population_v2_1_gridded/NGA_population_v2_1_gridded.tif")
#crop by sf
Ng_pop <- crop(Ng_pop, sf)
plot(Ng_pop)
#mask by sf

Ng_pop <- mask(Ng_pop, sf)
plot(Ng_pop)

#disaggregate
plot(Ng_plot)
plot(Ng_pop)
