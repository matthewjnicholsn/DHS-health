library(dplyr)
library(ggplot2)
library(sf)


sf_lga <- read_sf("/Users/matthewnicholson/Downloads/nigeria-lgas/new_lga_nigeria_2003.shp")
sf <- read_sf("/Users/matthewnicholson/Downloads/GRID3_NGA_health_facilities_v2_0_-7782622226219335882/GRID3_NGA_health_facilities_v2_0.shp")
state_region_mapping <- read.csv("/Users/matthewnicholson/DHS/Nigeria States.csv")

sf_lga <- sf_lga %>%
  left_join(state_region_mapping, by = c("STATE" = "state"))

aggregated_data <- sf_lga %>%
  group_by(region) %>%
  summarize(geometry = st_union(geometry)) 
#subet sf based on public vs private health outposts
sf2 <- subset(sf, ownership == 'Public')
sf3 <- subset(sf, ownership == 'Private')

#plot public

plot2 <- ggplot() +
  geom_sf(data = sf2, color = "red", size = 0.05) +  # Plot the health outposts
  geom_sf(data = aggregated_data, fill = "lightblue", color = "black",  alpha = 0.10) +  # Plot the regions
  theme_void() +  
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Publicly-Owned Health Outposts and Regional Boundaries in Nigeria",
       x = "Longitude",
       y = "Latitude")
#plot private
plot1 <- ggplot() +
  geom_sf(data = sf3, color = "red", size = 0.05) +  # Plot the health outposts
  geom_sf(data = aggregated_data, fill = "lightblue", color = "black",  alpha = 0.10) +  # Plot the regions
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Privately-Owned Health Outposts and Regional Boundaries in Nigeria",
       x = "Longitude",
       y = "Latitude")

panel <- plot1 + plot2
print(panel)

#DHS spatial covariates
sf_sub <- read_sf("/Users/matthewnicholson/Downloads/sdr_subnational_data_2025-04-18/shps/sdr_subnational_data_dhs_2018_lvl_1.shp")
sf_sub2 <- read_sf("/Users/matthewnicholson/Downloads/sdr_subnational_data_2025-04-18/shps/sdr_subnational_data_dhs_2018_lvl_2.shp")


