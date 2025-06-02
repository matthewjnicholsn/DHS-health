library(sf)
library(ggplot2)
library(dplyr)
my_sf5 <- read_sf("/Users/matthewnicholson/Downloads/nigeria-lgas/new_lga_nigeria_2003.shp")



# Create a data frame mapping states to regions
state_region_mapping <- read.csv("/Users/matthewnicholson/DHS/Nigeria States.csv")

# Join the region information to your shapefile
my_sf5 <- my_sf5 %>%
  left_join(state_region_mapping, by = c("STATE" = "state"))

region_sf2 <- my_sf5 %>%
  mutate(geometry = st_make_valid(geometry))



# Plot the regions with labels
ggplot(region_sf2) +
  geom_sf(aes(fill = region), color = "white") +  # Map fill to region
  theme_void() +
  labs(title = "Regions of Nigeria") +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette
  theme(legend.position = "right")  # Optional: adjust legend position