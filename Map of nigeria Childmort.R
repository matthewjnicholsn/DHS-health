library(haven)
library(dplyr)
library(forcats)
library(sf)
library(ggplot2)
library(viridis)
library(patchwork)
library(beepr)
#read in the shape file for nigeria
sf <-  read_sf("/Users/matthewnicholson/Downloads/nigeria-lgas/new_lga_nigeria_2003.shp")
#read in the csv of states by region
regions <- read.csv("/Users/matthewnicholson/DHS/Nigeria States.csv")
#merge regions into sf
sf <- sf %>%
  left_join(regions, by = c("STATE" = "state"))

#make a data frame for CM means and regions
child_mortality_data <- data.frame(
  region = c("North Central", "North East", "North West", "South East", "South South", "South West"),
  mortality_rate = c(49.81273431, 68.53929725, 91.60724815, 31.42529937, 38.9635095, 33.69982967),
  label = c("North Central", "North East", "North West", "South East", "South South", "South West")
)
sf <- sf %>%
  left_join(child_mortality_data, by = "region")


#aggregate small geom
aggregated_data <- sf %>%
  group_by(region) %>%
  summarize(geometry = st_union(geometry),
            Mean_CM = mean(mortality_rate, na.rm = TRUE), 
            .groups = 'drop') 


# Turn off S2
sf_use_s2(FALSE)
#get centroids for labels
aggregated_data <- aggregated_data %>%
  mutate(centroid = st_centroid(geometry))

aggregated_data <- aggregated_data %>%
  mutate(text_color = ifelse(region %in% c("North West", "North East"), "white", "black"))


#create the plot with colours adjusted for contrasting labels
ggplot(data = aggregated_data) +
  geom_sf(aes(fill = Mean_CM), color = "white") +
  geom_sf_text(aes(label = region, geometry = centroid, color = text_color),
               size = 3,
               check_overlap = TRUE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_fill_viridis(option = "viridis", direction = -1) +
  scale_color_identity()+
  labs(title = "Under 9 Mortality by Geopolitical Region in Nigeria",
       fill = "Mortality per 1000")
