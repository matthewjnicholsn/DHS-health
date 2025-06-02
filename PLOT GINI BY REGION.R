#plot gini coefficient by region
library(haven)
library(dplyr)
library(forcats)
library(sf)
library(ggplot2)
library(viridis)
library(patchwork)
#read in the shape file for nigeria
sf <-  read_sf("/Users/matthewnicholson/Downloads/nigeria-lgas/new_lga_nigeria_2003.shp")
#read in the csv of states by region
regions <- read.csv("/Users/matthewnicholson/DHS/Nigeria States.csv")
#merge regions into sf
sf <- sf %>%
  left_join(regions, by = c("STATE" = "state"))

gini <- read.csv("/Users/matthewnicholson/DHS/Chap2/GINI_by_region.csv")
#merge gini into sf
sf <- sf %>%
  left_join(gini, by = c("region" = "region"))
#aggregate small geom
aggregated_data <- sf %>%
  group_by(region) %>%
  summarize(geometry = st_union(geometry),
            Mean_gini = mean(Gini, na.rm = TRUE), 
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
  geom_sf(aes(fill = Mean_gini), color = "white") +
  geom_sf_text(aes(label = region, geometry = centroid, color = text_color),
               size = 3,
               check_overlap = TRUE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_fill_viridis(option = "viridis", direction = -1) +
  scale_color_identity()+
  labs(title = "GINI coefficient by region",
       fill = "GINI coefficient")
  