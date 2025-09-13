library(haven)
library(dplyr)
library(forcats)
library(sf)
library(ggplot2)
library(viridis)
library(patchwork)
#load in household data
HRUR <- read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2018_DHS_04072025_2116_219655/NGHR7BDT/NGHR7BFL.DTA")
#load in individual data 
#read_dta("/Users/matthewnicholson/DHS/Chap8/NGIR7BFL.DTA")

#create subset with only region and wealth index
#create subset with only var of interest
HR_subUR <- HRUR[, c("hv024", "hv270a")]
#read in nigeria shapefile
HR_sfUR <- read_sf("/Users/matthewnicholson/Downloads/nigeria-lgas/new_lga_nigeria_2003.shp")



# Create a data frame mapping states to regions
state_region_mappingUR <- read.csv("/Users/matthewnicholson/DHS/Nigeria States.csv")

# Join the region information to the shapefile
HR_sfUR <- HR_sfUR %>%
  left_join(state_region_mappingUR, by = c("STATE" = "state"))

#check geom is valid
region_sfUR <- HR_sfUR %>%
  mutate(geometry = st_make_valid(geometry))

# Create a factor variable for region for merging
HR_subUR$hv024_factorUR <- factor(HR_subUR$hv024, 
                              levels = c(1, 2, 3, 4, 5, 6), 
                              labels = c("North Central", "North East", "North West", "South East", "South South", "South West"))


# Compute means of wealth index for each region using dplyr
mean_wealth_by_regionUR <- HR_subUR %>%
  group_by(hv024_factorUR) %>%
  summarise(Mean_Wealth_IndexUR = mean(hv270a, na.rm = TRUE))

##need now to join HR_sub data by region to HR_sf

HR_sfUR <- HR_sfUR %>%
  left_join(mean_wealth_by_regionUR, by = c("region" = "hv024_factorUR"))


#aggregated_data <- HR_sf %>%
#group_by(region) %>%
#summarize(geometry = st_union(geometry), .groups = 'drop') %>%
#left_join(HR_sf %>% select(region, Mean_Wealth_Index) %>% distinct(), by = "region")

# Aggregate the geometries by region and calculate the mean wealth index
aggregated_dataUR <- HR_sfUR %>%
  group_by(region) %>%
  summarize(geometry = st_union(geometry), 
            Mean_Wealth_IndexUR = mean(Mean_Wealth_IndexUR, na.rm = TRUE), 
            .groups = 'drop') 

# Ensure the geometries are valid
aggregated_dataUR <- aggregated_dataUR %>%
  mutate(geometry = st_make_valid(geometry))
sf_use_s2(FALSE)
# Calculate centroids for labeling
aggregated_dataUR <- aggregated_dataUR %>%
  mutate(centroid = st_centroid(geometry))



# TEST PLOT WITH EDITED COLOURS
aggregated_dataUR <- aggregated_dataUR %>%
  mutate(text_color = ifelse(region %in% c("North West", "North East"), "white", "black"))

# Plot the aggregated data with labels
ggplot(data = aggregated_dataUR) +
  geom_sf(aes(fill = Mean_Wealth_IndexUR), color = "white") +
  geom_sf_text(aes(label = region, geometry = centroid, color = text_color), 
               size = 3,  # Adjust the size as needed
               check_overlap = TRUE) +  # Temporarily set to FALSE to see all labels
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(option = "H", direction = 1) +  # Optional: use a color scale
  scale_color_identity() +  # Use the color defined in text_color
  labs(title = "Wealth Index by Geopolitical Region in Nigeria",
       fill = "Mean Wealth Index (1-5)")
