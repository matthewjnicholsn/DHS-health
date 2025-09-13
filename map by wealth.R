library(haven)
library(dplyr)
library(forcats)
library(sf)
library(ggplot2)
library(viridis)
library(patchwork)
#load in household data
HR <- read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2018_DHS_04072025_2116_219655/NGHR7BDT/NGHR7BFL.DTA")
#load in individual data 
#read_dta("/Users/matthewnicholson/DHS/Chap8/NGIR7BFL.DTA")

#create subset with only region and wealth index
#create subset with only var of interest
HR_sub <- HR[, c("hv024", "hv270")]
#read in nigeria shapefile
HR_sf <- read_sf("/Users/matthewnicholson/Downloads/nigeria-lgas/new_lga_nigeria_2003.shp")

# Create a data frame mapping states to regions
state_region_mapping <- read.csv("/Users/matthewnicholson/DHS/Nigeria States.csv")

# Join the region information to the shapefile
HR_sf <- HR_sf %>%
  left_join(state_region_mapping, by = c("STATE" = "state"))

#check geom is valid
region_sf <- HR_sf %>%
  mutate(geometry = st_make_valid(geometry))

# Create a factor variable for region for mergine
HR_sub$hv024_factor <- factor(HR_sub$hv024, 
                              levels = c(1, 2, 3, 4, 5, 6), 
                              labels = c("North Central", "North East", "North West", "South East", "South South", "South West"))

# Check the structure of the new factor variable
str(HR_sub$hv024_factor)

# Compute means of wealth index for each region using dplyr
mean_wealth_by_region <- HR_sub %>%
  group_by(hv024_factor) %>%
  summarise(Mean_Wealth_Index = mean(hv270, na.rm = TRUE))

##need now to join HR_sub data by region to HR_sf

HR_sf <- HR_sf %>%
  left_join(mean_wealth_by_region, by = c("region" = "hv024_factor"))


#aggregated_data <- HR_sf %>%
#group_by(region) %>%
#summarize(geometry = st_union(geometry), .groups = 'drop') %>%
#left_join(HR_sf %>% select(region, Mean_Wealth_Index) %>% distinct(), by = "region")

# Aggregate the geometries by region and calculate the mean wealth index
aggregated_data <- HR_sf %>%
  group_by(region) %>%
  summarize(geometry = st_union(geometry), 
            Mean_Wealth_Index = mean(Mean_Wealth_Index, na.rm = TRUE), 
            .groups = 'drop') 

# Ensure the geometries are valid
aggregated_data <- aggregated_data %>%
  mutate(geometry = st_make_valid(geometry))

# Calculate centroids for labeling
aggregated_data <- aggregated_data %>%
  mutate(centroid = st_centroid(geometry))


# TEST PLOT WITH EDITED COLOURS
aggregated_data <- aggregated_data %>%
  mutate(text_color = ifelse(region %in% c("North West", "North East"), "white", "black"))

# Plot the aggregated data with labels
ggplot(data = aggregated_data) +
  geom_sf(aes(fill = Mean_Wealth_Index), color = "white") +
  geom_sf_text(aes(label = region, geometry = centroid, color = text_color), 
               size = 3,  # Adjust the size as needed
               check_overlap = TRUE) +  # Temporarily set to FALSE to see all labels
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(option = "mako", direction = 1) +  # Optional: use a color scale
  scale_color_identity() +  # Use the color defined in text_color
  labs(title = "Wealth Index by Region",
       fill = "Mean Wealth Index")

##now we do it for mort



child_mortality_data <- data.frame(
  region = c("North Central", "North East", "North West", "South East", "South South", "South West"),
  mortality_rate = c(49.81273431, 68.53929725, 91.60724815, 31.42529937, 38.9635095, 33.69982967),
  label = c("North Central", "North East", "North West", "South East", "South South", "South West")
)

region_sf <- HR_sf %>%
  left_join(child_mortality_data, by = "region")

# Plot the regions with childhood mortality rates
ggplot(region_sf) +
  geom_sf(aes(fill = mortality_rate), color = "white") +  # Map fill to mortality rate
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Under 9 Mortality by Geopolitical Unit in Nigeria",
       fill = "Mortality per 1000") +  # Add a title and legend label
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(option = "mako", direction = -1)  # Use a color scale (install 'viridis' package if needed)



