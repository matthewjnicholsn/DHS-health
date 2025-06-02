# Load required libraries
library(haven)
library(dplyr)
library(forcats)
library(sf)
library(ggplot2)
library(viridis)
library(patchwork)
library(gstat)  # For Kriging
library(sp)     # For spatial data handling
library(tidyr)
# Load in the household data
NG08 <- read_dta("/Users/matthewnicholson/DHS/Nigeria DHS's/NG_2008_DHS_04072025_2113_219655/NGHR53DT/NGHR53FL.DTA")

# Read in the shape file
SF <- read_sf("/Users/matthewnicholson/Downloads/nigeria-lgas/new_lga_nigeria_2003.shp")

# New df with states encoded
state_mapping <- read.csv("/Users/matthewnicholson/Downloads/state encoding.csv")

# Subset data with only state, region, and wealth index
HR <- NG08[, c("hv024", "shstate", "hv270")]

#join state codes to shapefile
SF <- SF %>% 
  left_join(state_mapping, by = c('STATE' = 'STATE'))
# Calculate means of wealth index
WI <- HR %>% 
  group_by(shstate) %>% 
  summarise(WI = mean(hv270, na.rm = TRUE))

# Join WI to SF
WI <- SF %>% 
  left_join(WI, by = c("label" = "shstate"))
#replace lake chad na value

WI <- WI %>%
  mutate(
    WI = replace_na(WI, 0),
    label = replace_na(label, 0)
  ) %>%
  select(-LONGITUDE, -LATITUDE, -FULL_NAME)

# Aggregate LGA geometry to state
aggregate <- WI %>% 
  group_by(STATE) %>% 
  summarize(geometry = st_union(geometry),
            WI = mean(WI, na.rm = TRUE))

#This is where the code gets wonky
# Prepare for Kriging
# Create a data frame with coordinates and wealth index
coords <- st_coordinates(aggregate)  # Extract coordinates
coords_df <- data.frame(coords, WI = aggregate$WI)

# Create a spatial object for Kriging
coordinates(coords_df) <- ~ X1 + X2  # Set coordinates
proj4string(coords_df) <- st_crs(SF)$proj4string  # Set the same CRS as the shapefile

# Create a variogram
variogram_model <- variogram(WI ~ 1, data = coords_df)

# Fit a variogram model (using a spherical model as an example)
fitted_variogram <- fit.variogram(variogram_model, model = vgm("Sph", nugget = 0, psill = 1, range = 100))

# Create a prediction grid (adjust cellsize as needed)
grid <- st_make_grid(aggregate, cellsize = 0.1)  # Adjust cellsize for resolution
grid_sf <- st_as_sf(grid)

# Perform Kriging interpolation
kriging_result <- krige(WI ~ 1, locations = coords_df, newdata = grid_sf, model = fitted_variogram)

# Convert the result to an sf object for easier plotting
kriging_sf <- st_as_sf(kriging_result)

# Plot the Kriging result
ggplot() +
  geom_sf(data = kriging_sf, aes(fill = var1.pred)) +  # var1.pred contains the predicted values
  scale_fill_viridis_c(option = "C", direction = 1) +  # Use a color scale
  labs(title = "Kriging Interpolation of Wealth Index in Nigeria",
       fill = "Wealth Index") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'bottom')