library(sf)
library(ggplot2)
library(viridis)
library(haven)
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)
library(sf)
library(sp)
library(gstat)
library(raster)
library(ggplot2)
library(viridis)
lib_list <- c("sf", "ggplot2", "viridis", "haven", "dplyr", "sf", "ggplot2",
              "viridis", "sf", "sp", "gstat", "raster", "ggplot2", "viridis") %>% 
  unique(lib_list)
et_sf <- read_sf("/Users/matthewnicholson/DHS/GPS files/Ethiopia/2000/ETGE42FL/ETGE42FL.shp")
et_hr <- readRDS(file = "/Users/matthewnicholson/DHS/DHS_surveys_rds_organized/Ethiopia_DHS/2000/ETHR41DT/Ethiopia_DHS_ET_2000_DHS_08072025_1921_219655_ETHR41DT_ETHR41FL.Rds")
et_wi <- as_tibble(read_dta("/Users/matthewnicholson/DHS/DHS_surveys/Ethiopia_DHS/ET_2000_DHS_08072025_1921_219655/ETWI41DT/ETWI41FL.DTA"))
View(et_sf)
View(et_hr)

et_hr <- et_hr %>% 
  rename(DHSCLUST = hv001) %>% 
  rename(whhid = hhid) %>% 
  left_join(et_wi %>% distinct(whhid, wlthind5), join_by(whhid),
            relationship = "many-to-many") %>% 
  left_join(et_sf %>% distinct(DHSCLUST, geometry), join_by(DHSCLUST), 
            relationship = "many-to-many") 

wealth_cluster <- aggregate(wlthind5 ~ DHSCLUST, data = et_hr, FUN = mean) %>% 
  left_join(wealth_cluster, et_sf, by = "DHSCLUST")
class(wealth_cluster)

wealth_cluster <- st_as_sf(wealth_cluster)


ggplot(data = wealth_cluster) +
  geom_sf(aes(color = wlthind5.x), size = 3) +
  scale_color_viridis(option = "viridis", name = "Mean Wealth Index") +
  theme_minimal() +
  labs(title = "Cluster-level Mean Wealth Index (Ethiopia 2000)")


# Assume wealth_cluster is already created as sf
wealth_cluster_sp <- as(wealth_cluster, "Spatial")

# 1. Get CRS from clusters
crs_clusters <- st_crs(wealth_cluster)

# 2. Create grid covering extent
bbox <- st_bbox(wealth_cluster)
x.range <- seq(bbox$xmin, bbox$xmax, length.out = 100)
y.range <- seq(bbox$ymin, bbox$ymax, length.out = 100)
grid <- expand.grid(x = x.range, y = y.range)

# 3. Assign CRS to grid and convert to sp
grid_sf <- st_as_sf(grid, coords = c("x", "y"), crs = crs_clusters)
grid_sp <- as(grid_sf, "Spatial")

# 4. Ensure CRS matches
st_crs(wealth_cluster) == st_crs(grid_sf) # Should be TRUE

# 5. Kriging
vgm_emp <- variogram(wlthind5.x ~ 1, data = wealth_cluster_sp)
plot(vgm_emp)

# Remove outliers (e.g., lag distances above a chosen threshold)
vgm_emp_clean <- subset(vgm_emp, np >= 30)
vgm_emp_clean <- subset(vgm_emp, dist <= 900)

# Suppose vgm_emp_clean is your empirical variogram data.frame
fit_parab <- lm(gamma ~ dist + I(dist^2), data = vgm_emp_clean)

# Extract coefficients
coef(fit_parab)  # a, b, c

# Generate fitted values for plotting
dist_seq <- seq(min(vgm_emp_clean$dist), max(vgm_emp_clean$dist), length.out=100)
parab_fit <- coef(fit_parab)[1] + coef(fit_parab)[2]*dist_seq + coef(fit_parab)[3]*dist_seq^2

# Plot
plot(vgm_emp_clean$dist, vgm_emp_clean$gamma, pch=21, col="blue",
     xlab="distance", ylab="semivariance", main="Parabolic Fit")
lines(dist_seq, parab_fit, col="purple", lwd=2)



kriging_result <- krige(wlthind5.x ~ 1, wealth_cluster_sp, grid_sp, model = vgm_fit)


duplicated_coords <- duplicated(st_coordinates(wealth_cluster))
table(duplicated_coords)
unique(wealth_cluster$geometry)
# Convert SpatialPixelsDataFrame to sf
kriged_sf <- st_as_sf(as(kriging_result, "SpatialPixelsDataFrame"))

# Plot with ggplot2
ggplot(kriged_sf) +
  geom_sf(aes(fill = var1.pred)) +
  scale_fill_viridis(option = "viridis", name = "Kriged Wealth Index") +
  theme_minimal() +
  labs(title = "Kriged Mean Wealth Index (Ethiopia 2000)")