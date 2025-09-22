lib_list <- c("sp", "gstat", "raster", "spacetime")
lapply(lib_list, require, character.only = T)
source("/Users/matthewnicholson/DHS/get_file_function.R")
year_list <- c(1990,2003,2008,2010,2013,2015,2018,2021)
st_file_list <- c(
 '/Users/matthewnicholson/DHS/GPS files/Nigeria/1990/NGGE23FL/NGGE23FL.shp',
  '/Users/matthewnicholson/DHS/GPS files/Nigeria/2003/NGGE4BFL/NGGE4BFL.shp',
   '/Users/matthewnicholson/DHS/GPS files/Nigeria/2008/NGGE52FL/NGGE52FL.shp',
    '/Users/matthewnicholson/DHS/GPS files/Nigeria/2010/NGGE61FL/NGGE61FL.shp',
     '/Users/matthewnicholson/DHS/GPS files/Nigeria/2013/NGGE6AFL/NGGE6AFL.shp',
      '/Users/matthewnicholson/DHS/GPS files/Nigeria/2015/NGGE71FL/NGGE71FL.shp',
       '/Users/matthewnicholson/DHS/GPS files/Nigeria/2018/NGGE7BFL/NGGE7BFL.shp',
        '/Users/matthewnicholson/DHS/GPS files/Nigeria/2021/NGGE81FL/NGGE81FL.shp'
      )
ir_file_list <- get_file("Nigeria_DHS", year_list, "IR")

#check equality of clusters between years
# compare consecutive pairs; stop at length-1 to avoid i+1 out-of-bounds
for(i in seq_len(length(st_file_list) - 1)) {
  st1 <- st_read(st_file_list[[i]], quiet = TRUE)
  st2 <- st_read(st_file_list[[i + 1]], quiet = TRUE)

  # make sure geometry columns exist and are comparable
  g1 <- st_geometry(st1)
  g2 <- st_geometry(st2)

  # normalize geometry types / order if needed by converting to WKB for exact match
  wkb1 <- st_as_binary(g1)
  wkb2 <- st_as_binary(g2)

  if(!identical(wkb1, wkb2)) {
    message(sprintf("Geometry differs between %s and %s (year %s -> %s)",
                    basename(st_file_list[[i]]),
                    basename(st_file_list[[i+1]]),
                    year_list[[i]],
                    year_list[[i+1]]))
  } else {
    message(sprintf("Geometry matches for %s and %s (year %s -> %s)",
                    basename(st_file_list[[i]]),
                    basename(st_file_list[[i+1]]),
                    year_list[[i]],
                    year_list[[i+1]]))
  }
}
#geometries all differ

#check if clusters differ
for(i in seq_len(length(st_file_list) - 1)) {
  st1 <- st_read(st_file_list[[i]], quiet = TRUE)
  st2 <- st_read(st_file_list[[i + 1]], quiet = TRUE)

  # make sure geometry columns exist and are comparable
  cluster1 <- as.numeric(st1$DHSCLUST)
  cluster2 <- as.numeric(st2$DHSCLUST)

  if(!identical(cluster1, cluster2)) {
    message(sprintf("Clusters differs between %s and %s (year %s -> %s)",
                    basename(st_file_list[[i]]),
                    basename(st_file_list[[i+1]]),
                    year_list[[i]],
                    year_list[[i+1]]))
  } else {
    message(sprintf("Clusters matches for %s and %s (year %s -> %s)",
                    basename(st_file_list[[i]]),
                    basename(st_file_list[[i+1]]),
                    year_list[[i]],
                    year_list[[i+1]]))
  }
}

#test for one year
data_1990 <- readRDS(ir_file_list[1]) |> 
  dplyr::select(v001, v106,v005) |> 
  dplyr::rename(cluster = v001, education = v106, wt = v005) |> 
  dplyr::mutate(education = dplyr::na_if(education, 9),  # convert 9 -> NA
      wt = wt / 1e6 ) |>
    dplyr::group_by(cluster) |>
    dplyr::summarize(
      weighted_education = sum(wt * education, na.rm = TRUE) / sum(wt[!is.na(education)], na.rm = TRUE),
      unweighted_n = dplyr::n(),
      total_weight = sum(wt, na.rm = TRUE),
      .groups = "drop"
    )

sf_1990 <- st_read(st_file_list[1]) |> 
  dplyr::select(DHSCLUST,geometry) |> 
  dplyr::rename(cluster = DHSCLUST) 

data_1990 <- data_1990 |> 
  dplyr::left_join(sf_1990, by = "cluster")
data_1990 <- st_as_sf(data_1990)
#take a peak at the data
ggplot(data = data_1990) +
  geom_sf(aes(color = weighted_education), size = 3) +
  scale_color_viridis(option = "viridis", name = "Mean education") +
  theme_minimal() +
  labs(title = "Cluster-level Mean education (Nigeria 1990)")

# Assume data_1990 is already created as sf
data_1990 <- as(data_1990, "Spatial")

# 1. Get CRS from clusters
crs_clusters <- st_crs(data_1990)

# 2. Create grid covering extent
bbox <- st_bbox(data_1990)
x.range <- seq(bbox$xmin, bbox$xmax, length.out = 100)
y.range <- seq(bbox$ymin, bbox$ymax, length.out = 100)
grid <- expand.grid(x = x.range, y = y.range)

# 3. Assign CRS to grid and convert to sp
grid_sf <- st_as_sf(grid, coords = c("x", "y"), crs = crs_clusters)
grid_sp <- as(grid_sf, "Spatial")

# 4. Ensure CRS matches
st_crs(data_1990) == st_crs(grid_sf) # Should be TRUE

# 5. Kriging
vgm_emp <- variogram(weighted_education~ 1, data = data_1990)
plot(vgm_emp)

# Remove outliers (e.g., lag distances above a chosen threshold)
vgm_emp_clean <- subset(vgm_emp, np >= 30)
vgm_emp_clean <- subset(vgm_emp, dist <= 900)

# Suppose vgm_emp_clean is your empirical variogram data.frame
fit_vgm <- fit.variogram(vgm_emp, vgm("Exp"))

# Extract coefficients
coef(fit_vgm)  # a, b, c

# Generate fitted values for plotting
dist_seq <- seq(min(vgm_emp$dist), max(vgm_emp$dist), length.out=100)
vgm_fit <- coef(fit_vgm)[1] + coef(fit_vgm)[2]*dist_seq + coef(fit_vgm)[3]*dist_seq^2

# Plot
plot(vgm_emp,model = vgm_fit)

plot(vgm_emp$dist, vgm_emp$gamma, pch=21, col="blue",
     xlab="distance", ylab="semivariance", main="spherical Fit")
lines(dist_seq, vgm_fit, col="purple", lwd=2)



kriging_result <- krige(ALT_DEM ~ 1, et_sf, grid_sp, model = vgm_fit)


duplicated_coords <- duplicated(st_coordinates(data_1990))
table(duplicated_coords)
unique(data_1990$geometry)
# Convert SpatialPixelsDataFrame to sf
kriged_sf <- st_as_sf(as(kriging_result, "SpatialPixelsDataFrame"))

# Plot with ggplot2
ggplot(kriged_sf) +
  geom_sf(aes(fill = var1.pred)) +
  scale_fill_viridis(option = "viridis", name = "Kriged Wealth Index") +
  theme_minimal() +
  labs(title = "Kriged Mean Wealth Index (Ethiopia 2000)")
