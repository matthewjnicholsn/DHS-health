lib_list <- c("sp","sf", "gstat", "ggplot2", "viridis", "haven", "dplyr")
lapply(lib_list, require, character.only = T)

# load in our data
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
# we get all of the files so that we can get the variograms for all years eventually, or conversely do a spatio-temporal kriging
# first we try one year, 1990
# load the gps file first
sf_1990 <- st_read(st_file_list[1]) |>
  dplyr::select(DHSCLUST, geometry, DHSREGCO) |>
  dplyr::rename(cluster = DHSCLUST, region = DHSREGCO)
#then the data file, as it is a long pipeline reliant on the sf being in the environment
data_1990 <- readRDS(ir_file_list[1]) |>
  dplyr::select(v001, v106, v005, v024) |>
  dplyr::rename(cluster = v001, education = v106, wt = v005, region = v024) |>
  dplyr::mutate(
    education = dplyr::na_if(education, 9),
    wt = as.numeric(wt) / 1e6
  ) |>
  dplyr::mutate(
    education = as.numeric(haven::as_factor(education, levels = "default"))
  ) |>
  dplyr::group_by(cluster) |> 
  dplyr::summarise(
    weighted_education = sum(wt * education, na.rm = TRUE) / sum(wt[!is.na(education)], na.rm = TRUE),
    unweighted_n = dplyr::n(),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  dplyr::left_join(sf_1990, by = "cluster")

#split data by region
re_1_data_1990 <- data_1990 |> 
filter(region == 1) |> st_as_sf()
re_2_data_1990 <- data_1990 |> 
  filter(region == 2)|> st_as_sf()
re_3_data_1990 <- data_1990 |> 
  filter(region ==3)|> st_as_sf()
re_4_data_1990 <- data_1990 |> 
  filter(region == 4)|> st_as_sf()
#r1 plot
ggplot(data = re_1_data_1990) +
  geom_sf(aes(color = weighted_education), size = 3) +
  scale_color_viridis(option = "viridis", name = "Mean education") +
  theme_minimal() +
  labs(title = "Cluster-level Mean education (R1 Nigeria 1990)")
#r2 plot
ggplot(data = re_2_data_1990) +
  geom_sf(aes(color = weighted_education), size = 3) +
  scale_color_viridis(option = "viridis", name = "Mean education") +
  theme_minimal() +
  labs(title = "Cluster-level Mean education (R2 Nigeria 1990)")

#r3 plot
ggplot(data = re_3_data_1990) +
  geom_sf(aes(color = weighted_education), size = 3) +
  scale_color_viridis(option = "viridis", name = "Mean education") +
  theme_minimal() +
  labs(title = "Cluster-level Mean education (R3 Nigeria 1990)")

# r4 plot
ggplot(data = re_4_data_1990) +
  geom_sf(aes(color = weighted_education), size = 3) +
  scale_color_viridis(option = "viridis", name = "Mean education") +
  theme_minimal() +
  labs(title = "Cluster-level Mean education (R4 Nigeria 1990)")

var(re_1_data_1990$weighted_education) #highest variance
var(re_2_data_1990$weighted_education) #second highest variance
var(re_3_data_1990$weighted_education) # third highest variance
var(re_4_data_1990$weighted_education) #lowest variance

#will try r1 first
# get CRS from clusters
crs_clusters <- st_crs(re_1_data_1990)

# 2. Create grid covering extent
bbox <- st_bbox(re_1_data_1990)
x.range <- seq(bbox$xmin, bbox$xmax, length.out = 100)
y.range <- seq(bbox$ymin, bbox$ymax, length.out = 100)
grid <- expand.grid(x = x.range, y = y.range)

# 3. Assign CRS to grid and convert to sp
grid_sf <- st_as_sf(grid, coords = c("x", "y"), crs = crs_clusters)
grid_sp <- as(grid_sf, "Spatial")

# reproject your sf data and grid
re_1_data_1990 <- st_transform(re_1_data_1990, 32632) #this is approximately the right projection
grid_sf   <- st_transform(grid_sf, 32632)


# Back to Spatial
re_1_data_1990_sp <- as(re_1_data_1990, "Spatial")
grid_sp      <- as(grid_sf, "Spatial")

# Add projected coords for trend
re_1_data_1990_sp$X <- coordinates(re_1_data_1990_sp)[,1]
re_1_data_1990_sp$Y <- coordinates(re_1_data_1990_sp)[,2]
grid_sp$X      <- coordinates(grid_sp)[,1]
grid_sp$Y      <- coordinates(grid_sp)[,2]

# Variogram + ordinary kriging
vgm_emp <- variogram(weighted_education ~ 1, re_1_data_1990_sp)  # ~1 = ordinary kriging
plot(vgm_emp)
vgm_fit <- fit.variogram(vgm_emp, model = vgm("Sph"))

plot(vgm_emp,vgm_fit)

k_model <- gstat(formula = weighted_education ~ 1,
                  data = re_1_data_1990_sp,
                  model = vgm_fit)

k_pred <- predict(k_model, grid_sp)   # should now work

ggplot() +
  geom_sf(data = k_pred, aes(fill = var1.pred), color = NA) +
  scale_fill_viridis_c(name = "Predicted education") +
  theme_minimal() +
  labs(title = "Universal Kriging: Educational Attainment (Nigeria, 1990)")

