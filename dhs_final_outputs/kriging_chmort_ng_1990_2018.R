library(sf)
library(sp)
library(gstat)
chmortp_clust_results <- vector("list", 5)
chmort_files <- c("/Users/matthewnicholson/DHS/Tables_child_mort_by_cluster_year1990.csv",
                   "/Users/matthewnicholson/DHS/Tables_child_mort_by_cluster_year2003.csv",
                   "/Users/matthewnicholson/DHS/Tables_child_mort_by_cluster_year2008.csv",
                   "/Users/matthewnicholson/DHS/Tables_child_mort_by_cluster_year2013.csv",
                  "/Users/matthewnicholson/DHS/Tables_child_mort_by_cluster_year2018.csv")
for(i in seq_along(countries)){
  chmortp_clust_results[[i]] <- read.csv(chmort_files[[i]])
}
vgm_plot_list <- list()
krig_plot_list <- list()
krig_var_plot_list <- list()
shp_file <- st_read("/Users/matthewnicholson/Downloads/nigeria_The_Federal_Republic_of_Nigeria_Country_Boundary/nigeria_The_Federal_Republic_of_Nigeria_Country_Boundary.shp")
nigeria_outline <- shp_file |> st_union() |> st_make_valid() |> st_transform(32632)
gps_list <- c('/Users/matthewnicholson/DHS/GPS files/Nigeria/1990/NGGE23FL/NGGE23FL.shp',
              '/Users/matthewnicholson/DHS/GPS files/Nigeria/2003/NGGE4BFL/NGGE4BFL.shp',
              '/Users/matthewnicholson/DHS/GPS files/Nigeria/2008/NGGE52FL/NGGE52FL.shp',
              '/Users/matthewnicholson/DHS/GPS files/Nigeria/2013/NGGE6AFL/NGGE6AFL.shp',
              '/Users/matthewnicholson/DHS/GPS files/Nigeria/2018/NGGE7BFL/NGGE7BFL.shp')

#need to re-write to sit in the j-loop over years
#also pre-configure vectors with length so subscript [[i]] is in-bounds
gps_mort <- list()
for(i in seq_along(gps_list)){
  gps_file <- st_read(gps_list[[i]]) |> 
    rename(cluster = DHSCLUST) |> 
    select(cluster,geometry)
  gps_mort[[i]] <- chmortp_clust_results[[i]] |> 
    left_join(gps_file, by = "cluster", relationship = "one-to-many") |> 
    mutate(prob = as.numeric(as.character(prob)) * 1000)


#kriging
gps_mort[[i]] <- gps_mort[[i]] |> 
  st_as_sf()

gps_mort[[i]] <- st_transform(gps_mort[[i]],32632)
# project to CRS

# create grid inside bounding box
bbox <- st_bbox(nigeria_outline)
res <- 10000   # 10 km grid resolution (adjust as needed)
x.range <- seq(bbox$xmin, bbox$xmax, by = res)
y.range <- seq(bbox$ymin, bbox$ymax, by = res)
grid <- expand.grid(x = x.range, y = y.range)

grid_sf <- st_as_sf(grid, coords = c("x", "y"), crs = 32632)

# keep only grid points inside Nigeria
grid_sf <- grid_sf[nigeria_outline, ]
  
gps_mort[[i]] <- as(gps_mort[[i]], "Spatial") |> 
  remove.duplicates()
  
grid_sp <- as(grid_sf, "Spatial")
  
gps_mort[[i]]$X <- coordinates(gps_mort[[i]])[,1]
gps_mort[[i]]$Y <- coordinates(gps_mort[[i]])[,2]
grid_sp$X <- coordinates(grid_sp)[,1]
grid_sp$Y <- coordinates(grid_sp)[,2]
  
#variogram and krigings
  
  
vgm_emp <- variogram(prob ~ 1, gps_mort[[i]])
vgm_fit <- fit.variogram(vgm_emp, model = vgm(c('Exp', 'Mat', 'Gau', 'Sph')))
  
vgm_plot_list[[i]] <- plot(vgm_emp,vgm_fit)
  
krig_model <- gstat(formula = prob ~ 1,
                    data = gps_mort[[i]],
                    model = vgm_fit)
krig_predict <- predict(krig_model, grid_sp)

  
#convert predictions to plot
  k_pred_df <- as.data.frame(krig_predict)
  coords <- coordinates(krig_predict)
  k_pred_df$x <- coords[,1]
  k_pred_df$y <- coords[,2]
  dim(k_pred_df)

#plots
krig_plot_list[[i]] <- ggplot() +
  geom_tile(data = k_pred_df, aes(x = x, y = y, fill = var1.pred)) +
  geom_sf(data = nigeria_outline, fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_viridis_c(name = "Predicted child mortality", option = "M")+
  coord_sf()
  theme_minimal() +
    labs(title = paste("Ordinary Kriging predictions for child mortality probability per 1000 child births Nigeria", year_list[[i]]))

krig_var_plot_list[[i]] <- ggplot() +
  geom_tile(data = k_pred_df, aes(x = x, y = y, fill = var1.var)) +
  geom_sf(data = nigeria_outline, fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_viridis_c(name = "Variance of Predicted child mortality", option = "M")+
  coord_sf()
  theme_minimal() +
    labs(title = paste("Ordinary Kriging variance for child mortality probability per 1000 child births Nigeria", year_list[[i]]))
  
}
for(i in seq_along(krig_plot_list)){
  plot(krig_plot_list[[i]])
}
