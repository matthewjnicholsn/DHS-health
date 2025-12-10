#quick ordinary krige of chmort prob in eth
library(sf);library(sp);library(gstat);library(automap)
eth_outline <- st_read('/Users/matthewnicholson/eth_region.shp') |> 
  st_combine() |> 
  st_make_valid() |> 
  st_transform(eth_outline, crs = 32632)
gps_mort <- chmort_res_list[[2]][[5]] |> 
  st_as_sf() |> 
  st_transform(gps_mort,crs = 32632)

bbox <- st_bbox(eth_outline)
res <- 1000   # 10 km grid resolution (adjust as needed)
x.range <- seq(bbox$xmin, bbox$xmax, by = res)
y.range <- seq(bbox$ymin, bbox$ymax, by = res)
grid <- expand.grid(x = x.range, y = y.range)

grid_sf <- st_as_sf(grid, coords = c("x", "y"), crs = 32632)

# keep only grid points inside Nigeria
grid_sf <- grid_sf[eth_outline, ]
  
gps_mort <- as(gps_mort, "Spatial") |> 
  remove.duplicates()
  
grid_sp <- as(grid_sf, "Spatial")
  
gps_mort$X <- coordinates(gps_mort)[,1]
gps_mort$Y <- coordinates(gps_mort)[,2]
grid_sp$X <- coordinates(grid_sp)[,1]
grid_sp$Y <- coordinates(grid_sp)[,2]
  
#variogram and krigings
  
  
vgm_emp <- variogram(prob ~ 1, gps_mort)
vgm_fit <- fit.variogram(vgm_emp, model = vgm(c('Exp', 'Mat', 'Gau', 'Sph')))
  
plot(vgm_emp,vgm_fit)
  
krig_model <- gstat(formula = prob ~ 1,
                    data = gps_mort,
                    model = vgm_fit)
krig_predict <- predict(krig_model, grid_sp)

krig_predict_sf <- st_as_sf(krig_predict)
st_write(krig_predict_sf, dsn = 'eth_2019_chmort_krig.shp')

k_pred_df <- as.data.frame(krig_predict)
  coords <- coordinates(krig_predict)
  k_pred_df$x <- coords[,1]
  k_pred_df$y <- coords[,2]
  dim(k_pred_df)

ggplot() +
  geom_tile(data = k_pred_df, aes(x = x, y = y, fill = var1.pred)) +
  geom_sf(data = eth_outline, fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_viridis_c(name = "Predicted child mortality", option = "M")+
  coord_sf()+
  theme_minimal() +
    labs(title = paste("Ordinary Kriging predictions for child mortality probability Ethiopia 2019"))
#very uninformative result

#try autokrige to see if it fits a better empirical variogram
vgm_auto <- autofitVariogram(prob ~ 1, gps_mort)
auto_krig_pred <- autoKrige(prob ~ 1, gps_mort, grid_sp)

#convert to df for plot
auto_krig_df <- as.data.frame(auto_krig_pred$krige_output)
  coords <- coordinates(auto_krig_df)
  auto_krig_df$x <- coords[,1]
  auto_krig_df$y <- coords[,2]
  dim(auto_krig_df)

#plot
ggplot() +
  geom_tile(data = auto_krig_df, aes(x = x, y = y, fill = var1.pred)) +
  geom_sf(data = eth_outline, fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_viridis_c(name = "Predicted child mortality", option = "Viridis")+
  coord_sf()+
  theme_minimal() +
    labs(title = paste("Ordinary Kriging predictions for child mortality probability Ethiopia 2019"))
#also very inunfr