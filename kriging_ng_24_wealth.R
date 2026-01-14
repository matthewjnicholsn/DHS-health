rm(list=ls())
lapply(c("dplyr","haven","sf","sp","gstat","ggplot2"),require,character.only=T)
ng_24 <- readRDS("/Users/matthewnicholson/DHS/DHS_surveys_rds_organized/Nigeria/2024/NGHR8BDT/Nigeria_DHS_NG_2024_DHS_12192025_2031_219655_NGHR8BDT_NGHR8BFL.rds")
ng_24_gps <- st_read("/Users/matthewnicholson/Downloads/NG_2024_spatial/NGGE8AFL/NGGE8AFL.shp")

ng_24_sub <- ng_24 %>% 
  select(hv001,hv270) %>% 
  rename(cluster = hv001,
         wealth = hv270)

ng_gps_sub <- ng_24_gps %>% 
  select(DHSCLUST,geometry) %>% 
  rename(cluster = DHSCLUST) %>% 
  left_join(ng_24_sub, by = "cluster") %>% 
  st_as_sf() %>% 
  st_transform(32632)

nigeria_outline <- st_read("/Users/matthewnicholson/Downloads/nigeria-lgas/new_lga_nigeria_2003.shp") |> 
  st_union() |> st_make_valid() |> st_transform(32632) 

bbox <- st_bbox(nigeria_outline)
res <- 1000   # cell size in meters 

x.range <- seq(bbox$xmin, bbox$xmax, by = res)
y.range <- seq(bbox$ymin, bbox$ymax, by = res)
grid <- expand.grid(x = x.range, y = y.range)

grid_sf <- st_as_sf(grid, coords = c("x", "y"), crs = 32632)
grid_sp <- as(grid_sf, "Spatial")

# convert cluster data back to spatial
res_clust_sp <- as(ng_gps_sub, "Spatial")
#find and remove zero distance points
zd <- zerodist(res_clust_sp)
res_clust_sp <- res_clust_sp[-zd[,1],]

# add coords for variogram/kriging
res_clust_sp$X <- coordinates(res_clust_sp)[,1]
res_clust_sp$Y <- coordinates(res_clust_sp)[,2]
grid_sp$X <- coordinates(grid_sp)[,1]
grid_sp$Y <- coordinates(grid_sp)[,2]

# variogram + kriging
vgm_emp <- variogram(wealth ~ 1, res_clust_sp)
vgm_fit <- fit.variogram(vgm_emp, model = vgm(c("Exp","Mat","Gau","Sph")))
plot(vgm_emp,vgm_fit)
k_model <- gstat(formula = wealth ~ 1,
                 data = res_clust_sp,
                 model = vgm_fit)
k_pred <- krige(wealth ~ 1, res_clust_sp, grid_sp, model = vgm_fit, maxdist = 4.5e5)


beep()

# convert predictions to df for ggplot
k_pred_df <- as.data.frame(k_pred)
# st_write(k_pred_df, "nigeria_2024_wealth_kriged.shp")
coords <- coordinates(k_pred) 
  

k_pred_df$x <- coords[,1]
k_pred_df$y <- coords[,2]

# check size before plotting
dim(k_pred_df)

# plot with geom_tile (handles irregular spacing better than geom_raster)
ggplot(k_pred_df, aes(x = x, y = y, fill = var1.pred)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Predicted Wealth", option = "F") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Ordinary kriging: Weath 2024")

#export shape file
k_pred_df <- st_as_sf(k_pred_df,coords = c("x","y"))
st_write(k_pred_df,dsn = "nigeria_2024_wealth_krig.shp")
