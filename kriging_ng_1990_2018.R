# R
library(sf)
library(sp)
library(gstat)
library(automap)
library(dplyr)
library(ggplot2)
library(viridis)
library(beepr)
# Settings
crs_proj <- 32632L        # projected CRS for Nigeria workflow
res <- 10000              # grid resolution in metres (10 km) ~ testing model with higher res...

# Read & prepare Nigeria outline (projected)
shp_file <- st_read("/Users/matthewnicholson/Downloads/nigeria_The_Federal_Republic_of_Nigeria_Country_Boundary/nigeria_The_Federal_Republic_of_Nigeria_Country_Boundary.shp", quiet = TRUE)
nigeria_outline <- shp_file |> st_union() |> st_make_valid()
nigeria_outline_proj <- st_transform(nigeria_outline, crs_proj)

# Build regular point grid (projected) and keep points inside Nigeria
bbox_proj <- st_bbox(nigeria_outline_proj)
x.range <- seq(bbox_proj$xmin, bbox_proj$xmax, by = res)
y.range <- seq(bbox_proj$ymin, bbox_proj$ymax, by = res)
grid_df <- expand.grid(x = x.range, y = y.range)
grid_sf_proj <- st_as_sf(grid_df, coords = c("x", "y"), crs = crs_proj)
inside_idx <- st_intersects(grid_sf_proj, nigeria_outline_proj, sparse = FALSE)[,1]
grid_sf_proj <- grid_sf_proj[inside_idx, , drop = FALSE]

# Convert to sp object used by gstat
grid_sp <- as(grid_sf_proj, "Spatial")

# Prepare output containers
vgm_fit_list <- vector("list", length(gps_list))
vgm_emp_list <- vector("list", length(gps_list))
krig_plot_list <- vector("list", length(gps_list))
krig_var_plot_list <- vector("list", length(gps_list))

for (i in seq_along(gps_list)) {
  # read gps, join mortality results, and prepare prob (per 1000)
  gps_file <- st_read(gps_list[[i]], quiet = TRUE) |>
    rename(cluster = DHSCLUST) |>
    select(cluster, geometry)
  dat <- chmortp_clust_results[[i]] |>
    left_join(gps_file, by = "cluster", relationship = "one-to-many") |>
    mutate(prob = as.numeric(as.character(prob)) * 1000)

  # to sf and project to same CRS used for grid
  dat_sf <- st_as_sf(dat)
  dat_sf <- st_transform(dat_sf, crs_proj)

  # to sp and remove duplicate coordinates
  dat_sp <- as(dat_sf, "Spatial")
  dat_sp <- sp::remove.duplicates(dat_sp)

  # add XY for convenience (optional)
  coords_dat <- coordinates(dat_sp)
  dat_sp@data$X <- coords_dat[,1]
  dat_sp@data$Y <- coords_dat[,2]

  # empirical variogram
  vgm_emp <- tryCatch(variogram(prob ~ 1, dat_sp), error = function(e) NULL)
  vgm_emp_list[[i]] <- vgm_emp

  # automap autofit (safe fallback)
  af <- tryCatch(
    automap::autofitVariogram(prob ~ 1, input_data = dat_sp, verbose = FALSE),
    error = function(e) NULL
  )

  if (!is.null(af)) {
    # automap result typically stores fitted variogram in $var_model or $var_model (compatibility checks)
    if ("var_model" %in% names(af)) {
      vgm_fit <- af$var_model
    } else if ("var.model" %in% names(af)) {
      vgm_fit <- af$var.model
    } else if ("model" %in% names(af)) {
      vgm_fit <- af$model
    } else {
      vgm_fit <- af
    }
  } else {
    # fallback: build a simple start model using sample variance & reasonable range
    samp_var <- stats::var(dat_sp@data$prob, na.rm = TRUE)
    start_psill <- max(samp_var * 0.7, 1e-6)
    start_nugget <- max(samp_var * 0.3, 1e-6)
    approx_range <- max(diff(x.range), diff(y.range)) / 6
    start_vgm <- gstat::vgm(psill = start_psill, model = "Exp", range = approx_range, nugget = start_nugget)
    vgm_fit <- tryCatch(fit.variogram(vgm_emp, start_vgm), error = function(e) start_vgm)
  }

  vgm_fit_list[[i]] <- vgm_fit

  # Kriging (ordinary)
  krig_model <- gstat::gstat(formula = prob ~ 1, data = dat_sp, model = vgm_fit)
  krig_predict <- predict(krig_model, grid_sp)

  # convert predictions to dataframe for plotting
  k_pred_df <- as.data.frame(krig_predict)
  coords_pred <- coordinates(krig_predict)
  k_pred_df$x <- coords_pred[,1]
  k_pred_df$y <- coords_pred[,2]

  # plots (geom_raster for regular grid)
  # predictions plot
krig_plot_list[[i]] <- ggplot(k_pred_df, aes(x = x, y = y, fill = var1.pred)) +
  geom_raster(interpolate = FALSE) +
  geom_sf(data = nigeria_outline_proj, inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_viridis_c(name = "Predicted (per 1000)", option = "viridis") +
  coord_sf(crs = st_crs(nigeria_outline_proj)) +
  theme_minimal() +
  labs(title = paste("Ordinary Kriging predictions —", year_list[[i]]))

krig_var_plot_list[[i]] <- ggplot(k_pred_df, aes(x = x, y = y, fill = var1.var)) +
  geom_raster(interpolate = FALSE) +
  geom_sf(data = nigeria_outline_proj, inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_viridis_c(name = "Prediction variance", option = "viridis") +
  coord_sf(crs = st_crs(nigeria_outline_proj)) +
  theme_minimal() +
  labs(title = paste("Kriging variance —", year_list[[i]]))
beep()
}

# return results
list(
  vgm_emp = vgm_emp_list,
  vgm_fit = vgm_fit_list,
  krig_plot_list = krig_plot_list,
  krig_var_plot_list = krig_var_plot_list
)

#plot
for(i in seq_along(krig_plot_list)){
  plot(krig_plot_list[[i]])
  plot(krig_var_plot_list[[i]])
}
