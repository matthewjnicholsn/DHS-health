
for(j in seq_along(years[[i]])){
  tryCatch({
    message("Pre-processing for chmort kriging ", countries[[i]], " ", years[[i]][j])
  gps_file <- st_read(gps_file_list[[i]][j])
  if(is.null(gps_file) || nrow(gps_file) == 0){
      warning("gps file is empty for ", countries[[i]], " ", years[[i]][j])
    }

  crs_proj <- st_crs(gps_file)
  outline_file <- st_read(outline_file_list[[i]]) |> 
    st_union() |> 
    st_make_valid() |> 
    st_transform(
      # outline_file, 
      crs = crs_proj)
  },
  error = function(e){

    message("Error in chmort kriging for ", countries[[i]], " ", years[[i]][j])
    message("Message: ", e$message)

    cat("Country:", countries[[i]], "\n",
        "Year:", years[[i]][j], "\n",
        "Error:", e$message, "\n\n",
        file = "chmort_krig_error_log.txt",
        append = TRUE)
  })
  #set resolution
  res <- 10000    
  # build our point grid (projected) and keep points inside outline
  bbox_proj <- st_bbox(outline_file)
  x.range <- seq(bbox_proj$xmin, bbox_proj$xmax, by = res)
  y.range <- seq(bbox_proj$ymin, bbox_proj$ymax, by = res)
  grid_df <- expand.grid(x = x.range, y = y.range)
  grid_sf_proj <- st_as_sf(grid_df, coords = c("x", "y"), crs = crs_proj)
  inside_idx <- st_intersects(grid_sf_proj, outline_file, sparse = FALSE)[,1]
  grid_sf_proj <- grid_sf_proj[inside_idx, , drop = FALSE]
  # Convert to sp object used by gstat
  grid_sp <- as(grid_sf_proj, "Spatial")

  # read gps, join mortality results, and prepare prob (per 1000)
  gps_file <- gps_file |>
    rename(cluster = DHSCLUST) |>
    select(cluster, geometry)
  dat <- read.csv(chmort_file_list[[i]][j]) |>
    left_join(gps_file, by = "cluster") |>
    mutate(prob = as.numeric(as.character(prob)) * 1000)

  # to sf and project to same CRS used for grid
  dat_sf <- st_as_sf(dat)
  dat_sf <- st_transform(dat_sf, crs = crs_proj)

  # to sp and remove duplicate coordinates
  dat_sp <- as(dat_sf, "Spatial")
  dat_sp <- sp::remove.duplicates(dat_sp)

  # add XY for convenience (optional)
  coords_dat <- coordinates(dat_sp)
  dat_sp@data$X <- coords_dat[,1]
  dat_sp@data$Y <- coords_dat[,2]

  # empirical variogram
  vgm_emp <- tryCatch(variogram(prob ~ 1, dat_sp), error = function(e) NULL)
  vgm_emp_list[[i]][j] <- vgm_emp

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

  vgm_fit_list[[i]][j] <- vgm_fit

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
krig_plot_list[[i]][j] <- ggplot(k_pred_df, aes(x = x, y = y, fill = var1.pred)) +
  geom_raster(interpolate = FALSE) +
  geom_sf(data = nigeria_outline_proj, inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_viridis_c(name = "Predicted (per 1000)", option = "viridis") +
  coord_sf(crs = st_crs(nigeria_outline_proj)) +
  theme_minimal() +
  labs(title = paste("Ordinary Kriging predictions —", year_list[[i]]))

krig_var_plot_list[[i]][j] <- ggplot(k_pred_df, aes(x = x, y = y, fill = var1.var)) +
  geom_raster(interpolate = FALSE) +
  geom_sf(data = nigeria_outline_proj, inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_viridis_c(name = "Prediction variance", option = "viridis") +
  coord_sf(crs = st_crs(nigeria_outline_proj)) +
  theme_minimal() +
  labs(title = paste("Kriging variance —", year_list[[i]]))

  }

# return results
list(
  vgm_emp = vgm_emp_list,
  vgm_fit = vgm_fit_list,
  krig_plot_list = krig_plot_list,
  krig_var_plot_list = krig_var_plot_list
)


