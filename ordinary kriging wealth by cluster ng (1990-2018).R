# R
# Kriging of cluster-level weighted mean wealth index (per your weighted_wealth_by_cluster_by_year_ng.R)
library(dplyr)
library(sf)
library(sp)
library(gstat)
library(automap)
library(ggplot2)
library(viridis)

# --- User settings / paths (adjust if needed) ---
year_list <- c(1990, 2003, 2008, 2013, 2018)

# HR file list should be the same one you used previously (get_file result)
# hr_file_list <- get_file(countries = "Nigeria_DHS", years = year_list, surveys = "HR")
# If hr_file_list already exists in your environment, the script will use it.
# Shapefile list copied from your attached script:
shp_file_list <- c(
  '/Users/matthewnicholson/DHS/GPS files/Nigeria/1990/NGGE23FL/NGGE23FL.shp',
  '/Users/matthewnicholson/DHS/GPS files/Nigeria/2003/NGGE4BFL/NGGE4BFL.shp',
  '/Users/matthewnicholson/DHS/GPS files/Nigeria/2008/NGGE52FL/NGGE52FL.shp',
  '/Users/matthewnicholson/DHS/GPS files/Nigeria/2013/NGGE6AFL/NGGE6AFL.shp',
  '/Users/matthewnicholson/DHS/GPS files/Nigeria/2018/NGGE7BFL/NGGE7BFL.shp'
)

# 1990 wealth DTA used in your original script (only needed for 1990 join)
ng_90_wi_path <- "/Users/matthewnicholson/DHS/DHS_surveys/Nigeria_DHS/NG_1990_DHS_04072025_2113_219655/NGWI21DT/NGWI21FL.DTA"

# Nigeria country boundary shape used for plotting / clipping
# Replace this path if you have a different national shapefile; your session had one loaded earlier.
nigeria_shp_path <- "/Users/matthewnicholson/Downloads/nigeria_The_Federal_Republic_of_Nigeria_Country_Boundary/nigeria_The_Federal_Republic_of_Nigeria_Country_Boundary.shp"

# Grid resolution (metres) and projected CRS (use an appropriate UTM / metric CRS for Nigeria)
crs_proj <- 32632L    # UTM zone 32N (example) — keep what you used earlier
res_m <- 10000        # 10 km grid spacing

# --- 1) Compute cluster-level weighted wealth (mirrors your earlier logic) ---
# Expectation: hr_file_list is a list of data frames (readRDS results) with hv001, hv005, hv270 (or equivalent)
if (!exists("hr_file_list")) stop("hr_file_list not found in environment — supply the HR file list (readRDS/get_file).")

# load 1990 wealth file used in original script
ng_90_wi <- haven::read_dta(ng_90_wi_path) |> 
  rename(hhid = whhid, hv270 = wlthind5)

wealth_results <- vector("list", length(hr_file_list))
shp_results <- vector("list", length(hr_file_list))

for (i in seq_along(hr_file_list)) {
  hr_dat <- readRDS(hr_file_list[[i]])   # assume structure compatible with your earlier script
  # prepare cluster shapefile
  shp_file <- st_read(shp_file_list[[i]], quiet = TRUE) |>
    rename(hv001 = DHSCLUST) |>
    select(hv001, geometry)

  if (i == 1) {
    # 1990: join the separate wealth file to get hv270
    wealth_df <- hr_dat |>
      left_join(ng_90_wi, by = "hhid") |>
      select(hv001, hv005, hv270) |>
      group_by(hv001) |>
      summarize(
        weighted_wealth = sum(hv005 * hv270, na.rm = TRUE) / sum(hv005[!is.na(hv270)], na.rm = TRUE),
        unweighted_n = n(),
        total_weight = sum(hv005, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(hv001 = as.numeric(hv001), year = year_list[i])
  } else {
    wealth_df <- hr_dat |>
      select(hv001, hv005, hv270) |>
      group_by(hv001) |>
      summarize(
        weighted_wealth = sum(hv005 * hv270, na.rm = TRUE) / sum(hv005[!is.na(hv270)], na.rm = TRUE),
        unweighted_n = n(),
        total_weight = sum(hv005, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(hv001 = as.numeric(hv001), year = year_list[i])
  }

  wealth_results[[i]] <- wealth_df
  shp_results[[i]] <- shp_file |> left_join(wealth_df, by = "hv001")
}

# Combined tables (optional, kept for reference)
wealthclust_yrs <- bind_rows(wealth_results)
wealthclust_shp_yrs <- bind_rows(shp_results)

# --- 2) Prepare Nigeria outline and prediction grid (projected CRS) ---
nigeria_outline <- st_read(nigeria_shp_path, quiet = TRUE) |> st_union() |> st_make_valid()
nigeria_outline_proj <- st_transform(nigeria_outline, crs_proj)

# regular grid covering bbox, then keep points inside Nigeria
bbox_proj <- st_bbox(nigeria_outline_proj)
x_seq <- seq(bbox_proj$xmin, bbox_proj$xmax, by = res_m)
y_seq <- seq(bbox_proj$ymin, bbox_proj$ymax, by = res_m)
grid_df <- expand.grid(x = x_seq, y = y_seq)
grid_sf_proj <- st_as_sf(grid_df, coords = c("x", "y"), crs = crs_proj)

inside_idx <- st_intersects(grid_sf_proj, nigeria_outline_proj, sparse = FALSE)[,1]
grid_sf_proj <- grid_sf_proj[inside_idx, , drop = FALSE]

# convert to SpatialPointsDataFrame for gstat
grid_sp <- as(grid_sf_proj, "Spatial")

# --- 3) Loop over years: variogram fit (automap) + ordinary kriging + plotting ---
n <- length(shp_results)
vgm_emp_list <- vector("list", n)
vgm_fit_list <- vector("list", n)
krig_maps <- vector("list", n)
krig_var_maps <- vector("list", n)

for (i in seq_len(n)) {
  shp_i <- shp_results[[i]]

  # ensure we have the response weighted_wealth and geometry
  if (!("weighted_wealth" %in% names(shp_i))) {
    vgm_emp_list[[i]] <- NULL
    vgm_fit_list[[i]] <- NULL
    krig_maps[[i]] <- NULL
    krig_var_maps[[i]] <- NULL
    next
  }

  # drop NA responses and convert to sf -> project -> Spatial
  shp_i_sf <- st_as_sf(shp_i) |> filter(!is.na(weighted_wealth))
  if (nrow(shp_i_sf) < 3) {
    # not enough points to variogram/krige
    vgm_emp_list[[i]] <- NULL
    vgm_fit_list[[i]] <- NULL
    krig_maps[[i]] <- NULL
    krig_var_maps[[i]] <- NULL
    next
  }

  shp_i_sf_proj <- st_transform(shp_i_sf, crs_proj)

  dat_sp <- as(shp_i_sf_proj, "Spatial")
  dat_sp <- sp::remove.duplicates(dat_sp)   # remove duplicate coords if any

  # empirical variogram
  v_emp <- tryCatch(variogram(weighted_wealth ~ 1, dat_sp), error = function(e) NULL)
  vgm_emp_list[[i]] <- v_emp

  # automap autofitVariogram with safe fallback
  af <- tryCatch(
    automap::autofitVariogram(weighted_wealth ~ 1, input_data = dat_sp, verbose = FALSE),
    error = function(e) NULL
  )

  if (!is.null(af)) {
    # extract fitted model (automap uses different names across versions)
    if ("var_model" %in% names(af)) vgm_fit <- af$var_model
    else if ("var.model" %in% names(af)) vgm_fit <- af$var.model
    else if ("model" %in% names(af)) vgm_fit <- af$model
    else vgm_fit <- af
  } else {
    # fallback simple start model: split sample variance into sill/nugget
    samp_var <- stats::var(dat_sp@data$weighted_wealth, na.rm = TRUE)
    start_psill <- max(samp_var * 0.7, 1e-6)
    start_nugget <- max(samp_var * 0.3, 1e-6)
    approx_range <- max(diff(x_seq), diff(y_seq)) / 6
    start_vgm <- vgm(psill = start_psill, model = "Exp", range = approx_range, nugget = start_nugget)
    vgm_fit <- tryCatch(fit.variogram(v_emp, start_vgm), error = function(e) start_vgm)
  }

  vgm_fit_list[[i]] <- vgm_fit

  # ordinary kriging using gstat
  krig_model <- gstat(formula = weighted_wealth ~ 1, data = dat_sp, model = vgm_fit)
  krig_pred_sp <- predict(krig_model, grid_sp)

  # to data.frame for plotting
  kdf <- as.data.frame(krig_pred_sp)
  coords <- coordinates(krig_pred_sp)
  kdf$x <- coords[,1]
  kdf$y <- coords[,2]

  # plotting: use geom_tile (tile centers at points); ensure sf layer doesn't inherit aesthetics
  krig_maps[[i]] <- ggplot(kdf, aes(x = x, y = y, fill = var1.pred)) +
    geom_tile() +
    geom_sf(data = nigeria_outline_proj, inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.3) +
    scale_fill_viridis_c(name = "Predicted weighted wealth", option = "viridis") +
    coord_sf(crs = st_crs(nigeria_outline_proj)) +
    theme_minimal() +
    labs(title = paste("Kriging prediction —", year_list[i]))

  krig_var_maps[[i]] <- ggplot(kdf, aes(x = x, y = y, fill = var1.var)) +
    geom_tile() +
    geom_sf(data = nigeria_outline_proj, inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.3) +
    scale_fill_viridis_c(name = "Kriging variance", option = "viridis") +
    coord_sf(crs = st_crs(nigeria_outline_proj)) +
    theme_minimal() +
    labs(title = paste("Kriging variance —", year_list[i]))
}

# Return a structured result (inspect these objects interactively)
list(
  wealth_cluster = wealthclust_yrs,
  wealth_shp = wealthclust_shp_yrs,
  vgm_emp = vgm_emp_list,
  vgm_fit = vgm_fit_list,
  krig_maps = krig_maps,
  krig_var_maps = krig_var_maps
)