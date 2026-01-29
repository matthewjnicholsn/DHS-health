rm(list = ls())
setwd("~/DHS")
lapply(c("dplyr","sf","gstat","haven","stars","terra","survey"),require,character.only = TRUE)
source("get_file_function.R")

# Universal kriging of DRC wealth data using population raster data as covariate

#get the data file for wealth
drc_dat <- readRDS("/Users/matthewnicholson/DHS/DHS_surveys_rds_organized/DRC/2023.5/CDHR81DT/DRC_DHS_CD_2023-24_DHS_01062026_1747_219655_CDHR81DT_CDHR81FL.rds")
drc_spatial <- st_read('/Users/matthewnicholson/DHS/GPS files/DRC/2023-2024/CDGE81FL/CDGE81FL.shp') |> 
  rename(cluster = "DHSCLUST") |> 
  select(cluster,geometry) |> 
  st_as_sf()
#create the outline for the boundary box
drc_outline <- st_read("/Users/matthewnicholson/Downloads/cod_admin_boundaries.shp/cod_admin0.shp") |> 
  st_union() |> st_make_valid() |> st_transform(32734)

# load in the population raster
drc_pop_rast <- rast("/Users/matthewnicholson/Downloads/COD_population_v4_4_gridded/COD_Population_v4_4_gridded.tif")
drc_pop <- as.points(drc_pop_rast) |> 
  st_as_sf()
#now we do the calculations to get wealth at the cluster level
wealth_data <- drc_dat |> 
  select(hv001,hv270,hv005) |> 
  rename(cluster = hv001,
         wealth = hv270,
         weight = hv005) |> 
  mutate(wealth = as.numeric(as.character(wealth)),
         weight = as.numeric(as.character(weight))) |> 
  group_by(cluster) |> 
  summarize(
            weighted_wealth = sum(weight * wealth, na.rm = TRUE) / 
                              sum(weight[!is.na(wealth)], na.rm = TRUE),
            unweighted_n = n(),
            total_weight = sum(weight, na.rm = TRUE),
            .groups = "drop") |> 
  left_join(drc_spatial,by = "cluster") |> 
  select(weighted_wealth,cluster,geometry) |> 
  st_as_sf()

#other way to do that is with a survey object
drc_dat <- readRDS("/Users/matthewnicholson/DHS/DHS_surveys_rds_organized/DRC/2023.5/CDHR81DT/DRC_DHS_CD_2023-24_DHS_01062026_1747_219655_CDHR81DT_CDHR81FL.rds") |> 
  mutate(hv005 = hv005/1000000)
dstrat <- svydesign(ids = ~hv021, strata = ~hv023, weights = ~ hv005, data = drc_dat)

weighted_wealth <- svyby(~hv270,~hv001, design = dstrat, svymean)

drc_spatial <- st_read('/Users/matthewnicholson/DHS/GPS files/DRC/2023-2024/CDGE81FL/CDGE81FL.shp') |> 
  rename(hv001 = "DHSCLUST") |> 
  select(hv001,geometry) |> 
  st_as_sf()

weight_drc_dat <- left_join(weighted_wealth,drc_spatial, by = "hv001") |> 
  st_as_sf() |> 
  st_transform(st_crs(drc_outline))

#Now we setup the kriging 
drc_outline <- st_transform(drc_outline, 32734)
# create grid inside bounding box
bbox <- st_bbox(drc_outline)
res <- 10000   # 10 km grid resolution (adjust as needed)

x.range <- seq(bbox$xmin, bbox$xmax, by = res)
y.range <- seq(bbox$ymin, bbox$ymax, by = res)
grid <- expand.grid(x = x.range, y = y.range)

grid_sf <- st_as_sf(grid, coords = c("x", "y"), crs = 32734)

# keep only grid points inside DRC
grid_sf <- grid_sf[drc_outline, ]

# convert to Spatial for gstat
wealth_data_sp <- as(wealth_data, "Spatial") |> 
  remove.duplicates()

grid_sp <- as(grid_sf, "Spatial")

# add coords
wealth_data_sp$X <- coordinates(wealth_data_sp)[,1]
wealth_data_sp$Y <- coordinates(wealth_data_sp)[,2]
grid_sp$X <- coordinates(grid_sp)[,1]
grid_sp$Y <- coordinates(grid_sp)[,2]

#fit variogram

#one problem will be that the points in the population grid might not all be or any be represented by cluster locations (because of jittering)
# may need to "snap" the clusters to nearest grid points, which could be tricky
vgm_emp <- variogram(log(weighted_wealth) ~ 1, wealth_data_sp)
vgm_fit <- fit.variogram(vgm_emp, model = vgm(c("Exp"
 ,"Mat","Gau","Sph"
)))

k_model <- gstat(formula = weighted_wealth ~ 1,
                 data = wealth_data_sp,
                 model = vgm_fit)
wealth_data_sp <- wealth_data_sp |> st_as_sf() 
wealth_data_sp <- st_transform(wealth_data_sp, st_crs(grid_sf))
wealth_data_sp <- as(wealth_data_sp, "Spatial")

kriged <- krige(weighted_wealth ~ 1, grid_sf, wealth_data_sp, vgm_fit)

#redo
wealth_data_sf <- st_as_sf(wealth_data_sp)
wealth_data_sf$pop_dens <- terra::extract(drc_pop_rast, vect(wealth_data_sf))[,2]
wealth_data_sf[is.na(wealth_data_sf)] <- 0

lm_model <- lm(weighted_wealth ~ pop_dens, data = wealth_data_sf)
summary(lm_model) # Check R-squared, significance

wealth_data_sf$trend <- predict(lm_model, newdata = wealth_data_sf)
wealth_data_sf$residual <- wealth_data_sf$weighted_wealth - wealth_data_sf$trend
class(wealth_data_sf)

vgm_residual <- variogram(residual ~ 1, data = wealth_data_sf)
plot(vgm_residual)

fit_vgm <- fit.variogram(vgm_residual, model = vgm("Sph"))
plot(vgm_residual, fit_vgm)
print(fit_vgm)


# Step 5: Interpolation (Regression Kriging)

# 5.1. Create a prediction grid from the population raster
# We'll use the population raster as the template for our prediction grid
pred_grid <- as.points(drc_pop_rast)
pred_grid_sf <- st_as_sf(pred_grid)
names(pred_grid_sf)[names(pred_grid_sf) == "lyr1"] <- "pop_dens"  # Rename to match your variable
pred_grid_sf <- pred_grid_sf |> 
  rename(pop_dens = COD_Bas_Uele_population_v4_4_gridded)

# 5.2. Predict the TREND component at all grid locations using the linear model
# First, ensure we have the linear model (from Step 3)
# Assuming you already ran: lm_model <- lm(wealth_index ~ pop_dens, data = wealth_data_sf)
pred_grid_sf$trend_pred <- predict(lm_model, newdata = pred_grid_sf)

pred_grid_sf <- st_transform(pred_grid_sf,32734)

# 5.3. Perform ORDINARY KRIGING on the RESIDUALS from the wealth data
# First, ensure wealth_data_sf has the residuals column (from Step 3)
# Assuming you already ran: wealth_data_sf$residual <- wealth_data_sf$wealth_index - predict(lm_model)

# Perform kriging on residuals
krige_residual <- krige(formula = residual ~ 1,        # Ordinary kriging on residuals
                        locations = wealth_data_sf,    # Your wealth data with residuals
                        newdata = pred_grid_sf,        # Prediction grid
                        model = fit_vgm)              # Fitted variogram model from Step 4

# Add the kriged residuals to our prediction grid
pred_grid_sf$residual_pred <- krige_residual$var1.pred

# 5.4. COMBINE: Final Prediction = Trend + Interpolated Residual
pred_grid_sf$wealth_pred <- pred_grid_sf$trend_pred + pred_grid_sf$residual_pred

# Optional: Also add the kriging variance from the residual interpolation
pred_grid_sf$krige_var <- krige_residual$var1.var

# Step 6: Generate Final Output Raster

# Convert the predicted point grid back to a raster
# First, convert pred_grid_sf to vector format for rasterization
pred_grid_vect <- vect(pred_grid_sf)

# Rasterize the wealth predictions using drc_pop_rast as template
wealth_prediction_raster <- rasterize(pred_grid_vect, 
                                      drc_pop_rast, 
                                      field = "wealth_pred")

# Also create raster for kriging variance (uncertainty map)
wealth_variance_raster <- rasterize(pred_grid_vect, 
                                    drc_pop_rast, 
                                    field = "krige_var")

# Step 7: Visualization and Export

# Plot the final wealth prediction
plot(wealth_prediction_raster, 
     main = "Wealth Index Prediction - DRC",
     col = hcl.colors(100, "RdYlGn"))

# Add the original data points for reference
plot(st_geometry(wealth_data_sf), add = TRUE, pch = 16, cex = 0.6, col = "black")

# Plot the uncertainty map
plot(wealth_variance_raster,
     main = "Prediction Variance (Uncertainty)",
     col = hcl.colors(100, "Plasma"))

# Calculate summary statistics
cat("\n=== Prediction Statistics ===\n")
cat("Mean predicted wealth:", mean(pred_grid_sf$wealth_pred, na.rm = TRUE), "\n")
cat("SD of predictions:", sd(pred_grid_sf$wealth_pred, na.rm = TRUE), "\n")
cat("Range:", range(pred_grid_sf$wealth_pred, na.rm = TRUE), "\n")

cat("\n=== Uncertainty Statistics ===\n")
cat("Mean kriging variance:", mean(pred_grid_sf$krige_var, na.rm = TRUE), "\n")

# Step 8: Export Results

# Save the prediction raster
writeRaster(wealth_prediction_raster, 
            "DRC_Wealth_Prediction.tif",
            overwrite = TRUE)

# Save the uncertainty raster
writeRaster(wealth_variance_raster,
            "DRC_Wealth_Prediction_Variance.tif",
            overwrite = TRUE)

# Optional: Save the prediction points as a shapefile for further analysis
st_write(pred_grid_sf, "DRC_Wealth_Predictions.shp", delete_dsn = TRUE)

# Step 9: Alternative - Direct Universal Kriging Approach
# This does everything in one step (trend + residuals) with proper error propagation

# First, need to fit a variogram that includes the covariate
vgm_uk <- variogram(wealth_index ~ pop_dens, data = wealth_data_sf)
fit_vgm_uk <- fit.variogram(vgm_uk, model = vgm("Sph"))

# Now perform universal kriging
uk_result <- krige(formula = wealth_index ~ pop_dens,   # External drift model
                   locations = wealth_data_sf,          # Your wealth data
                   newdata = pred_grid_sf,              # Grid with pop_dens
                   model = fit_vgm_uk)                  # Fitted variogram

# Compare results
cat("\n=== Model Comparison ===\n")
cat("Regression Kriging mean:", mean(pred_grid_sf$wealth_pred, na.rm = TRUE), "\n")
cat("Universal Kriging mean:", mean(uk_result$var1.pred, na.rm = TRUE), "\n")
cat("Correlation between methods:", 
    cor(pred_grid_sf$wealth_pred, uk_result$var1.pred, use = "complete.obs"), "\n")

# Create raster from universal kriging results
uk_raster <- rasterize(vect(uk_result), drc_pop_rast, field = "var1.pred")
uk_var_raster <- rasterize(vect(uk_result), drc_pop_rast, field = "var1.var")

# Save universal kriging results
writeRaster(uk_raster, "DRC_Wealth_UniversalKriging.tif", overwrite = TRUE)