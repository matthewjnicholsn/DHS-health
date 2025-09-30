rm(list = ls())
lib_list <- c("sp","sf", "beepr", "gstat", "ggplot2", "viridis", "haven", "dplyr", "stringr", "purrr", "DHS.rates", "sjlabelled")
lapply(lib_list, require, character.only = T)
#get file function
source("/Users/matthewnicholson/DHS/get_file_function.R")
#BRdata for childmort calculations
BRdata <- readRDS(get_file(countries = "Nigeria_DHS", years = 1990, surveys = "BR"))
#gps data file for cluster coordinates
gps_data <- st_read('/Users/matthewnicholson/DHS/GPS files/Nigeria/1990/NGGE23FL/NGGE23FL.shp')|> 
  select(DHSCLUST,DHSREGCO,URBAN_RURA,LATNUM,LONGNUM,ALT_DEM,DATUM,geometry) |> 
  rename(cluster = DHSCLUST,
  region = DHSREGCO)
#shape file for region and state polygons (extent for kriging)
shp_file <- st_read("/Users/matthewnicholson/Downloads/nigeria_The_Federal_Republic_of_Nigeria_Country_Boundary/nigeria_The_Federal_Republic_of_Nigeria_Country_Boundary.shp") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# calculate chmort by cluster

# ******************************************************************************
# Program: 			  CM_CHILD.R
# Purpose: 		    Produce child mortality indicators   
# Data inputs: 		BR dataset
# Data outputs:		coded variables, and output on screen and in excel tables
# Author:				  Mahmoud Elkasabi
# Date last modified:  September 15 2021 by Mahmoud Elkasabi
# ******************************************************************************
#   
# -----------------------------------------------------------------------------#
# # Indicators created in this file:
# NNMR		"Neonatal Mortality Rate"
# PNNMR		"Post-neonatal Mortality Rate"
# IMR			"Infant Mortality Rate"
# CMR			"Child Mortality Rate"
# U5MR		"Under-5 Mortality Rate"
# -----------------------------------------------------------------------------#
#

BRdata <- BRdata %>%
  mutate(child_sex = b4) %>%
  mutate(child_sex = set_label(child_sex, label = "Sex of child"))  %>%
  mutate(months_age = b3-v011) %>%
  mutate(mo_age_at_birth =
           case_when(
             months_age < 20*12   ~ 1 ,
             months_age >= 20*12 & months_age < 30*12 ~ 2,
             months_age >= 30*12 & months_age < 40*12 ~ 3,
             months_age >= 40*12 & months_age < 50*12 ~ 4)) %>%
  mutate(mo_age_at_birth = factor(mo_age_at_birth, levels = c(1,2,3,4), labels = c("Mother's age at birth < 20", "Mother's age at birth 20-29", "Mother's age at birth 30-39","Mother's age at birth 40-49"))) %>%
  mutate(mo_age_at_birth = set_label(mo_age_at_birth, label = "Mother's age at birth")) %>%
  mutate(birth_order =
           case_when(
             bord == 1  ~ 1,
             bord >= 2 & bord <= 3 ~ 2,
             bord >= 4 & bord <= 6 ~ 3,
             bord >= 7  ~ 4,
             bord == NA ~ 99)) %>%
  mutate(birth_order = factor(birth_order, levels = c(1,2,3,4), labels = c("Birth order:1", "Birth order:2-3", "Birth order:4-6","Birth order:7+"))) %>%
  mutate(birth_order = set_label(birth_order, label = "Birth order"))  %>%
  mutate(prev_bint =
           case_when(
             b11 <= 23 ~ 1,
             b11 >= 24 & b11 <= 35 ~ 2,
             b11 >= 36 & b11 <= 47 ~ 3,
             b11 >= 48 ~ 4)) %>%
  mutate(prev_bint = set_label(prev_bint, label = "Preceding birth interval"))  %>%
  mutate(birth_size =
           case_when(
             m18 >= 4 & m18 <= 5 ~ 1,
             m18 <= 3 ~ 2,
             m18 > 5 ~ 99)) %>%
  mutate(birth_size = set_label(birth_size, label = "Birth size")) 

BRdata[["prev_bint"]] <- ifelse(is.na(BRdata[["prev_bint"]]), 999, BRdata[["prev_bint"]])
BRdata[["birth_size"]] <- ifelse(is.na(BRdata[["birth_size"]]), 999, BRdata[["birth_size"]])

BRdata <- BRdata %>%
mutate(prev_bint = factor(prev_bint, levels = c(1,2,3,4,999), labels = c("Previous birth interval <2 years", "Previous birth interval 2 years", "Previous birth interval 3 years","Previous birth interval 4+ years", "missing"))) %>%
mutate(birth_size = factor(birth_size, levels = c(1,2,99,999), labels = c("Birth size: Small/very small","Birth size: Average or larger", "Birth size: Don't know/missing", "missing" )))

##################################################################################
# MORTALITY RATES ################################################################
##################################################################################

BRdata_CMORT <- (BRdata[, c("v001", "v021", "v022","v024", "v025", "v005", "v008","v011", 
                            "b3", "b7", "v106", "child_sex", "mo_age_at_birth", "birth_order", "prev_bint","birth_size")]) # add v001 here for cluster level and rm v190

# NNMR, PNNMR, IMR, CMR & U5MR
# TABLES 8.1, 8.2 and 8.3

#get chmortp for cluster data and aggregate prob by cluster
  res_clust <- as.data.frame(chmortp(BRdata_CMORT, Class = "v001", Period = 120)) |> 
    group_by(Class) |> 
    summarise(
      prob = sum(W.DEATHS) / sum(W.EXPOSURE),
      .groups = 'drop'
    ) |> 
    mutate(Class = as.numeric(Class)) |> 
    rename(cluster = Class)


write.csv(res_clust, file = "Tables_child_mort_Ng1990_cluster.csv")

########################################################################################
#test region 1 first
#now to prepare for kriging
#merge data with gps file to visualize
res_clust <- res_clust |> 
  left_join(gps_data, by = "cluster") |> 
  mutate(prob = as.numeric(as.character(prob)) * 1000) |> 
  filter(region %in% c(1:4)) |> 
  filter(cluster != 1402) |> 
  st_as_sf() 
#remove 0,0 coords
#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~

#in this part I want to use this file: ng_1980_shp <- st_read("/Users/matthewnicholson/Downloads/Nigeria_states/1987-1991/1987-1991.shp"),
# which has polygon geometry of nigeria to constrain the bounds of the kriging to the shape of nigeria. I will need to map only the outer lines as the file contains teh
#geometries for each region. So the geoms will neeed to be aggregated and then the shape will be used as the bounds for the kriging and for the outline of the map (as a layer maybe)


#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~

ggplot(data = res_clust) +
  geom_sf(aes(color = prob), size = 3) +
  scale_color_viridis(option = "viridis", name = "child death probability") +
  theme_minimal() +
  labs(title = "Cluster-level child death probability (Nigeria 1990)")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# CONSTRAINING KRIGING TO NIGERIA BOUNDARY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# aggregate shapefile to single Nigeria outline
nigeria_outline <- shp_file |> st_union() |> st_make_valid()

# project to same CRS as cluster data
res_clust <- st_transform(res_clust, 32632) # UTM
nigeria_outline <- st_transform(nigeria_outline, 32632)
# plot(nigeria_outline) to check geometry union worked. Will take a long time as it's a large shapefile
#problems with the shapefile, may need a diff one


# create grid inside bounding box
bbox <- st_bbox(nigeria_outline)
res <- 10000   # 10 km grid resolution (adjust as needed)

x.range <- seq(bbox$xmin, bbox$xmax, by = res)
y.range <- seq(bbox$ymin, bbox$ymax, by = res)
grid <- expand.grid(x = x.range, y = y.range)

grid_sf <- st_as_sf(grid, coords = c("x", "y"), crs = 32632)

# keep only grid points inside Nigeria
grid_sf <- grid_sf[nigeria_outline, ]

# convert to Spatial for gstat
res_clust_sp <- as(res_clust, "Spatial") |> 
  remove.duplicates()

grid_sp <- as(grid_sf, "Spatial")

# add coords
res_clust_sp$X <- coordinates(res_clust_sp)[,1]
res_clust_sp$Y <- coordinates(res_clust_sp)[,2]
grid_sp$X <- coordinates(grid_sp)[,1]
grid_sp$Y <- coordinates(grid_sp)[,2]

# variogram + kriging
vgm_emp <- variogram(prob ~ 1, res_clust_sp)
vgm_fit <- fit.variogram(vgm_emp, model = vgm(c("Exp"
#  ,"Mat","Gau","Sph"
)))
plot(vgm_emp, vgm_fit)

k_model <- gstat(formula = prob ~ 1,
                 data = res_clust_sp,
                 model = vgm_fit)


k_pred <- predict(k_model, grid_sp)
beep()

# convert predictions for ggplot
k_pred_df <- as.data.frame(k_pred)
coords <- coordinates(k_pred)
k_pred_df$x <- coords[,1]
k_pred_df$y <- coords[,2]

dim(k_pred_df)
# plot with Nigeria boundary
ng_90_chmort_kirg <- ggplot() +
  geom_tile(data = k_pred_df, aes(x = x, y = y, fill = var1.pred)) +
  # geom_sf(data = nigeria_outline, fill = NA, color = "black", linewidth = 0.6) +
  scale_fill_viridis_c(name = "Predicted child mortality", option = "M") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Ordinary kriging: child mortality (1990), constrained to Nigeria")
ng_90_chmort_var <- ggplot() +
  geom_tile(data = k_pred_df, aes(x = x, y = y, fill = var1.var)) +
  # geom_sf(data = nigeria_outline, fill = NA, color = "black", linewidth = 0.6) +
  scale_fill_viridis_c(name = "Predicted child mortality", option = "M") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Ordinary kriging: child mortality variance (1990), constrained to Nigeria")

ggsave("ng_90_chnort_krig.png", plot = ng_90_chmort_kirg)
ggsave("ng_90_chmortvar.png", plot = ng_90_chmort_var)

#does not appear spatially continuous, high variance between observed values, predictions steeply vary past short distances. 
#explore co-kriging with universal kriging, ie population will be positively correlated, health outposts will be negatively correlated

#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~#~~~~~~~~~~~
# now we will try co-kriging, using population density as a co-variable
co_var <- res_clust |> 
  left_join(
    read.csv('/Users/matthewnicholson/DHS/GPS files/Nigeria/1990/NGGC22FL/NGGC22FL.csv') |>
      rename(cluster = DHSCLUST,
             pop = All_Population_Count_2005) |>
      mutate(pop = as.numeric(as.character(log1p(pop)))) |> 
      select(pop, cluster),
    by = "cluster"
  )
#this data frame now has cluster #, population per cluster, child deaths per 1000 live births per cluster, and geometry for each cluster.
#ready for co-kriging


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# CO-KRIGING: child mortality ~ population, constrained to Nigeria boundary
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#prep nigeria outline for grid
# aggregate shapefile to single Nigeria outline
nigeria_outline <- shp_file |> st_union() |> st_make_valid()

# project to same CRS as cluster data
res_clust <- st_transform(res_clust, 32632) # UTM zone 32N
nigeria_outline <- st_transform(nigeria_outline, 32632)

# create grid inside bounding box
bbox <- st_bbox(nigeria_outline)
res <- 1000   # 10 km grid resolution (adjust as needed)

x.range <- seq(bbox$xmin, bbox$xmax, by = res)
y.range <- seq(bbox$ymin, bbox$ymax, by = res)
grid <- expand.grid(x = x.range, y = y.range)

grid_sf <- st_as_sf(grid, coords = c("x", "y"), crs = 32632)

# keep only grid points inside Nigeria
grid_sf <- grid_sf[nigeria_outline, ]
# Convert co_var to sf first (if not already)
co_var_sf <- st_as_sf(co_var)

# Transform to UTM zone 32N (same CRS as grid_sf)
co_var_sf <- st_transform(co_var_sf, st_crs(grid_sf))

# Convert to Spatial for gstat
co_var_sp <- as(co_var_sf, "Spatial") |> remove.duplicates()
co_var_sp$X <- coordinates(co_var_sp)[,1]
co_var_sp$Y <- coordinates(co_var_sp)[,2]

# Make sure both target (prob) and covariate (pop) are numeric
co_var_sp$prob <- as.numeric(co_var_sp$prob)
co_var_sp$pop  <- as.numeric(co_var_sp$pop)

# Standardize variables (z-scores)
co_var_sp$prob_std <- scale(co_var_sp$prob)[,1]
co_var_sp$pop_std  <- scale(co_var_sp$pop)[,1]

# 1. Define a multivariate gstat object with standardized variables
gstat_obj <- gstat(NULL, id = "prob", formula = prob_std ~ 1, data = co_var_sp)
gstat_obj <- gstat(gstat_obj, id = "pop",  formula = pop_std  ~ 1, data = co_var_sp)

# 2. Compute empirical variograms (direct + cross)
vgm_emp_multi <- variogram(gstat_obj)
plot(vgm_emp_multi)

# 3. Define a starting model (scaled sill, moderate range/nugget)
vgm_model <- vgm(psill = 1, model = "Sph", range = 50000, nugget = 0.1)

# 4. Fit linear model of coregionalization (LMC)
vgm_fit_lmc <- fit.lmc(vgm_emp_multi, gstat_obj, vgm_model,
                       fit.ranges = FALSE, fit.sills = TRUE)

plot(vgm_emp_multi, vgm_fit_lmc)

# 5. Define gstat object for co-kriging using fitted LMC
g_co <- gstat(NULL, id = "prob", formula = prob_std ~ 1, data = co_var_sp)
g_co <- gstat(g_co, id = "pop", formula = pop_std ~ 1, data = co_var_sp)

# Step 2: Attach the fitted LMC
g_co <- gstat(g_co, model = vgm_fit_lmc, fill.all = TRUE)

# Step 3: Predict to grid
ck_pred <- predict(g_co, grid_sf) 

# 7. Convert predictions to df for ggplot
ck_pred_df <- as.data.frame(ck_pred)
coords <- coordinates(ck_pred)
ck_pred_df$x <- coords[,1]
ck_pred_df$y <- coords[,2]

# 8. Plot co-kriging predictions (note: values are in z-scores!)
ng_90_ckrig <- ggplot() +
  geom_tile(data = ck_pred_df, aes(x = x, y = y, fill = prob.pred)) +
  geom_sf(data = nigeria_outline, fill = NA, color = "black", linewidth = 0.6) +
  scale_fill_viridis_c(name = "Co-kriged child mortality (z-score)", option = "mako") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Co-kriging: standardized child mortality with standardized population (1990)")
plot(ng_90_ckrig)
ng_90_ckrig_var <- ggplot() +
  geom_tile(data = ck_pred_df, aes(x = x, y = y, fill = prob.var)) +
  geom_sf(data = nigeria_outline, fill = NA, color = "black", linewidth = 0.6) +
  scale_fill_viridis_c(name = "Prediction variance", option = "mako") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Co-kriging variance (standardized variables)")
plot(ng_90_ckrig_var)
ggsave("ng_90_ckrig.png", plot = ng_90_ckrig)
ggsave("ng_90_ckrigvar.png", plot = ng_90_ckrig_var)


## I USED THIS CODE TO DO THE CO-KRIGING (WORKS) BUT IT DOES NOT LOOK SPATIALLY CONTINUOUS
 # keep only grid points inside Nigeria
 grid_sf <- grid_sf[nigeria_outline, ]
#  library(sf)
#  library(sp)
#  library(gstat)
 
 # --- CRS: everything in UTM 32N ---
 co_var_sf <- st_as_sf(co_var) |> st_transform(st_crs(grid_sf))
 co_var_sp <- as(co_var_sf, "Spatial") |> remove.duplicates()
 
 # Ensure numeric and standardize
 co_var_sp$prob <- as.numeric(co_var_sp$prob)
 co_var_sp$pop  <- as.numeric(co_var_sp$pop)
 co_var_sp$prob_std <- scale(co_var_sp$prob)[,1]
 co_var_sp$pop_std  <- scale(co_var_sp$pop)[,1]
 
 # --- Build gstat object ---
 g_co <- gstat(NULL, id = "prob", formula = prob_std ~ 1, data = co_var_sp)
 g_co <- gstat(g_co, id = "pop", formula = pop_std ~ 1, data = co_var_sp)
 
 # --- Empirical variograms and LMC ---
 vgm_emp_multi <- variogram(g_co)
 vgm_model <- vgm(psill = 1, model = "Sph", range = 50000, nugget = 0.1)
 vgm_fit_lmc <- fit.lmc(vgm_emp_multi, g_co, vgm_model,
                        fit.ranges = FALSE, fit.sills = TRUE)
 
 # --- Attach the fitted LMC to gstat object ---
 g_co_lmc <- g_co
 g_co_lmc$model <- vgm_fit_lmc  # attach LMC
 g_co_lmc$fill.all <- TRUE       # fill all variogram parameters
 
 # --- Convert grid to Spatial ---
 grid_sp <- as(grid_sf, "Spatial")
 grid_sp$X <- coordinates(grid_sp)[,1]
 grid_sp$Y <- coordinates(grid_sp)[,2]
 
 # --- Predict ---
 ck_pred <- predict(g_co_lmc, grid_sp)
 
 # --- Convert predictions to dataframe for ggplot ---
 ck_pred_df <- as.data.frame(ck_pred)
 coords <- coordinates(ck_pred)
 ck_pred_df$x <- coords[,1]
 ck_pred_df$y <- coords[,2]
# [inverse distance weighted interpolation]
 ng_90_ckrig <- ggplot() +
   geom_tile(data = ck_pred_df, aes(x = x, y = y, fill = prob.pred)) +
   geom_sf(data = nigeria_outline, fill = NA, color = "black", linewidth = 0.6) +
   scale_fill_viridis_c(name = "Co-kriged child mortality (z-score)", option = "M") +
   coord_sf() +
   theme_minimal() +
   labs(title = "Co-kriging: standardized child mortality with standardized population (1990)")
 plot(ng_90_ckrig)
# Warning message:
# In viridisLite::viridis(n, alpha, begin, end, direction, option) :
#   Option 'M' does not exist. Defaulting to 'viridis'.
 ng_90_ckrig <- ggplot() +
   geom_tile(data = ck_pred_df, aes(x = x, y = y, fill = prob.pred)) +
   geom_sf(data = nigeria_outline, fill = NA, color = "black", linewidth = 0.6) +
   scale_fill_viridis_c(name = "Co-kriged child mortality (z-score)", option = "Mako") +
   coord_sf() +
   theme_minimal() +
   labs(title = "Co-kriging: standardized child mortality with standardized population (1990)")
 plot(ng_90_ckrig)

## NOW YOU SEE WHAT I MEAN - I NEED TO TRY A DIFFERENT METHOD. IT  WOULD BE GOOD TO DO SOMETHING LIKE THIS
## PCA TO EXTRACT FACTORS 
## CO-KRIGING WITH MULTIPLE VARS FROM FACTOR POSITIVELY CORR WITH MORT OR REGRESSION KRIGING AFTER PCA...