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
shp_file <- st_read("/Users/matthewnicholson/Downloads/Nigeria_states/1987-1991/1987-1991.shp") 

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

# dissolve shapefile to single Nigeria outline
nigeria_outline <- shp_file |> st_union() |> st_make_valid()

# project to same CRS as cluster data
res_clust <- st_transform(res_clust, 32632) # UTM
nigeria_outline <- st_transform(nigeria_outline, 32632)
# plot(nigeria_outline) to check geometry union worked. Will take a long time as it's a large shapefile

# create grid inside bounding box
bbox <- st_bbox(nigeria_outline)
res <- 1000   # 10 km grid resolution (adjust as needed)

x.range <- seq(bbox$xmin, bbox$xmax, by = res)
y.range <- seq(bbox$ymin, bbox$ymax, by = res)
grid <- expand.grid(x = x.range, y = y.range)

grid_sf <- st_as_sf(grid, coords = c("x", "y"), crs = 32632)

# keep only grid points inside Nigeria
grid_sf <- grid_sf[nigeria_outline, ]

# convert to Spatial for gstat
res_clust_sp <- as(res_clust, "Spatial")
grid_sp <- as(grid_sf, "Spatial")

# add coords
res_clust_sp$X <- coordinates(res_clust_sp)[,1]
res_clust_sp$Y <- coordinates(res_clust_sp)[,2]
grid_sp$X <- coordinates(grid_sp)[,1]
grid_sp$Y <- coordinates(grid_sp)[,2]

# variogram + kriging
vgm_emp <- variogram(prob ~ 1, res_clust_sp)
vgm_fit <- fit.variogram(vgm_emp, model = vgm(c("Exp","Mat","Gau","Sph")))
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
ggplot() +
  geom_tile(data = k_pred_df, aes(x = x, y = y, fill = var1.pred)) +
  # geom_sf(data = nigeria_outline, fill = NA, color = "black", linewidth = 0.6) +
  scale_fill_viridis_c(name = "Predicted child mortality", option = "F") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Ordinary kriging: child mortality (1990), constrained to Nigeria")
beep()
