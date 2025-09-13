lib_list <- c("dplyr", "tidyr", "sf", "ggplot2", "viridis")
lapply(lib_list, require, character.only = T)

# Load CCHS data
cchs <- read.csv("/Users/matthewnicholson/Downloads/2019-2020 CCHS_Public available_CSV/data files/pumf_cchs.csv")
canada <- read_sf('/Users/matthewnicholson/Downloads/2019-2020 CCHS_Public available_CSV/lpr_000b16a_e 1/lpr_000b16a_e.shp')

# Rename columns for clarity
cchs <- cchs %>% 
  rename(
    id = ADM_RNO1,
    province = GEOGPRV,
    BMI = HWTDGISW
  )

# Only keep BMI values of 1 (underweight) or 2 (overweight)
BMI_df <- cchs %>%
  filter(BMI %in% c(1, 2)) 

# Calculate proportion overweight by province
bmi_by_province <- BMI_df %>%
  group_by(province) %>%
  summarise(prop_overweight = mean(BMI == 2, na.rm = TRUE),
            n = n()) %>%
  ungroup() %>%
  mutate(PRUID = as.numeric(province))

# Prepare spatial data for joining
canada <- canada %>% mutate(PRUID = as.numeric(PRUID))

# Join CCHS summary to shapefile
canada_bmi <- canada %>%
  left_join(bmi_by_province, by = "PRUID") %>%
  mutate(centroid = st_centroid(geometry))

# Plot choropleth of overweight proportion by province
ggplot(data = canada_bmi) +
  geom_sf(aes(fill = prop_overweight), color = "white") +
  geom_sf_text(aes(label = PRNAME), size = 3, check_overlap = TRUE) +
  theme_void() +
  scale_fill_viridis_c(option = "viridis")