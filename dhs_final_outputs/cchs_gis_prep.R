library(sf)
library(dplyr)
library(readr)
library(stringr)
#just ontario test
cchs <- read_csv('/Users/matthewnicholson/Downloads/my data/data files/pumf_cchs.csv') |> 
  filter(GEOGPRV == 35) |> 
  mutate(GEODGHR4 = as.numeric(str_replace(GEODGHR4, "(?<=^.{2}).", ""))) 

on_hr <- c(unique(cchs$GEODGHR4))

can_hr <- st_read("/Users/matthewnicholson/Downloads/Cart2022_shp-eng/HR_000b22a_e/HR_000b22a_e.shp")|> 
  dplyr::filter(HR_UID %in% on_hr)


#make ontario boundary
on_bound <- can_hr |> 
  st_union() |> st_make_valid() 
plot(on_bound)
plot(can_hr)

cchs <- cchs |> 
   group_by(GEODGHR4) |>
        summarize(
          health = sum(WTS_M * GEN_005, na.rm = TRUE) / 
                            sum(WTS_M[!is.na(WTS_M)], na.rm = TRUE),
          unweighted_n = n(),
          total_weight = sum(WTS_M, na.rm = TRUE),
          .groups = "drop")
cchs <- cchs |> 
  select(GEODGHR4,health) |> 
  rename(HR_UID = GEODGHR4) |> 
  mutate(HR_UID = as.character(HR_UID)) |> 
  left_join(can_hr, by = 'HR_UID')

#write our ontario health region shapefile
st_write(cchs, dsn = 'On_hr_2022_with_health.shp')
test <- st_read('On_hr_2022_with_health.shp')

#all of Canada test
cchs_data <- read_csv('/Users/matthewnicholson/Downloads/my data/data files/pumf_cchs.csv') |> 
  mutate(GEODGHR4 = as.numeric(sub("(?<=^.{2})9", "", GEODGHR4, perl = TRUE))) |> #removes extraneous 9
  group_by(GEODGHR4) |>
        summarize(
          health = sum(WTS_M * GEN_005, na.rm = TRUE) / 
                            sum(WTS_M[!is.na(WTS_M)], na.rm = TRUE),
          unweighted_n = n(),
          total_weight = sum(WTS_M, na.rm = TRUE),
          .groups = "drop")

can_hr <- st_read("/Users/matthewnicholson/Downloads/Cart2022_shp-eng/HR_000b22a_e/HR_000b22a_e.shp")

cchs <- cchs_data |> 
  select(GEODGHR4,health) |> 
  rename(HR_UID = GEODGHR4) |> 
  mutate(HR_UID = as.character(HR_UID)) |> 
  left_join(can_hr, by = 'HR_UID')

st_write(cchs,dsn = "canada_health_regions_final.shp")

canada_boundary <- can_hr |> st_union() |> st_make_valid() 
st_write(canada_boundary, "canada_boundary.shp")



cchs_data <- read_csv('/Users/matthewnicholson/Downloads/my data/data files/pumf_cchs.csv') 
length(unique(cchs_data$GEODGHR4))

library(psych)


cchs_data <- read_csv('/Users/matthewnicholson/Downloads/my data/data files/pumf_cchs.csv')
cchs_health <- select(cchs_data, c(GEN_005,GEN_010,GEN_015,GEN_020,GEN_025,GEN_030))

cchs_fa <- fa(r = cchs_health,
              nfactors = 1,
              rotate = "varimax",
              fm = "ml",
              scores = T)

province_labels <- tibble(
  province = c("Newfoundland and labrador", "Prince Edward Island","Nova Scotia", "New Brunswick","Quebec","Ontario","Manitoba","Saskatchewan","Alberta","British Columbia","Yukon/Northwest/Nunavut Territories"),
  GEOGPRV = c(10,11,12,13,24,35,46,47,48,59,60)
)

health_factor <- as.data.frame(cchs_fa$scores)
cchs_data_wfactor <- cbind(cchs_data, health_factor) |> 
  mutate(GEODGHR4 = as.character(sub("(?<=^.{2})9", "", GEODGHR4, perl = TRUE))) |> #removes extraneous 9
  rename(health_factor = ML1) |> 
  group_by(GEODGHR4,GEOGPRV) |>
        summarize(
          health = sum(WTS_M * health_factor, na.rm = TRUE) / 
                            sum(WTS_M[!is.na(WTS_M)], na.rm = TRUE),
          unweighted_n = n(),
          total_weight = sum(WTS_M, na.rm = TRUE),
          .groups = "drop") |> 
  rename(HR_UID = GEODGHR4) |> 
  left_join(can_hr, by = 'HR_UID') |> 
  left_join(province_labels, by = "GEOGPRV", relationship = "many-to-one") |> 
  st_as_sf() 

st_write(cchs_data_wfactor, dsn = "canada_health_factor.shp")

cchs_data_wfactor$GEODGHR4 <- as.character(cchs_data_wfactor$GEODGHR4)

psych::alpha(cchs_health)
