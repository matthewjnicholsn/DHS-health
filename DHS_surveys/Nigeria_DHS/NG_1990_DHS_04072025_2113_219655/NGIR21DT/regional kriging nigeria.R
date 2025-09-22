lib_list <- c("sp","sf", "gstat", "ggplot2", "viridis", "haven", "dplyr")
lapply(lib_list, require, character.only = T)

# load in our data
source("/Users/matthewnicholson/DHS/get_file_function.R")
year_list <- c(1990,2003,2008,2010,2013,2015,2018,2021)
st_file_list <- c(
 '/Users/matthewnicholson/DHS/GPS files/Nigeria/1990/NGGE23FL/NGGE23FL.shp',
  '/Users/matthewnicholson/DHS/GPS files/Nigeria/2003/NGGE4BFL/NGGE4BFL.shp',
   '/Users/matthewnicholson/DHS/GPS files/Nigeria/2008/NGGE52FL/NGGE52FL.shp',
    '/Users/matthewnicholson/DHS/GPS files/Nigeria/2010/NGGE61FL/NGGE61FL.shp',
     '/Users/matthewnicholson/DHS/GPS files/Nigeria/2013/NGGE6AFL/NGGE6AFL.shp',
      '/Users/matthewnicholson/DHS/GPS files/Nigeria/2015/NGGE71FL/NGGE71FL.shp',
       '/Users/matthewnicholson/DHS/GPS files/Nigeria/2018/NGGE7BFL/NGGE7BFL.shp',
        '/Users/matthewnicholson/DHS/GPS files/Nigeria/2021/NGGE81FL/NGGE81FL.shp'
      )
ir_file_list <- get_file("Nigeria_DHS", year_list, "IR")
# we get all of the files so that we can get the variograms for all years eventually, or conversely do a spatio-temporal kriging
# first we try one year, 1990
# load the gps file first
sf_1990 <- st_read(st_file_list[1]) |>
  dplyr::select(DHSCLUST, geometry, DHSREGCO) |>
  dplyr::rename(cluster = DHSCLUST, region = DHSREGCO)
#then the data file, as it is a long pipeline reliant on the sf being in the environment
data_1990 <- readRDS(ir_file_list[1]) |>
  dplyr::select(v001, v106, v005, v024) |>
  dplyr::rename(cluster = v001, education = v106, wt = v005, region = v024) |>
  dplyr::mutate(
    education = dplyr::na_if(education, 9),
    wt = as.numeric(wt) / 1e6
  ) |>
  dplyr::mutate(
    education = as.numeric(haven::as_factor(education, levels = "default"))
  ) |>
  dplyr::group_by(cluster) |> 
  dplyr::summarise(
    weighted_education = sum(wt * education, na.rm = TRUE) / sum(wt[!is.na(education)], na.rm = TRUE),
    unweighted_n = dplyr::n(),
    total_weight = sum(wt, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  dplyr::left_join(sf_1990, by = "cluster") |>
  st_as_sf()