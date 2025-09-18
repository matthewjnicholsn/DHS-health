lib_list <- c("dplyr", "haven", "sf", "DHS.rates", "ggplot2")
lapply(lib_list, require, character.only = T)
source("/Users/matthewnicholson/DHS/get_file_function.R")
source("/Users/matthewnicholson/DHS/chap8/chmort_by_cluster.R")
countries <- c("Kenya_DHS")
years <- c(2003)
surveys <- c("BR")
data <- get_file(countries,years,surveys)
ke_sf <- read_sf("/Users/matthewnicholson/DHS/GPS files/Kenya/2003/KEGE43FL/KEGE43FL.shp") |> 
  rename(v001 = DHSCLUST) |> 
  select(v001, URBAN_RURA, ALT_DEM, DATUM, geometry)

data <- data |> 
left_join(ke_sf |> distinct(v001, geometry), join_by(v001), 
            relationship = "many-to-many")
chmort_state(file_paths = get_file("Kenya_DHS", 2003, "BR"))
## cannot calculate mortality per cluster, needs multiple clusters to calculate error
## can group by a few clusters, expand the point or make it the centroid, some major concessions with this methods





