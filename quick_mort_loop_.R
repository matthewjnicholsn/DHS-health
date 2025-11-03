lib_list <- c("DHS.rates", "dplyr", "ggplot2", "sf")
lapply(lib_list,require,character.only = T)
shp_file_list <- c('/Users/matthewnicholson/DHS/GPS files/Nigeria/1990/NGGE23FL/NGGE23FL.shp', '/Users/matthewnicholson/DHS/GPS files/Nigeria/2003/NGGE4BFL/NGGE4BFL.shp',
'/Users/matthewnicholson/DHS/GPS files/Nigeria/2008/NGGE52FL/NGGE52FL.shp','/Users/matthewnicholson/DHS/GPS files/Nigeria/2013/NGGE6AFL/NGGE6AFL.shp',
'/Users/matthewnicholson/DHS/GPS files/Nigeria/2018/NGGE7BFL/NGGE7BFL.shp')

source("get_file_function.R")
file_list <- get_file(countries = "Nigeria_DHS", years = c(1990,2003,2008,2013,2018), surveys = "BR")
chmort_res <- list()
for(i in seq_along(file_list)){
  br <- readRDS(file_list[i])
  sf <- st_read(shp_file_list[i])
  chmort_res[[i]] <- chmort(br)

}
