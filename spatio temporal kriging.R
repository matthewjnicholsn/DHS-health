lib_list <- c("sp", "gstat", "raster", "spacetime") #load packages
lapply(lib_list, require, character.only = T)
source("/Users/matthewnicholson/DHS/get_file_function.R") #source get file fun
year_list <- c(1990,2003,2008,2010,2013,2015,2018,2021) #list of years to include
st_file_list <- c(
 '/Users/matthewnicholson/DHS/GPS files/Nigeria/1990/NGGE23FL/NGGE23FL.shp',
  '/Users/matthewnicholson/DHS/GPS files/Nigeria/2003/NGGE4BFL/NGGE4BFL.shp',
   '/Users/matthewnicholson/DHS/GPS files/Nigeria/2008/NGGE52FL/NGGE52FL.shp',
    '/Users/matthewnicholson/DHS/GPS files/Nigeria/2010/NGGE61FL/NGGE61FL.shp',
     '/Users/matthewnicholson/DHS/GPS files/Nigeria/2013/NGGE6AFL/NGGE6AFL.shp',
      '/Users/matthewnicholson/DHS/GPS files/Nigeria/2015/NGGE71FL/NGGE71FL.shp',
       '/Users/matthewnicholson/DHS/GPS files/Nigeria/2018/NGGE7BFL/NGGE7BFL.shp',
        '/Users/matthewnicholson/DHS/GPS files/Nigeria/2021/NGGE81FL/NGGE81FL.shp'
      ) #list of gps files (get file doesn't work for these yet)
st_list <- lapply(st_file_list, st_read) #make a list
names(st_list) <- paste0("st_", year_list) #name with year
#repeat for ir (also load wi where not included)
ir_file_list <- get_file("Nigeria_DHS", year_list, "IR")
ir_list <- lapply(ir_file_list, readRDS)
names(ir_list) <- paste0("ir_", year_list)
#v106 = highest education, 9 = NA, v005 = sample weight, v001 = cluster number

#ultimately want a data frame with these vars: year, cluster number, mean education, geometry
#need to get cluster weighted mean of v106 for each survey file, then merge to respective gps file by v001
ir_summary <- vector("list", length(ir_list))
names(ir_summary) <- names(ir_list)
st_summary <- vector("list", length(st_list))
names(st_summary) <- names(st_list)

for(i in seq_along(st_list)){
  st <- st_list[[i]]
  st_summary[[i]] <- st |> 
    dplyr::select(DHSCLUST, geometry) |> 
    dplyr::rename(cluster = DHSCLUST)
}
for (i in seq_along(ir_list)) {
  df <- ir_list[[i]]
  
  ir_summary[[i]] <- df |>
    dplyr::select(v001, v106, v005) |>
    dplyr::rename(cluster = v001, education = v106, wt = v005) |>
    dplyr::mutate(
      education = dplyr::na_if(education, 9),  # convert 9 -> NA
      wt = wt / 1e6                            # scale DHS weights (optional)
    ) |>
    dplyr::group_by(cluster) |>
    dplyr::summarize(
      weighted_education = sum(wt * education, na.rm = TRUE) / sum(wt[!is.na(education)], na.rm = TRUE),
      unweighted_n = dplyr::n(),
      total_weight = sum(wt, na.rm = TRUE),
      .groups = "drop"
    )
}
for(i in seq_along(st_summary)){
  df <- ir_summary[[i]]
  st <- st_summary[[i]] |> 
    st_join(df,by=cluster)
}
