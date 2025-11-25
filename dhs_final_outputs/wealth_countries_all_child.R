
#calculate weighted wealth index means per state for each year

for(j in seq_along(years[[i]])){
  hr_data <- readRDS(hr_file_list[[i]][j])
  if(is.null(hr_data$hv270) == T){
    wi_data <- readRDS(wi_file_list[[i]][j]) |> 
      select(c("whhid","wlthind5")) |> 
      rename(hhid = whhid,
             hv270= wlthind5) |> 
      left_join(hr_data, by = "hhid", relationship = "one-to-many")
    gps_file <- st_read(gps_file_list[[i]][j]) |> 
      dplyr::rename(hv001 = DHSCLUST) |> 
      select(c("hv001","geometry")) |> 
      left_join(wi_data, by = "hv001") |> 
      select(c("hv001","hv005","hv270")) |> 
      group_by(hv001) |> 
      summarize(
        weighted_wealth = sum(hv005 * hv270, na.rm = TRUE) / sum(hv005[!is.na(hv270)], na.rm = TRUE),
    unweighted_n = n(),
    total_weight = sum(hv005, na.rm = TRUE),
    .groups = "drop"
  )  |> 
  mutate(hv001 = as.numeric(hv001)) |> 
      mutate(year = years[[i]][j]) 
  
  file_name <- paste0("wealth_clust_",countries[[i]],"_",years[[i]][j],"_",".csv")
  wealth_file_list[[i]][j] <- file_name
  write.csv(gps_file, file = file_name)
  }

  else{
    gps_file <- st_read(gps_file_list[[i]][j]) |> 
      dplyr::rename(hv001 = DHSCLUST) |> 
      dplyr::select(c("hv001", "geometry"))
    hr_data <- hr_data |> 
      select(c("hv001","hv005","hv270")) |> 
      left_join(gps_file, by = "hv001", relationship = "many-to-one") |> 
      group_by(hv001) |> 
       summarize(
        weighted_wealth = sum(hv005 * hv270, na.rm = TRUE) / sum(hv005[!is.na(hv270)], na.rm = TRUE),
    unweighted_n = n(),
    total_weight = sum(hv005, na.rm = TRUE),
    .groups = "drop"
  )  |> 
  mutate(hv001 = as.numeric(hv001)) |> 
  mutate(year = years[[i]][j])
  file_name <- paste0("wealth_clust_",countries[[i]],"_",years[[i]][j],"_",".csv")
  wealth_file_list[[i]][j] <- file_name
  write.csv(gps_file, file = file_name)
  }
}