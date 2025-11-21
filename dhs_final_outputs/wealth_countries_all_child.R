lapply(c("dplyr","ggplot2","viridis","sf","DHS.rates","haven"),require,character.only=T)
source("get_file_function.R")
year_list <- c(1990,2003,2008,2013,2018)
hr_file_list <- get_file(countries = "Nigeria_DHS", years = c(1990,2003,2008,2013,2018), surveys = "HR")
br_file_list <- get_file(countries = "Nigeria_DHS", years = c(1990,2003,2008,2013,2018), surveys = "BR")
shp_file_list <- c(
                    '/Users/matthewnicholson/DHS/GPS files/Nigeria/1990/NGGE23FL/NGGE23FL.shp',
                    '/Users/matthewnicholson/DHS/GPS files/Nigeria/2003/NGGE4BFL/NGGE4BFL.shp',
                    '/Users/matthewnicholson/DHS/GPS files/Nigeria/2008/NGGE52FL/NGGE52FL.shp',
                    '/Users/matthewnicholson/DHS/GPS files/Nigeria/2013/NGGE6AFL/NGGE6AFL.shp',
                    '/Users/matthewnicholson/DHS/GPS files/Nigeria/2018/NGGE7BFL/NGGE7BFL.shp'
                  )
ng_90_wi <- read_dta("/Users/matthewnicholson/DHS/DHS_surveys/Nigeria_DHS/NG_1990_DHS_04072025_2113_219655/NGWI21DT/NGWI21FL.DTA") |> 
  rename(hhid=whhid,
         hv270 = wlthind5) 
#check for state naming

#calculate weighted wealth index means per state for each year
wealth_results <- list()
shp_results <- list()
for(i in seq_along(hr_file_list)){
  wealth_results[[i]] <- readRDS(hr_file_list[[i]])
  #condition for 1990
  if(i == 1){
    shp_file <- st_read(shp_file_list[[i]]) |> 
      dplyr::rename(hv001 = DHSCLUST) |> 
      dplyr::select(hv001,geometry)
    wealth_results[[i]]<- wealth_results[[i]] |> 
      left_join(ng_90_wi, by = "hhid") |> 
      select(c("hv001","hv005","hv270"
    # ,"shstate"
    )) |> 
      group_by(hv001) |> 
      summarize(
        weighted_wealth = sum(hv005 * hv270, na.rm = TRUE) / sum(hv005[!is.na(hv270)], na.rm = TRUE),
    unweighted_n = n(),
    total_weight = sum(hv005, na.rm = TRUE),
    .groups = "drop"
  )  |> 
  mutate(hv001 = as.numeric(hv001)) |> 
      mutate(year = year_list[i]) 
  
    shp_results[[i]] <- shp_file |> 
      left_join(wealth_results[[i]], by = "hv001")
  }
  else{
    shp_file <- st_read(shp_file_list[[i]]) |> 
      dplyr::rename(hv001 = DHSCLUST) |> 
      dplyr::select(hv001,geometry)
    wealth_results[[i]]<- wealth_results[[i]] |> 
      select(c("hv001","hv005","hv270"
    # ,"shtate"
    )) |> 
      group_by(hv001) |> 
      summarize(
        weighted_wealth = sum(hv005 * hv270, na.rm = TRUE) / sum(hv005[!is.na(hv270)], na.rm = TRUE),
    unweighted_n = n(),
    total_weight = sum(hv005, na.rm = TRUE),
    .groups = "drop"
  )  |> 
  mutate(hv001 = as.numeric(hv001)) |> 
  mutate(year = year_list[i])
   shp_results[[i]] <- shp_file |> 
      left_join(wealth_results[[i]], by = "hv001")
  }
}
wealthclust_yrs <- bind_rows(wealth_results)
wealthclust_shp_yrs <- bind_rows(shp_results)
plot_list <- list()
for(i in seq_along(year_list)){
  year_shp <- year_list[[i]]
  wealth_clust_shp_temp <- wealthclust_shp_yrs |> 
    filter(year == year_shp)
  plot_list[[i]] <- ggplot(data = wealth_clust_shp_temp) +
    geom_sf(aes(color = weighted_wealth), size = 3)+
  scale_color_viridis(option = "viridis", name = "Mean weighted wealth index (1-5)")+
theme_minimal()+
labs(title = "cluster-level mean weighted welath index ")
}
# wealthclust_yrs_model <- glm(weighted_wealth ~ year, data = wealthclust_yrs)
# summary(wealthclust_yrs_model)
# plot(wealthclust_yrs_model)


#example for loop debug
shp_file_90 <- st_read(shp_file_list[[1]]) |> 
  dplyr::rename(hv001 = DHSCLUST) |> 
  dplyr::select(hv001,geometry)
shp_1990 <- wealthclust_yrs |> 
  filter(year == 1990) |> 
  left_join(shp_file_90, by = "hv001") |> 
  st_as_sf()
p1 <- ggplot(data = shp_1990) +
    geom_sf(aes(color = weighted_wealth), size = 3)+
  scale_color_viridis(option = "viridis", name = "Mean weighted wealth index (1-5)")+
theme_minimal()+
labs(title = "cluster-level mean weighted welath index (Nigeria 1990)")
ggsave
plot(p1)
