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
ng_base_shp <- st_read("/Users/matthewnicholson/Downloads/ng_shp/ng.shp")
for(i in seq_along(year_list)){
  year_shp <- year_list[[i]]
  wealth_clust_shp_temp <- wealthclust_shp_yrs |> 
    filter(year == year_shp)
  if(nrow(wealth_clust_shp_temp) > 0){
    base_to_plot <- st_transform(ng_base_shp, st_crs(wealth_clust_shp_temp))
  } else {
    base_to_plot <- ng_base_shp
  }
  plot_list[[i]] <- ggplot() +
    geom_sf(data = base_to_plot, fill = "grey98", color = "grey60", size = 0.2) +
    geom_sf(data = wealth_clust_shp_temp, aes(color = weighted_wealth), size = 3) +
    scale_color_viridis(option = "viridis", name = "Mean weighted wealth index (1-5)") +
    theme_minimal() +
    labs(title = paste0("Cluster-level mean weighted wealth index (", year_shp, ")"))
}
# wealthclust_yrs_model <- glm(weighted_wealth ~ year, data = wealthclust_yrs)
# summary(wealthclust_yrs_model)
# plot(wealthclust_yrs_model)
p1 <- plot_list[[1]]
p2 <- plot_list[[2]]
p3 <- plot_list[[3]]
p4 <- plot_list[[4]]
p5 <- plot_list[[5]]

ggsave("cluster_level_wealth_ng_1990.png", plot = p1, scale = 2.0)
ggsave("cluster_level_wealth_ng_2003.png", plot = p2, scale = 2.0)
ggsave("cluster_level_wealth_ng_2008.png", plot = p3, scale = 2.0)
ggsave("cluster_level_wealth_ng_2013.png", plot = p4, scale = 2.0)
ggsave("cluster_level_wealth_ng_2018.png", plot = p5, scale = 2.0)

