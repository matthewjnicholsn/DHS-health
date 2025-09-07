library(sf)
library(ggplot2)
library(viridis)
library(haven)
library(dplyr)
et_sf <- read_sf("/Users/matthewnicholson/DHS/GPS files/Ethiopia/2000/ETGE42FL/ETGE42FL.shp")
et_hr <- readRDS(file = "/Users/matthewnicholson/DHS/DHS_surveys_rds_organized/Ethiopia_DHS/2000/ETHR41DT/Ethiopia_DHS_ET_2000_DHS_08072025_1921_219655_ETHR41DT_ETHR41FL.Rds")
et_wi <- as_tibble(read_dta("/Users/matthewnicholson/DHS/DHS_surveys/Ethiopia_DHS/ET_2000_DHS_08072025_1921_219655/ETWI41DT/ETWI41FL.DTA"))
View(et_sf)
View(et_hr)

et_hr <- et_hr %>% 
  rename(DHSCLUST = hv001) %>% 
  rename(whhid = hhid) %>% 
  left_join(et_wi %>% distinct(whhid, wlthind5), join_by(whhid),
            relationship = "many-to-many") %>% 
  left_join(et_sf %>% distinct(DHSCLUST, geometry), join_by(DHSCLUST), 
            relationship = "many-to-many") 


p1 <- ggplot(data = et_hr) +
  geom_sf(aes(fill = ), color = "black")
plot(p1)
# +
#   theme_void()+
#   scale_fill_viridis(option = "viridis")
# plot(p1)
# # +
# #   geom_sf(aes(fill = Gini), color = "white") +
# #   geom_sf_text(aes(label = region, geometry = centroid, color = "black"),
# #                size = 3,
# #                check_overlap = TRUE) +
# #   theme_void() +
# #   theme(plot.title = element_text(hjust = 0.5)) +
# #   theme(legend.position = "bottom") +
# #   scale_fill_viridis_c(option = "viridis", direction = -1, 
# #                        limits = c(0, 1), 
# #                        breaks = seq(0, 1, by = 0.2)) +
# #   scale_color_identity() +
# #   labs(title = "Wealth inequality by region in Ethiopia (2016)",
# #        fill = "GINI coefficient")
