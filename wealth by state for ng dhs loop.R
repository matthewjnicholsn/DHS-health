rm(list = ls(all = TRUE))
library(haven)
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)

#need to fix the reprojection issue
suppressWarnings({
#read in shape file and state mapping 
state_mapping <- read.csv("/Users/matthewnicholson/Downloads/state encoding.csv")
SF <- read_sf("/Users/matthewnicholson/Downloads/nigeria-lgas/new_lga_nigeria_2003.shp")
#merge state-mapping and sf
SF <- SF %>% 
  left_join(state_mapping, by = "STATE")


#set filepaths for all files in used in the loop
file_paths <- c(
  "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2003_DHS_04072025_2113_219655/NGHR4BDT/NGHR4BFL.DTA",
  "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2008_DHS_04072025_2113_219655/NGHR53DT/NGHR53FL.DTA",
  "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2010_MIS_04072025_2114_219655/NGHR61DT/NGHR61FL.DTA",
  "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2013_DHS_04072025_2114_219655/NGHR6ADT/NGHR6AFL.DTA",
  "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2015_MIS_04072025_2115_219655/NGHR71DT/NGHR71FL.DTA", #MIS
  "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2018_DHS_04072025_2116_219655/NGHR7BDT/NGHR7BFL.DTA"
)

#begin the for loop

for (i in seq_along(file_paths)) {
   #read the household file
   HR <- read_dta(file_paths[i])
   
   #subset household file
   HR <- HR[, c("hv024", "shstate", "hv270", "hv005")]
   
   if (i == 1) {
     #2003 state mapping
     old_mapping <- c(
       "akwa ibom" = 1, "anambra" = 2, "bauchi" = 3, "edo" = 4, "benue" = 5,
       "borno" = 6, "cross river" = 7, "adamawa" = 8, "imo" = 9, "kaduna" = 10,
       "kano" = 11, "katsina" = 12, "kwara" = 13, "lagos" = 14, "niger" = 15,
       "ogun" = 16, "ondo" = 17, "oyo" = 18, "plateau" = 19, "rivers" = 20,
       "sokoto" = 21, "abia" = 22, "delta" = 23, "enugu" = 24, "jigawa" = 25,
       "kebbi" = 26, "kogi" = 27, "osun" = 28, "taraba" = 29, "yobe" = 30,
       "bayelsa" = 31, "ebonyi" = 32, "ekiti" = 33, "gombe" = 34, "nassarawa" = 35,
       "zamfara" = 36, "abuja (fct)" = 37
     )
     
     # >2008 state mapping
     new_mapping <- c(
       "sokoto" = 10, "zamfara" = 20, "katsina" = 30, "jigawa" = 40, "yobe" = 50,
       "borno" = 60, "adamawa" = 70, "gombe" = 80, "bauchi" = 90, "kano" = 100,
       "kaduna" = 110, "kebbi" = 120, "niger" = 130, "abuja" = 140, "nassarawa" = 150,
       "plateau" = 160, "taraba" = 170, "benue" = 180, "kogi" = 190, "kwara" = 200,
       "oyo" = 210, "osun" = 220, "ekiti" = 230, "ondo" = 240, "edo" = 250,
       "anambra" = 260, "enugu" = 270, "ebonyi" = 280, "cross river" = 290, "akwa ibom" = 300,
       "abia" = 310, "imo" = 320, "rivers" = 330, "bayelsa" = 340, "delta" = 350,
       "lagos" = 360, "ogun" = 370
     )
     
     # transform the shstate variable in the HR dataframe
     HR$shstate <- new_mapping[old_mapping[HR$shstate]]
   }
   
   
   #get weighted means of wealth index
   WI <- HR %>% 
     group_by(shstate) %>% 
     summarise(WI = weighted.mean(hv270, hv005 / 1000000, na.rm = TRUE))
   #join WI to SF
   WI <- SF %>% 
     left_join(WI, by = c("label" = "shstate"))
   
   #aggregate LCA geom to state
   aggregate <- WI %>% 
     group_by(STATE) %>% 
     summarize(geometry = st_union(geometry),
               WI = mean(WI, na.rm = TRUE))
   #this makes it work?
   sf_use_s2(FALSE)
   
   #get centroids for labels in plot
   aggregate <- aggregate %>% 
     mutate(centroid = st_centroid(geometry))
   
   #i guess for titles
   plot_title <- paste("Mean Wealth Index by State in Nigeria -", 2002 + i)  # adjusting for the year
   
   #make some maps
  p <- ggplot(data = aggregate) +
  geom_sf(aes(fill = WI), color = "white") +
  geom_sf_text(aes(label = STATE, geometry = centroid),
               size = 3,
               check_overlap = FALSE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'bottom') +
  scale_fill_viridis_c(option = 'C', direction = 1, limits = c(1, 5), oob = scales::squish) +  # Fixed limits
  labs(title = plot_title,  
       fill = "Mean wealth index (1-5)")
   
   #save the beauties
   ggsave(filename = paste0("Mean_Wealth_Index_", 2002 + i, ".png"), plot = p)
}
})
