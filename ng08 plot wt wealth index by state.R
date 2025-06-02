
library(haven)
library(dplyr)
library(forcats)
library(sf)
library(ggplot2)
library(viridis)
library(patchwork)

#load in the household data
NG08 <- read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2008_DHS_04072025_2113_219655/NGHR53DT/NGHR53FL.DTA")
#read in the shape file
SF <- read_sf("/Users/matthewnicholson/Downloads/nigeria-lgas/new_lga_nigeria_2003.shp")

#new df with states encoded
state_mapping <- read.csv("/Users/matthewnicholson/Downloads/state encoding.csv")

#subset data with only state, region, and wealth index
HR <- NG08[, c("hv024","shstate", "hv270", "hv005")]

#this code isn't necessary but I'm leaving it in for later use 
#rename shstate variable and create factor
#HR$shstate <- factor(HR$shstate, 
# levels = c(for (i in seq(10,370, by = 10))
# labels = c("Sokoto", "Zamfara", "Katsina", "Jigawa",
#    "Yobe", "Borno", "Adamawa", "Gombe", 
# "Bauchi", "Kano", "Kaduna", "Kebbi", 
#    "Niger", "Abuja", "Nassarawa", "Plateau",
#    "Taraba", "Benue", "Kogi", "Kwara", "Oyo", 
#    "Osun", "Ekiti", "Ondo", "Edo","Anambra","Enugu",
#    "Ebonyi","Cross River","Akwa Ibom","Abia","Imo",
#    "Rivers","Bayelsa","Delta","Lagos","Ogun")))

#join state codes to shapefile
SF <- SF %>% 
  left_join(state_mapping, by = c('STATE' = 'STATE'))

#calculate means of wealth index
WI <- HR %>% 
  group_by(shstate) %>% 
  summarise(WI = weighted.mean(hv270, hv005/1000000, na.rm - TRUE))

#join WI to SF
WI <- SF %>% 
  left_join(WI, by = c("label" = "shstate"))


#aggregate LCA geom to state
aggregate <- WI %>% 
  group_by(STATE) %>% 
  summarize(geometry = st_union(geometry),
            WI = mean(WI, na.rm = TRUE))
sf_use_s2(FALSE)
#get centroids for labels in plot
aggregate <- aggregate %>% 
  mutate(centroid = st_centroid(geometry))

#plot the data
ggplot(data = aggregate) +
  geom_sf(aes(fill = WI), color = "white") +
  geom_sf_text(aes(label = STATE, geometry = centroid),
               size = 3,
               check_overlap = F) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'bottom') +
  scale_fill_viridis_c(option = 'C', direction = 1) +
  labs(title = "Mean Wealth Index by State in Nigeria",  
       fill = "Mean wealth index (1-5)")

#other option for color gradient
ggplot(data = aggregate) +
  geom_sf(aes(fill = WI), color = "white") +
  geom_sf_text(aes(label = STATE, geometry = centroid),
               size = 3,
               check_overlap = FALSE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'bottom') +
  scale_fill_gradient(low = "red", high = "yellow") +
  labs(title = "Mean Wealth Index by State in Nigeria",  
       fill = "Mean wealth index (1-5)")
