
library(haven)
library(dplyr)
library(forcats)
library(sf)
library(ggplot2)
library(viridis)
library(patchwork)

ng_99_HR <- read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_1990_DHS_04072025_2113_219655/NGHR21DT/NGHR21FL.DTA")

ng99_wi <- read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_1990_DHS_04072025_2113_219655/NGWI21DT/NGWI21FL.DTA")
#remove centered wi
ng99_wi <- ng99_wi %>% 
  select(-wlthindf) %>% 
  rename(hhid = whhid)

#subset data with only state, and household id
ng_99_HR <- ng_99_HR[, c("hhid","shstate")]


#merge the wealth index to the household data
ng_99_HR <- merge(ng_99_HR, ng99_wi, by = "hhid")

#now we import the shapefile

SF_NG <- read_sf("/Users/matthewnicholson/Downloads/nigeria-lgas/new_lga_nigeria_2003.shp")

#new df with states encoded for shapefile
state_mapping <- read.csv("/Users/matthewnicholson/Downloads/state encoding.csv")

#join the state codes to the shape file
SF_NG <- SF_NG %>% 
  left_join(state_mapping, by = c('STATE' = 'STATE'))

#calculate means of wealth index
ng_99_HR <- ng_99_HR %>% 
  group_by(shstate) %>% 
  summarise(wlthind5 = mean(wlthind5, na.rm - TRUE))

#join WI to SF
WI <- SF %>% 
  left_join(WI, by = c("label" = "shstate"))



View(ng99_wi)
View(ng_99_HR)
