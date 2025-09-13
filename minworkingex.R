library(dplyr)
library(ggplot2)

#load in the files
CCHS <- read.csv('/Users/matthewnicholson/Downloads/2019-2020 CCHS_Public available_CSV/Data_Donn‚es/pumf_cchs.csv')
#rename var
#health overall self rated
CCHS <- CCHS %>% 
  rename(hlth = GEN_005, geog = GEOGPRV, wt = WTS_M)

aggregate(hlth ~ geog, FUN = mean, data = CCHS)
