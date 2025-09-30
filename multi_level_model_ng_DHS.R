########### MULTI-LEVEL MODELLING ##################
lib_list <- c("dplyr", "haven", "sf", "ggplot2")
lapply(lib_list, require, character.only = T) 
rm(lib_list)
##1. The household weight (HV005) from the household recode (HR) dataset, or the woman weight (V005)
from the woman recode (IR) dataset.
source("/Users/matthewnicholson/DHS/get_file_function.R")
ng_2018_hr <- readRDS(get_file(countries = "Nigeria_DHS", years = 2018, surveys = "HR")) |> 
  rename(hweight = hv005,
  cluster = hv001,
  stratum = hv022,
  domain = hv024
)
# #  The total number of completed/interviewed clusters 𝑎ℎ
# 𝑐 (clusters with at least one interviewed
# household) in stratum h for all strata.

clusters_int <- ng_2018_hr |> 
  filter(hv015 == 1) |> 
  group_by(hv001, .drop = T)
length(unique(clusters_int$hv001))
# > 1389

# 3. The total number of households Mh in stratum h for all strata. In a typical DHS survey, this can be found
# in Table A.1 in Appendix A on the sampling design.
hh_er_strata <- read.csv("/Users/matthewnicholson/Downloads/strata_hh_ng_2018.csv") |> 
  select(strata,urban.households,rural.households) 
hh_er_strata$urban.households <- as.numeric(gsub(",","",hh_er_strata$urban.households))
hh_er_strata$rural.households <- as.numeric(gsub(",", "",hh_er_strata$rural.households))
hh_er_strata <- hh_er_strata |> mutate(hh = urban.households + rural.households) |> 
  na.omit()

# 4. Total number of households in the country

# 40883666.430


# 6. total n of households that completed the survey

hh_comp <- ng_2018_hr |> 
  filter(hv015 == 1)
length(unique(hh_comp$hhid))

# 40427

# 7. total n of households in the country at the time of the survey 
# 40883666.430