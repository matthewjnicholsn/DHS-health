rm(list=ls())
# pre set up
setwd("~/DHS")
source('get_file_function.R')
country_list <- c("Nigeria","Ethiopia","Ghana","Kenya","DRC")
year_list <- c(c(1990,2008,2024),c(2000,2011,2019),c(1988,2008,2022),c(1989,2003,2022),c(2007,2013.5,2023.5))
hr_file_list <- vector('list',length(countries))
hr_data_list <- vector('list',length(countries))

#get the data
for(i in seq_along(country_list)){
  for(j in seq_along(years[[i]])){
  hr_file_list[[i]][j] <- get_file(countries = country_list[[i]], years = year_list[[i]][j], surveys = "HR")
  if(is.null(hr_file_list[[i]][j]) == T){
    message("no file found for ",country_list[[i]]," ",year_list[[i]][j])
    stop()
  }
  hr_data_list[[i]][j] <- readRDS(hr_file_list[[i]][j])
  if(is.null(hr_data_list[[i]][j]) == T){
  message("file could not be read for", country_list[[i]]," ",year_list[[i]][j])
  }
  }
}


#run necessary analysis
#will start with an example for Nigeria
library(dplyr)
library(survey)
ng_file_list <- c("/Users/matthewnicholson/DHS/DHS_surveys_rds_organized/Nigeria/1990/NGHR21DT/Nigeria_DHS_NG_1990_DHS_04072025_2113_219655_NGHR21DT_NGHR21FL.Rds",
"/Users/matthewnicholson/DHS/DHS_surveys_rds_organized/Nigeria/2008/NGHR53DT/Nigeria_DHS_NG_2008_DHS_04072025_2113_219655_NGHR53DT_NGHR53FL.Rds",
"/Users/matthewnicholson/DHS/DHS_surveys_rds_organized/Nigeria/2024/NGHR8BDT/Nigeria_DHS_NG_2024_DHS_12192025_2031_219655_NGHR8BDT_NGHR8BFL.rds")
ng_years <- c(1990,2008,2024)
results <- tibble()
for(i in seq_along(ng_years)){
  ng <- readRDS(ng_file_list[[i]])
  if(!"hv270" %in% names(ng)){
    ng_wi <- readRDS(get_file(countries = "Nigeria", years = ng_years[[i]], surveys = "WI")) |> 
      rename(hv270 = wlthind5,
      hhid = whhid) |> 
      select(hhid,hv270)
  }
  else{
  ng_weights <- ng$hv005/1000000
  ng <- ng |> 
    select(hhid,hv001,hv005,hv024,hv025) |> 
    left_join(ng_wi, by = "hhid")
  ng_svy <- svydesign(data = ng,  ids =  ~hv001, strata = ~hv025, weights = ng_weights)
  results$mean_wealth <- svymean(ng$hv270, design = ng_svy, na.rm = T)
  }
  }


