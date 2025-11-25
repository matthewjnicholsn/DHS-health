lapply(c("dplyr","ggplot2","viridis","sf","DHS.rates","haven", "DHS.rates","viridis","gstat", "sjlabelled","automap"),require,character.only=T)
#get all gps and dhs files into scope
source("get_file_function.R")
source("dhs_final_outputs/get_rds_func.R")
countries <- c("Nigeria", "Ethiopia", "DRC", "Ghana", "Kenya")
gps_file_list <- list()
br_file_list <- list()
hr_file_list <- list()
wi_file_list <- list()
chmort_file_list <- vector("list", 5)
wealth_file_list <- vector("list", 5)
years <- list(c(1990,2003,2008,2013,2018),c(2000,2005,2011,2016,2019),c(2007,2013.5),
              c(1993,1998,2003,2008,2014,2022),c(2003,2008.5,2014,2022))

for(i in seq_along(countries)){
  gps_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), file_type = "gps")
  br_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]),  surveys = "BR")
  hr_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "HR")
  wi_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "WI")
}

#basic script structure will be

for(i in seq_along(countries)){
  #first calculate cluster level chmort
  # source('dhs_final_outputs/chmort_countries_all_child.R')
  #then calculate cluster level mean wealth
  source('dhs_final_outputs/wealth_countries_all_child.R')
  # #then run kriging models on chmort data
  # source("dhs_final_outputs/kriging_ng_1990_2018.R")
  # #then krige
  # source('dhs_final_outputs/kriging_wealth_ng_1990_2018.R')
}
