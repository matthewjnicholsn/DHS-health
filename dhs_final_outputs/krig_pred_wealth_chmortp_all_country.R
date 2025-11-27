lapply(c("dplyr","ggplot2","viridis","sf","DHS.rates","haven", "DHS.rates","viridis","gstat", "sjlabelled","automap"),require,character.only=T)
#get all gps and dhs files into scope
source("get_file_function.R")
source("dhs_final_outputs/get_rds_func.R")
countries <- c("Nigeria", "Ethiopia", "DRC", "Ghana", "Kenya")
gps_file_list <- list()
br_file_list <- list()
hr_file_list <- list()
wi_file_list <- list()
pr_file_list <- list()
chmort_file_list <- vector("list", 5)
wealth_file_list <- vector("list", 5)
gini_results_file_list <- vector('list', 5)
outline_file_list <- c("/Users/matthewnicholson/Downloads/nigeria-lgas/new_lga_nigeria_2003.shp",
                       "/Users/matthewnicholson/Downloads/ethiopiaregion/Eth_Region_2013.shp",
                       "/Users/matthewnicholson/Downloads/cod_admbnda_rgc_itos_20190911_shp/cod_admbnda_adm0_rgc_itos_20190911.shp",
                       "/Users/matthewnicholson/Downloads/cod_admbnda_rgc_itos_20190911_shp/cod_admbnda_adm0_rgc_itos_20190911.shp",
                       "/Users/matthewnicholson/Downloads/kenyan-counties/County.shp")
vgm_fit_list <- vector("list", length(gps_file_list))
vgm_emp_list <- vector("list", length(gps_file_list))
krig_plot_list <- vector("list", length(gps_file_list))
krig_var_plot_list <- vector("list", length(gps_file_list))
years <- list(c(1990,2003,2008,2013,2018),c(2000,2005,2011,2016,2019),c(2007,2013.5),
              c(1993,1998,2003,2008,2014,2022),c(2003,2008.5,2014,2022))

for(i in seq_along(countries)){
  gps_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), file_type = "gps")
  br_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]),  surveys = "BR")
  hr_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "HR")
  wi_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "WI")
  pr_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "PR")
}

#basic script structure will be

for(i in seq_along(countries)){
  #first calculate cluster level chmort
  # message("Running mortality script for:", countries[[i]]".")
  
  # tryCatch(
  #   {
  #     source('dhs_final_outputs/chmort_countries_all_child.R')
  #   },
  #   error = function(e){
  #     message("Error in chmort script for", countries[[i]])
  #     message("Message:", e$message)
  #     cat(
  #       "Country:", countries[[i]], "\n",
  #       "Error:", e$message, "\n\n",
  #       file = "wealth_error_log.txt",
  #       append = T
  #     )
  #   },
  #   warning = function(w){
  #     message("Warning in chmort script for", countries[[i]])
  #     messaeg("Message: ", w$message)
  #   }
  # )


  #then calculate cluster level mean wealth
  
  # tryCatch(
  #   {
  #     source('dhs_final_outputs/wealth_countries_all_child.R')
  #   },
  #   error = function(e){
  #     message("Error in wealth script for", countries[[i]])
  #     message("Message:", e$message)
  #     cat(
  #       "Country:", countries[[i]], "\n",
  #       "Error:", e$message, "\n\n", #add file arg
  #       file = "wealth_error_log.txt",
  #       append = T
  #     )
  #   },
  #   warning = function(w){
  #     message("Warning in wealth script for", countries[[i]])
  #     message("Message: ", w$message)
  #   }
  # )

  #Calculate gini coefficients for clusters
  source("/Users/matthewnicholson/DHS/dhs_final_outputs/gini_1990_2018.R")

  # #then run kriging models on chmort data
  # tryCatch({
  #   source("dhs_final_outputs/kriging_ng_1990_2018.R")
  # },
  # error = function(e){
  #     message("Error in chmort kriging script for", countries[[i]])
  #     message("Message:", e$message)
  #     cat(
  #       "Country:", countries[[i]], "\n",
  #       "Error:", e$message, "\n\n",
  #       file = "chmort_krig_error_log.txt",
  #       append = T
  #     )
  #   },
  #   warning = function(w){
  #     message("Warning in chmort kriging script for", countries[[i]])
  #     messaeg("Message: ", w$message)
  #   }
  # )
  # #then krige
  # source('dhs_final_outputs/kriging_wealth_ng_1990_2018.R')
  
  }
