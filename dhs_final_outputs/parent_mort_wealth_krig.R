#CODING ALL DEPENDICES INTO ONE SCRIPT SO I CAN UNDERSTAND WHAT'S HAPPENING
# 
# ##### first we calculate child mort proability by cluster level in nigeria 1990-2018
#


#source all depencies
gps_list <- c('/Users/matthewnicholson/DHS/GPS files/Nigeria/1990/NGGE23FL/NGGE23FL.shp',
              '/Users/matthewnicholson/DHS/GPS files/Nigeria/2003/NGGE4BFL/NGGE4BFL.shp',
              '/Users/matthewnicholson/DHS/GPS files/Nigeria/2008/NGGE52FL/NGGE52FL.shp',
              '/Users/matthewnicholson/DHS/GPS files/Nigeria/2013/NGGE6AFL/NGGE6AFL.shp',
              '/Users/matthewnicholson/DHS/GPS files/Nigeria/2018/NGGE7BFL/NGGE7BFL.shp')
#first calculate cluster level chmort
source('dhs_final_outputs/chmort_countries_all_child.R')
#then run kriging models on chmort data
source("dhs_final_outputs/kriging_ng_1990_2018.R")
#then calculate cluster level mean wealth
source('dhs_final_outputs/wealth_ng_1990_2018.R')
#then krige
source('dhs_final_outputs/kriging_wealth_ng_1990_2018.R')

#plot the relevant plots for wealth
plot(krig_maps[[5]]);plot(krig_var_maps[[5]])
#plot for mort
plot(krig_plot_list[[5]]); plot(krig_var_plot_list[[5]])
#then run change models on the krigings data
#may want to do a co-kriging or a bayesian empirical kriging
# source('modeling spatio-temporal changes in wealth and child mortality in nigeria.R')
