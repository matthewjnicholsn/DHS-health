library(sf) ; library(dplyr) ; library(ggplot2)
year_list <- c(1988,1993,1998,2003,2008,2014,2022)
source('get_file_function.R')
gh_file_list  <- c(get_file(countries = "Ghana_DHS", years = year_list, surveys = "BR"))

gh_shp_list <- c('/Users/matthewnicholson/DHS/GPS files/Ghana/1993/GHGE33FL/GHGE33FL.shp' ,
                 '/Users/matthewnicholson/DHS/GPS files/Ghana/1998/GHGE42FL/GHGE42FL.shp' ,
                 '/Users/matthewnicholson/DHS/GPS files/Ghana/2003/GHGE4BFL/GHGE4BFL.shp' ,
                 '/Users/matthewnicholson/DHS/GPS files/Ghana/2008/GHGE5AFL/GHGE5AFL.shp' ,
                 '/Users/matthewnicholson/DHS/GPS files/Ghana/2014/GHGE71FL/GHGE71FL.shp' ,
                 '/Users/matthewnicholson/DHS/GPS files/Ghana/2022/GHGE8AFL/GHGE8AFL.shp')

gh_boundary_shp <- st_read('/Users/matthewnicholson/Downloads/gha_admbnda_gss_20210308_SHP/gha_admbndp_admALL_gss_itos_20210308.shp') |> 
  st_union() |> st_make_valid()

 