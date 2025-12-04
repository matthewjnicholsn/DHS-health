
ng_90_cov <- read.csv('/Users/matthewnicholson/DHS/GPS files/Nigeria/1990/NGGC22FL/NGGC22FL.csv')
file_list <- get_file(countries = "Nigeria", years = 1990, surveys = c("HR","BR","PR"))
ng_90_hr <- readRDS(file_list[[1]]); ng_90_br <- readRDS(file_list[[2]]); ng_90_pr <- readRDS(file_list[[3]])

# b3
# Date of birth of child (CMC)
# b5
# Child is alive (1 = Yes, 0 = No)
# b7
# Age at death in months (imputed)
# v008
# Date of interview (CMC)
# v005
# Woman’s individual sample weight
ng_90_chmort <- ng_90_br |> 
  select(c("b3",'b5','b7','v008','v005'))
 