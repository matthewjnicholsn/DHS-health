libraries <- c("haven", "dplyr", "stringr", "survey", "beepr", "stringr")
lapply(libraries, require, character.only = T)
source("/Users/matthewnicholson/DHS/get_file_function.R")
source("/Users/matthewnicholson/DHS/admort_scripts/simplified_admort_func.R")
source("/Users/matthewnicholson/DHS/patch_dhsrate_functions.R")
country <- c("Chad_DHS","Congo_DHS", "DRC_DHS", "Ethiopia_DHS", "Ghana_DHS",
             "Kenya_DHS","Nigeria_DHS", "Zimbabwe_DHS")
year <- c(1990:2021)
survey <- c("BR", "IR")
data <- readRDS(file = "/Users/matthewnicholson/DHS/DHS_surveys_rds_organized/Congo_DHS/2005/CGBR51DT/Congo_DHS_CG_2005_DHS_08072025_1919_219655_CGBR51DT_CGBR51FL.Rds")
file_list <- get_file(country, year, survey)
admort_results <- ls()
for(i in seq_along(file_list)){
  ir_data <- readRDS(file_list[i])
  indicators <- c("ASMR", "AAMR")
  patch_dhsrate_functions(indicator_list = str_to_upper(indicators))
  for(j in indicators){indicator <- indicators[j]
    admort_results[[indicator]] <- as_tibble(simplified_admort_func(ir_data, Indicator = indicator))
  }
}
beep()
chmort_results <- ls()

for(i in seq_along(file_list)){
  readRDS(file_list[i])
  chmort_results[i] <- as_tibble(chmort(file_list[i]))
}
beep()