library(dplyr)
source('get_file_function.R')

countries <- c("Nigeria", "Ethiopia", "DRC", "Ghana", "Kenya")
years <- list(c(1990,2003,2008,2013,2018),c(2000,2005,2011,2016,2019),c(2007,2013.5),
              c(1993,1998,2003,2008,2014,2022),c(2003,2008.5,2014,2022))

hr_file_list <- vector('list',length(countries))
wi_file_list <- vector('list',length(countries))
weighted_wealth_results <- vector('list',length(countries))

for(i in seq_along(countries)){
  hr_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "HR")
  wi_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "WI") 
}

for(i in seq_along(countries)){
  message("processing country ",countries[[i]])
  for(j in seq_along(years[[i]])){
    message("year ",years[[i]][j])
    hr_data <- readRDS(hr_file_list[[i]][j])
    if(!"hv270" %in% names(hr_data)){
      message("hv270 missing, proceeding with wi join")
      wi_data <- readRDS(wi_file_list[[i]][j]) |> 
        rename(hv270 = wlthind5,
               hv271 = wlthindf,
               hhid = whhid)
      hr_data <- left_join(hr_data,wi_data, by = "hhid") |> 
        select(hv270,hv005,hv024,hv025) |> 
        group_by(hv024) |>
        summarize(
          weighted_wealth = sum(hv005 * hv270, na.rm = TRUE) / 
                            sum(hv005[!is.na(hv270)], na.rm = TRUE),
          unweighted_n = n(),
          total_weight = sum(hv005, na.rm = TRUE),
          .groups = "drop")
      weighted_wealth_results[[i]][[j]] <- hr_data
    }
    else{
      message("hv270 is present, proceeding")
      hr_data <- hr_data |> 
        select(hv270,hv005,hv024,hv025) |> 
        group_by(hv024) |>
        summarize(
          weighted_wealth = sum(hv005 * hv270, na.rm = TRUE) / 
                            sum(hv005[!is.na(hv270)], na.rm = TRUE),
          unweighted_n = n(),
          total_weight = sum(hv005, na.rm = TRUE),
          .groups = "drop")
      weighted_wealth_results[[i]][[j]] <- hr_data
    }
  }
}
