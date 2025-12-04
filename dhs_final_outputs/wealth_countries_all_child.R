wealth_res_list[[i]] <- vector("list", length(years[[i]]))
wealth_file_list[[i]] <- vector("list", length(years[[i]]))

for(j in seq_along(years[[i]])){

  tryCatch({

    message("Processing ", countries[[i]], 
            " year ", years[[i]][j])

    hr_data <- readRDS(hr_file_list[[i]][j])

    if(is.null(hr_data) || nrow(hr_data) == 0){
      warning("hr_data is empty for ", countries[[i]], " ", years[[i]][j])
    }

if(!"hv270" %in% names(hr_data)){

      message("hv270 missing in hr file, using wi file")
   shp_file <- st_read(gps_file_list[[i]][j]) |>
        rename(cluster = DHSCLUST) |>
        select(cluster, geometry) 

      gps_data <- readRDS(wi_file_list[[i]][j]) |>
        select(whhid, wlthind5) |>
        rename(hhid = whhid, hv270 = wlthind5) |>
        # Join WI data to HR data (which contains hv001, hv005, and hv025)
        left_join(hr_data, by = "hhid", relationship = "one-to-many") |> 
        # 1. Group only by the Cluster ID (hv001) for the wealth calculation.
        #    The hv025 column is *retained* because it is not summarized.
        dplyr::group_by(hv001) |>
        
        dplyr::summarize(
          weighted_wealth = sum(hv005 * hv270, na.rm = TRUE) / 
                            sum(hv005[!is.na(hv270)], na.rm = TRUE),
          unweighted_n = n(),
          total_weight = sum(hv005, na.rm = TRUE),
          
          # 2. Retain the Urban/Rural status using first(hv025).
          #    Since all values are identical within the cluster, first() is safe.
          urban_rural_code = first(hv025), 
          .groups = "drop"
        ) |>
        
        # 3. Convert the code to a readable label for clarity later
        dplyr::mutate(
            urban_rural = dplyr::case_when(
                urban_rural_code == 1 ~ "Urban",
                urban_rural_code == 2 ~ "Rural",
                TRUE ~ "Unknown"
            ),
            hv001 = as.numeric(hv001),
            year = years[[i]][j],
            country = countries[[i]]
        ) |> 
        dplyr::rename(cluster = hv001) |> 
           left_join(shp_file, by = "cluster") |> 
        st_as_sf()
    

} else {

      message("Processing with hr_data, hv270 is present")

      shp_file <- st_read(gps_file_list[[i]][j]) |>
        rename(cluster = DHSCLUST) |>
        select(cluster, geometry)

      gps_file <- hr_data |>
        # 1. Select hv025 along with other necessary variables
        dplyr::select(hv001, hv005, hv270, hv025) |>
        
        # 2. Group only by the Cluster ID (hv001) for the wealth calculation.
        dplyr::group_by(hv001) |> 
        
        dplyr::summarize(
          weighted_wealth = sum(hv005 * hv270, na.rm = TRUE) / 
                            sum(hv005[!is.na(hv270)], na.rm = TRUE),
          unweighted_n = n(),
          total_weight = sum(hv005, na.rm = TRUE),
          
          # 3. Retain the Urban/Rural status using first(hv025).
          urban_rural_code = first(hv025), 
          .groups = "drop"
        ) |>
        
        # 4. Convert the code to a readable label
        dplyr::mutate(
            urban_rural = dplyr::case_when(
                urban_rural_code == 1 ~ "Urban",
                urban_rural_code == 2 ~ "Rural",
                TRUE ~ "Unknown"
            ),
            hv001 = as.numeric(hv001),
            year = years[[i]][j],
            country = countries[[i]]
        ) |> 
        dplyr::rename(cluster = hv001) |> 
         left_join(shp_file, by = "cluster") |> 
        st_as_sf()
  
}

    file_name <- paste0("wealth_clust_",countries[[i]],"_",years[[i]][j],".csv")
    wealth_file_list[[i]][j] <- file_name
    wealth_res_list[[i]][[j]] <- gps_file
    write.csv(gps_file, file = file_name, row.names = FALSE)

}, error = function(e){

    message("Error for ", countries[[i]], " ", years[[i]][j])
    message("Message: ", e$message)

    cat("Country:", countries[[i]], "\n",
        "Year:", years[[i]][j], "\n",
        "Error:", e$message, "\n\n",
        file = "wealth_error_log.txt",
        append = TRUE)
  })
}
