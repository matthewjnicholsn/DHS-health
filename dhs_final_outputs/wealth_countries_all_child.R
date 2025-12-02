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

      wi_data <- readRDS(wi_file_list[[i]][j]) |>
        select(whhid, wlthind5) |>
        rename(hhid = whhid, hv270 = wlthind5) |>
        left_join(hr_data, by = "hhid", relationship = "one-to-many")

      gps_file <- st_read(gps_file_list[[i]][j]) |>
        rename(hv001 = DHSCLUST) |>
        select(hv001, geometry) |>
        left_join(wi_data, by = "hv001") |>
        group_by(hv001) |>
        summarize(
          weighted_wealth = sum(hv005 * hv270, na.rm = TRUE) / 
                            sum(hv005[!is.na(hv270)], na.rm = TRUE),
          unweighted_n = n(),
          total_weight = sum(hv005, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(hv001 = as.numeric(hv001),
               year = years[[i]][j],
               country = countries[[i]]) |> 
        rename(cluster = hv001)

    } else {

      message("Processing with hr_data, hv270 is present")

      gps_file <- st_read(gps_file_list[[i]][j]) |>
        rename(hv001 = DHSCLUST) |>
        select(hv001, geometry)

      gps_file <- hr_data |>
        select(hv001, hv005, hv270) |>
        left_join(gps_file, by = "hv001", relationship = "many-to-one") |>
        group_by(hv001) |>
        summarize(
          weighted_wealth = sum(hv005 * hv270, na.rm = TRUE) / 
                            sum(hv005[!is.na(hv270)], na.rm = TRUE),
          unweighted_n = n(),
          total_weight = sum(hv005, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(hv001 = as.numeric(hv001),
               year = years[[i]][j],
               country = countries[[i]])|> 
        rename(cluster = hv001)
    }

    file_name <- paste0("wealth_clust_",countries[[i]],"_",years[[i]][j],".csv")
    wealth_file_list[[i]][j] <- file_name
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
