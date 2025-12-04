

combine_file_list[[i]] <- vector("list", length(years[[i]]))
for (j in seq_along(years[[i]])) {
  # read in both files and merge them
  if (length(wealth_res_list[[i]]) != length(chmort_res_list[[i]])) {
    message("lists ", i, " of different lengths")
  } else {
    wealth_file <- wealth_res_list[[i]][[j]] |>
      dplyr::select(cluster, weighted_wealth, geometry, year, country) |>
      dplyr::mutate(cluster = as.character(cluster))
    mort_file <- chmort_res_list[[i]][[j]] |>
      dplyr::select(cluster, prob, year, country) |>
      dplyr::mutate(cluster = as.character(cluster))
    gini_file <- gini_res_list[[i]][[j]] |>
      dplyr::mutate(cluster = as.character(cluster))
    combine_file <- wealth_file |>
      dplyr::left_join(mort_file, by = c("cluster", "year", "country")) |>
      dplyr::left_join(gini_file, by = "cluster")
    combine_file_list[[i]][[j]] <- combine_file
  }
}
