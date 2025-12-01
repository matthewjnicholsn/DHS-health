#script to model gini results and chmort results
#rq: does cluster-level inequality influence the odds of a death exposure?
#depends on partent script
gini_chmort_data <- vector("list", 5)
for(j in seq_along(years[[i]])){
  gini_data_temp <- read.csv(gini_results_file_list[[i]][j])
  gini_data_temp <- gini_data_temp |> 
    rename(cluster = Class)
  chmort_data_temp <- read.csv(chmort_file_list[[i]][j])
  chmort_data_temp <- chmort_data_temp |> 
    left_join(gini_data_temp, by = "cluster")
  gini_chmort_data[[i]][[j]] <- chmort_data_temp
}
