

for(j in seq_along(years[[i]])){
  #read in both files and merge them
  if((length(wealth_file_list[[i]])==length(chmort_file_list[[i]])) == F){
    message("lists "[[i]], " of different lengths")
  }
  else{
    wealth_file <- read.csv(wealth_file_list[[i]][j])
    mort_file <- read.csv(chmort_file_list[[i]][j])
    gini_file <- read.csv(gini_results_file_list[[i]][j])
    combine_file <- wealth_file |> 
      left_join(mort_file, by = "cluster") |> 
      left_join(gini_file, by = "cluster")
    combine_file_list[[i]][[j]] <- combine_file
  }
}
