# Initialize a list to hold the final dataframe for each country
country_panels <- list()

for (i in seq_along(countries)) {
  
  message("Combining data for: ", countries[[i]])
  
  # Create a temporary list to hold the merged years for THIS country
  year_stack <- list()
  
  for (j in seq_along(years[[i]])) {
    
    # 1. Retrieve the three dataframes from your existing lists
    #    Using tryCatch to skip years where data generation might have failed/is NULL
    try({
      df_wealth <- wealth_res_list[[i]][[j]]
      df_mort   <- chmort_res_list[[i]][[j]]
      df_gini   <- gini_res_list[[i]][[j]]
      
      # Check if all exist before proceeding
      if (!is.null(df_wealth) && !is.null(df_mort) && !is.null(df_gini)) {
        
        # 2. Standardize Key Types
        #    Crucial: convert cluster to character to ensure successful joins
        df_wealth$cluster <- as.character(df_wealth$cluster)
        df_mort$cluster   <- as.character(df_mort$cluster)
        df_gini$cluster   <- as.character(df_gini$cluster)
        
        # 3. The Merge
        #    We start with Wealth because it contains the geometry (sf object).
        #    We left_join the others.
        merged_year <- df_wealth %>%
          left_join(df_mort, by = c("cluster", "year", "country")) %>%
          left_join(df_gini, by = "cluster")
        
        # Add to the stack for this country
        year_stack[[j]] <- merged_year
      }
    }, silent = TRUE)
  }
  
  # 4. Collapse the stack into one dataframe (sf object) for the country
  #    bind_rows handles sf objects gracefully and fills missing columns with NA
  country_panels[[i]] <- do.call(rbind, year_stack)
  
  # Optional: Save the final Country Panel immediately
  saveRDS(country_panels[[i]], file = paste0("Final_Panel_", countries[[i]], ".rds"))
}

# Now, country_panels[[1]] is the complete dataset for Nigeria, [[2]] for Ethiopia, etc. 