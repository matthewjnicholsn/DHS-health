library(sf)
library(dplyr)

# Define the definitive column order that MUST be used for every year's data frame.
# This order is based on the columns created in your wealth, mortality, and gini scripts.
FINAL_COL_ORDER <- c(
    "cluster", "year", "country", 
    # Wealth columns
    "weighted_wealth", "unweighted_n", "total_weight", 
    "urban_rural_code", "urban_rural", 
    # Child Mortality column
    "prob", 
    # Gini column (assuming your Gini script names the result column "gini_value")
    "gini", 
    # Geometry must be last for the sf package to work best with rbind
    "geometry"
)

# Initialize a list to hold the final dataframe for each country
country_panels <- list()

for (i in seq_along(countries)) {
  
  country_name <- countries[[i]]
  message("Combining data for: ", country_name)
  
  year_stack <- list()
  
  for (j in seq_along(years[[i]])) {
    
    try({
      df_wealth <- wealth_res_list[[i]][[j]]
      df_mort   <- chmort_res_list[[i]][[j]]
      df_gini   <- gini_res_list[[i]][[j]]
      
      if (!is.null(df_wealth) && !is.null(df_mort) && !is.null(df_gini) && nrow(df_wealth) > 0) {
        
        # 1. Standardize Key Types
        df_wealth$cluster <- as.character(df_wealth$cluster)
        df_mort$cluster   <- as.character(df_mort$cluster)
        df_gini$cluster   <- as.character(df_gini$cluster)
        
        # 2. The Merge
        merged_year <- df_wealth %>%
          left_join(df_mort, by = c("cluster", "year", "country")) %>%
          left_join(df_gini, by = "cluster")
      
        # 3.COLUMN STANDARDIZATION & ORDERING 
        # a) Check for missing final columns (Gini value or Mortality Prob might be missing)
        # b) Add any missing columns with NA values
        for (col in FINAL_COL_ORDER) {
            if (!(col %in% names(merged_year)) && col != "geometry") {
                merged_year[[col]] <- NA
            }
        }
        
        # c) Select and order the columns exactly according to the defined list
        #    Note: st_sf will put 'geometry' last automatically, but we keep it in the list for safety.
        merged_year <- merged_year %>%
            dplyr::select(all_of(FINAL_COL_ORDER))

        # 4. GEOMETRY COERCION SAFETY CHECK
        if (!inherits(merged_year, "sf")) {
            if ("geometry" %in% names(merged_year)) {
                merged_year <- st_as_sf(merged_year, sf_column_name = "geometry")
                message("   -> Repaired SF class for year ", years[[i]][j])
            } else {
                warning("   -> Geometry missing for year ", years[[i]][j], ". Skipping.")
                merged_year <- NULL
            }
        }
        
        # 5. Add to the stack
        if (!is.null(merged_year)) {
            year_stack[[j]] <- merged_year
        }
      }
    }, silent = TRUE)
  }
  
  # 6. Collapse the stack using do.call(rbind, ...) on the now-consistent list
  if (length(year_stack) > 0) {
      final_panel <- do.call(rbind, year_stack)
      country_panels[[i]] <- final_panel
      message("Final panel for ", country_name, " successfully created.")
      
      saveRDS(final_panel, file = paste0("Final_Panel_", country_name, ".rds"))
      
  } else {
      warning("No data found for ", country_name, ". Panel set to NULL.")
      country_panels[[i]] <- NULL
  }
}