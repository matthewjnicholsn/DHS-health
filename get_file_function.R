library(fs)
library(stringr)

get_file <- function(countries, years, surveys, root = "/Users/matthewnicholson/DHS/DHS_surveys_rds_organized") {
  # Make sure all inputs are vectors
  countries <- as.character(countries)
  years <- as.character(years)
  surveys <- as.character(surveys)
  
  # Create all combinations
  combos <- expand.grid(country = countries, year = years, survey = surveys, stringsAsFactors = FALSE)
  
  # Helper function for single combo
  get_file_single <- function(country, year, survey) {
    dir_path <- path(root, country, year)
    if (!dir_exists(dir_path)) {
      warning("Directory ", dir_path, " does not exist.")
      return(NULL)
    }
    survey_dirs <- dir_ls(dir_path, type = "directory", regexp = survey)
    survey_files <- character()
    for (survey_dir in survey_dirs) {
      files <- dir_ls(survey_dir, regexp = paste0(".*", survey, ".*\\.Rds$"))
      survey_files <- c(survey_files, files)
    }
    if (length(survey_files) == 0) {
      survey_files <- dir_ls(dir_path, regexp = paste0(".*", survey, ".*\\.Rds$"))
    }
    if (length(survey_files) == 0) {
      warning("No files found for country=", country, ", year=", year, ", survey=", survey)
      return(NULL)
    }
    return(survey_files)
  }
  
  # Iterate through combos and collect results
  results <- list()
  for (i in seq_len(nrow(combos))) {
    country <- combos$country[i]
    year <- combos$year[i]
    survey <- combos$survey[i]
    key <- paste(country, year, survey, sep = "_")
    files <- get_file_single(country, year, survey)
    results[[key]] <- files
  }
  
  return(results)
}

# Example usage:
# countries <- c("Kenya", "Nigeria", "Congo")
# years <- c(2008, 2009)
# surveys <- c("BR", "IR")
# file_list <- get_file(countries, years, surveys)