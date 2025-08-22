library(fs)
library(stringr)

get_file <- function(countries, years, surveys, root = "/Users/matthewnicholson/DHS/DHS_surveys_rds_organized") {
  countries <- as.character(countries)
  years <- as.character(years)
  surveys <- as.character(surveys)
  combos <- expand.grid(country = countries, year = years, survey = surveys, stringsAsFactors = FALSE)
  
  file_paths <- character()
  
  for (i in seq_len(nrow(combos))) {
    country <- combos$country[i]
    year <- combos$year[i]
    survey <- combos$survey[i]
    dir_path <- path(root, country, year)
    if (!dir_exists(dir_path)) next
    survey_dirs <- dir_ls(dir_path, type = "directory", regexp = survey)
    survey_files <- character()
    for (survey_dir in survey_dirs) {
      files <- dir_ls(survey_dir, regexp = paste0(".*", survey, ".*\\.Rds$"))
      survey_files <- c(survey_files, files)
    }
    if (length(survey_files) == 0) {
      survey_files <- dir_ls(dir_path, regexp = paste0(".*", survey, ".*\\.Rds$"))
    }
    if (length(survey_files) > 0) {
      file_paths <- c(file_paths, survey_files)
    }
  }
  # Print file paths in quotes, as R does for character vectors
  # cat(paste0('"', file_paths, '"', collapse = ",\n"))
  # Or just return them:
  return(file_paths)
}

# Example usage:
# countries <- c("Kenya", "Nigeria", "Congo")
# years <- c(2008, 2009)
# surveys <- c("BR", "IR")
# file_paths <- get_file(countries, years, surveys)
# cat(paste0('"', file_paths, '"', collapse = ",\n"))