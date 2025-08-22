library(fs)
library(stringr)

get_file <- function(country, year, survey, root = "/Users/matthewnicholson/DHS/DHS_surveys_rds_organized") {
  # Construct the path to the country/year directory
  dir_path <- path(root, country, as.character(year))
  
  # Check if directory exists
  if (!dir_exists(dir_path)) {
    stop("Directory ", dir_path, " does not exist.")
  }
  
  # Find all folders/files in the year directory that contain the survey string
  survey_dirs <- dir_ls(dir_path, type = "directory", regexp = survey)
  survey_files <- character()
  
  # For each survey folder, list Rds files containing the survey code
  for (survey_dir in survey_dirs) {
    files <- dir_ls(survey_dir, regexp = paste0(".*", survey, ".*\\.Rds$"))
    survey_files <- c(survey_files, files)
  }
  
  # If no survey folders, look for files directly in year folder containing survey code
  if (length(survey_files) == 0) {
    survey_files <- dir_ls(dir_path, regexp = paste0(".*", survey, ".*\\.Rds$"))
  }
  
  # Return found file paths (if any)
  if (length(survey_files) == 0) {
    warning("No files found for country=", country, ", year=", year, ", survey=", survey)
    return(NULL)
  }
  
  return(survey_files)
}

# Example usage:
# get_file("Kenya", 2008, "BR")