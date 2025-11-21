library(fs)
library(stringr)

# get_file: find DHS Rds files (old behaviour) or GPS shapefiles (new)
get_file <- function(countries,
                     years,
                     surveys = NULL,
                     root = "/Users/matthewnicholson/DHS/DHS_surveys_rds_organized",
                     file_type = c("dhs", "gps"),
                     gps_root = file.path(dirname(root), "GPS files")) {

  file_type <- match.arg(file_type)
  countries <- as.character(countries)
  years <- as.character(years)
  file_paths <- character()

  if (file_type == "dhs") {
    if (is.null(surveys)) stop("`surveys` must be provided when file_type = 'dhs'")
    surveys <- as.character(surveys)
    combos <- expand.grid(country = countries, year = years, survey = surveys, stringsAsFactors = FALSE)

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
    file_paths <- unique(file_paths)
    return(file_paths)
  }

  # file_type == "gps"
  # gps_root default is dirname(root)/"GPS files"; allow override
  if (!dir_exists(gps_root)) {
    warning("gps_root does not exist: ", gps_root)
    return(character())
  }

  # Build year pattern for substring matching; escape special regex chars
  years_esc <- str_replace_all(years, "([\\^$.|()\\[\\]{}*+?\\\\-])", "\\\\\\1")
  year_pattern <- paste(years_esc, collapse = "|")

  for (country in countries) {
    country_dir <- path(gps_root, country)
    if (!dir_exists(country_dir)) next

    # list all shapefiles beneath the country directory (recursive)
    shp_files <- tryCatch(
      dir_ls(country_dir, recurse = TRUE, regexp = "\\.shp$", type = "file"),
      error = function(e) character()
    )
    if (length(shp_files) == 0) next

    # keep shapefiles where path (folder or filename) contains "GE" (case-insensitive)
    # and also contains one of the year strings (substring match)
    keep_idx <- grepl("GE", shp_files, ignore.case = TRUE) &
                grepl(year_pattern, shp_files, ignore.case = TRUE)
    matched <- shp_files[keep_idx]

    # If none matched both, fall back to shapefiles that contain the year anywhere
    if (length(matched) == 0) {
      matched <- shp_files[grepl(year_pattern, shp_files, ignore.case = TRUE)]
    }
    # As final fallback, return any shapefiles under the country folder that contain "GE"
    if (length(matched) == 0) {
      matched <- shp_files[grepl("GE", shp_files, ignore.case = TRUE)]
    }

    if (length(matched) > 0) file_paths <- c(file_paths, matched)
  }

  unique(file_paths)
}

# Example usage:
# get_file(c("Nigeria"), c("1990","2018"), surveys = "HR", root = "/.../DHS_surveys_rds_organized", file_type = "dhs")
# get_file(c("Nigeria","Ghana"), c("2010","2010-2011"), file_type = "gps", root = "/Users/matthewnicholson/DHS/DHS_surveys_rds_organized")